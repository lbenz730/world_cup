### Load Packages
library(dplyr)
library(XML)
library(RCurl)
library(ggplot2)

### Data Cleaning
x <- read.csv("international_soccer_game_data.csv", as.is = T)
n <- nrow(x)
x$date <- as.Date(x$date,"%Y-%m-%d")
x$days_since <- as.numeric(Sys.Date() - x$date)

### Dulplicate the Data Set for H/A
home <- select(x, home_team, away_team, tournament, neutral, home_score, 
               days_since, date) %>%
  mutate(location = "H")
away <- select(x, away_team, home_team, tournament, neutral, away_score, 
               days_since, date) %>%
  mutate(location = "A")
names(home) <- c("team", "opponent", "tournament", "neutral", "goals", 
                 "days_since", "date", "location")
names(away) <- c("team", "opponent", "tournament", "neutral", "goals", 
                 "days_since", "date", "location")
x <- rbind(home, away)
x$location[x$neutral] <- "N"

### Classify Game Types (codes each tournament to certain game type)
# OM <- Other Match
# WC <- World Cup
# WCQ <- World Cup Qualifying
# CC <- Continental Cup
# CCQ <- Continental Cup Qualifying
# FR <- Friendly
# CFC <- Confederations Cup

x$game_type <- "OM"
x$game_type[x$tournament == "FIFA World Cup"] <- "WC"
x$game_type[x$tournament == "FIFA World Cup qualification"] <- "WCQ"
x$game_type[x$tournament == "Friendly"] <- "FR"
x$game_type[x$tournament == "Confederations Cup"] <- "CFC"

x$game_type[x$tournament == "AFC Asian Cup"] <- "CC"
x$game_type[x$tournament == "AFC Challenge Cup"] <- "CC"
x$game_type[x$tournament == "African Cup of Nations"] <- "CC"
x$game_type[x$tournament == "CFU Caribbean Cup"] <- "CC"
x$game_type[x$tournament == "CONCACAF Championship"] <- "CC"
x$game_type[x$tournament == "Gold Cup"] <- "CC"
x$game_type[x$tournament == "Oceania Nations Cup"] <- "CC"
x$game_type[x$tournament == "UAFA Cup"] <- "CC"
x$game_type[x$tournament == "UEFA Euro"] <- "CC"

x$game_type[x$tournament == "AFC Asian Cup qualification"] <- "CCQ"
x$game_type[x$tournament == "AFC Challenge Cup qualification"] <- "CCQ"
x$game_type[x$tournament == "African Cup of Nations qualification"] <- "CCQ"
x$game_type[x$tournament == "CFU Caribbean Cup qualification"] <- "CCQ"
x$game_type[x$tournament == "CONCACAF Championship qualification"] <- "CCQ"
x$game_type[x$tournament == "Gold Cup qualification"] <- "CCQ"
x$game_type[x$tournament == "Oceania Nations Cup qualification"] <- "CCQ"
x$game_type[x$tournament == "UAFA Cup qualification"] <- "CCQ"
x$game_type[x$tournament == "UEFA Euro qualification"] <- "CCQ"

### Match Importance Parameters (based on those used in the FIFA rankings formula)
x$match_weight <- 1
x$match_weight[x$game_type == "WC"] <- 8
x$match_weight[x$game_type == "WCQ" | x$game_type == "CCQ"] <- 3
x$match_weight[x$game_type == "CFC" | x$game_type == "CC"] <- 5



wcyears <- seq(1998, 2018, 4)
disparity <- rep(NA, length(wcyears))

for(i in 1:length(wcyears)) {
  print(paste("Computing Year: ", wcyears[i]))
  ### Model Fitting 
  ### Parameters: Team, Opponent, Match Type, Location, Days Since Previous World Cup
  startdate <- as.Date(paste0(wcyears[i] - 4, "/01/01"))
  enddate <- as.Date(paste0(wcyears[i], "/06/01"))
  
  y <- filter(x, date >= startdate & date <= enddate)
  
  y$days_since <- as.numeric(y$date - startdate)
  y$match_weight <- 
    mutate(y, "match_weight" = match_weight * exp(-days_since/max(days_since))) %>% 
    pull(match_weight)
  glm.futbol <- glm(goals ~ team + opponent + location, 
                    family = "poisson",
                    data = y, 
                    weights = match_weight)
  team_num <- length(unique(y$team))
  rankings <- data.frame("team" = sort(unique(y$team)),
                         "offense" = rep(NA, team_num),
                         "defense" = rep(NA, team_num))
  off_scale_factor <- mean(glm.futbol$coefficients[2:team_num], na.rm = T)
  def_scale_factor <- mean(glm.futbol$coefficients[(team_num + 1):(2*team_num - 1)], na.rm = T)
  rankings$offense <- c(0, glm.futbol$coefficients[2:team_num]) - off_scale_factor
  rankings$defense <- c(0, glm.futbol$coefficients[(team_num + 1):(2*team_num - 1)]) - def_scale_factor
  rankings$net_rating <- rankings$offense - rankings$defense
  
  rankings <- rankings[order(rankings$net_rating, decreasing = T),]
  rankings$rank <- 1:nrow(rankings)
  
  url <- paste0("https://en.wikipedia.org/wiki/", wcyears[i], "_FIFA_World_Cup_knockout_stage")
  knockout <- as.data.frame(readHTMLTable(getURL(url))[[1]])
  knockout$Winners <- gsub("South Korea", "Korea Republic", as.character(knockout$Winners))
  knockout$`Runners-up` <- gsub("South Korea", "Korea Republic", as.character(knockout$`Runners-up`))
  
  if(wcyears[i] == 2002) {
    top <- c(knockout$Winners[c(2,4,5,7)], knockout$`Runners-up`[c(2,4,5,7)])
    bot <- c(knockout$Winners[c(1,3,6,8)], knockout$`Runners-up`[c(1,3,6,8)])
  }else{
    top <- c(knockout$Winners[c(1,3,5,7)], knockout$`Runners-up`[c(2,4,6,8)])
    bot <- c(knockout$Winners[c(2,4,6,8)], knockout$`Runners-up`[c(1,3,5,7)])
  }
  disparity[i] <- mean(rankings$net_rating[rankings$team %in% top], na.rm = T) - 
    mean(rankings$net_rating[rankings$team %in% bot], na.rm = T)
}

disparity <- abs(disparity)

df <- data.frame("Year" = as.factor(wcyears),
                 "Disparity" = disparity)
ggplot(df, aes(x = Year, y = Disparity)) + 
  geom_bar(stat = "identity", fill = "springgreen") + 
  labs(title = "Knockout Bracket Talent Disparity Based on Pre-Tournament Rankings") + 
  theme(plot.title = element_text(size = 16, hjust = 0.5), axis.title = element_text(size = 14))
       