### world_cup.R
### By: Luke Benz (@recspecs730) and Daniel Tokarz (@_TheRealDanT)
### Yale Undergraduate Sports Analytics Group
### June 2018

### Load Packages
library(dplyr)
library(ggplot2)
library(nnet)

### Data Cleaning
x <- read.csv("international_soccer_game_data.csv", as.is = T)
n <- nrow(x)
x$date[1:126] <- as.Date(x$date[1:126],"%Y-%m-%d")
x$date[127:n] <- as.Date(x$date[127:n],"%m/%d/%Y")
x$date <- as.Date(as.numeric(x$date), origin = "1970-01-01")
x$days_since <- as.numeric(Sys.Date() - x$date)

### Get Goal Differentials
x$goal_diff <- x$home_score - x$away_score

### Dulplicate the Data Set for H/A
home <- select(x, home_team, away_team, tournament, neutral, goal_diff, 
               days_since, date) %>%
  mutate(location = "H")
away <- select(x, away_team, home_team, tournament, neutral, goal_diff, 
               days_since, date) %>%
  mutate(location = "A", goal_diff = -1 * goal_diff)
names(home) <- c("team", "opponent", "tournament", "neutral", "goal_diff", 
                 "days_since", "date", "location")
names(away) <- c("team", "opponent", "tournament", "neutral", "goal_diff", 
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

### Match Importance Parameters (the same as those used in the FIFA rankings formula)
x$match_weight <- 1
x$match_weight[x$game_type == "WC"] <- 4
x$match_weight[x$game_type == "WCQ" | x$game_type == "CCQ"] <- 2.5
x$match_weight[x$game_type == "CFC" | x$game_type == "CC"] <- 3

### Model Fitting 
### Parameters: Team, Opponent, Match Type, Location, Days Since Previous World Cup
y <- filter(x, date >= "2014/01/01")
lm.futbol <- lm(goal_diff ~ team + opponent + location, 
                data = y, weights = match_weight * exp(-days_since/max(days_since)))
team_num <- (length(lm.futbol$coefficients) - 1)/ 2
rankings <- data.frame("team" = sort(unique(y$team)),
                       "yusag_coeff" = rep(NA, team_num))
scale_factor <- mean(lm.futbol$coefficients[2:team_num])
rankings$yusag_coeff <- c(0, lm.futbol$coefficients[2:team_num]) - scale_factor
rankings <- rankings[order(rankings$yusag_coeff, decreasing = T),]
rankings$rank <- 1:nrow(rankings)
write.csv(rankings, "rankings.csv", row.names = F)

### Win Prob Model
y$pred_gd <- lm.futbol$fitted.values
y$outcome <- "tie"
y$outcome[y$goal_diff > 0] <- "win"
y$outcome[y$goal_diff < 0] <- "loss"
wp_model <- multinom(outcome ~ pred_gd, data = y)



######## Monte Carlo Simulations
fixtures <- read.csv("fixtures.csv", as.is = T)
fixtures$pred_gd <- predict(lm.futbol, newdata = fixtures)
fixtures <- cbind(fixtures, predict(wp_model, newdata = fixtures, "probs"))
index <- !is.na(fixtures$goal_diff)
fixtures[index & fixtures$goal_diff > 0, c("loss", "tie", "win")] <- c(0,0,1)
fixtures[index & fixtures$goal_diff == 0, c("loss", "tie", "win")] <- c(0,1,0)
fixtures[index & fixtures$goal_diff < 0, c("loss", "tie", "win")] <- c(1,0,0)

sim_group <- function(group_name) {
  games <- filter(fixtures, group == group_name) 
  rand <- runif(6)
  for(i in 1:nrow(games)) {
    if(rand[i] <= games$win[i]) {
      games$team_pts[i] <- 3
      games$opp_pts[i] <- 0
    }
    else if(rand[i] <= games$win[i] + games$tie[i]) {
      games$team_pts[i] <- 1
      games$opp_pts[i] <- 1
    }
    else{
      games$team_pts[i] <- 0
      games$opp_pts[i] <- 3 
    }
  }
  ladder <- data.frame("country" = unique(c(games$team, games$opponent)),
                       "pts" = rep(NA, 4),
                       stringsAsFactors = F)
  for(i in 1:4) {
    ladder$pts[i] <- sum(games$team_pts[games$team == ladder$country[i]]) + 
      sum(games$opp_pts[games$opponent == ladder$country[i]])
  }
  ladder <- ladder[order(ladder$pts, decreasing = T),]
  
  ### Break Ties
  if(ladder$pts[1] == ladder$pts[2]) {
    ladder <- ladder[c(sample(1:2),3,4),]
  }
  else if(ladder$pts[2] == ladder$pts[3]) {
    ladder <- ladder[c(1,sample(2:3),4),]
  }
  else if(ladder$pts[1] == ladder$pts[3]) {
    ladder <- ladder[c(sample(1:3),4),]
  }
  else if(ladder$pts[2] == ladder$pts[4]) {
    ladder <- ladder[c(1,sample(2:4)),]
  }
  else if(ladder$pts[1] == ladder$pts[4]) {
    ladder <- ladder[c(sample(1:4)),]
  }
  return(ladder)
}

wc_sims <- data.frame("country" = unique(c(fixtures$team, fixtures$opponent)),
                      "group" = c(rep(c("A", "B", "C", "D", "E", "F", "G", "H"), rep(3, 8)),
                                  c("A", "B", "C", "D", "E", "F", "G", "H")),
                      "expected_pts" = rep(0, 32),
                      "first_in_group" = rep(0, 32),
                      "second_in_group" = rep(0, 32),
                      "r16" = rep(0, 32),
                      "qtrs" = rep(0, 32),
                      "semis" = rep(0, 32),
                      "finals" = rep(0, 32),
                      "champ" = rep(0, 32),
                      stringsAsFactors = F)
groups <- c("A", "B", "C", "D", "E", "F", "G", "H")
winners <- rep(NA, 8)
runners_up <- rep(NA, 8)

nsims <- 10000

for(k in 1:nsims) {
  if(k %% 100 == 0) {
    print(paste("Sim:", k))
  }
  ### Group Stage
  for(i in 1:8) {
    sim_ladder <- sim_group(groups[i])
    index <- wc_sims$country %in% sim_ladder$country
    wc_sims$expected_pts[index] <- wc_sims$expected_pts[index] + sim_ladder$pts/nsims
    index_1 <- wc_sims$country == sim_ladder$country[1]
    index_2 <- wc_sims$country == sim_ladder$country[2]
    wc_sims$first_in_group[index_1] <- wc_sims$first_in_group[index_1] + 1/nsims
    wc_sims$second_in_group[index_2] <- wc_sims$second_in_group[index_2] + 1/nsims
    wc_sims$r16[index_1 | index_2] <- wc_sims$r16[index_1 | index_2]+ 1/nsims
    winners[i] <- sim_ladder$country[1]
    runners_up[i] <- sim_ladder$country[2]
  }
  
  ### Knock-Out Stage
  teams_left <- 16
  ko_round <- 1
  winners <- c(winners[c(1,3,5,7,2,4,6,8)], runners_up[c(2,4,6,8,1,3,5,7)])
  
  while(teams_left > 1) {
    knockout <- data.frame("team" = winners[1:(teams_left/2)],
                           "opponent" = winners[(1 + teams_left/2):teams_left],
                           "winner" = rep(NA, teams_left/2),
                           "location" = rep("N", teams_left/2),
                           stringsAsFactors = F)
    
    knockout$location[knockout$team == "Russia"] <- "H"
    knockout$location[knockout$opponent == "Russia"] <- "A"
    
    knockout$pred_gd <- predict(lm.futbol, newdata = knockout)
    if(ko_round < 4) {
      knockout <- cbind(knockout, predict(wp_model, newdata = knockout, "probs"))
    }
    else {
      preds <-  predict(wp_model, newdata = knockout, "probs")
      knockout$loss <- preds[1]
      knockout$tie <- preds[2]
      knockout$win <- preds[3]
    }
    
    rand <- runif(nrow(knockout))
    for(i in 1:nrow(knockout)) {
      if(rand[i] <= knockout$win[i]) {
        knockout$winner[i] <- knockout$team[i]
      }
      else if(rand[i] <= knockout$win[i] + knockout$tie[i]) {
        knockout$winner[i] <- sample(c(knockout$team[i], knockout$opponent[i]), 1)
      }
      else{
        knockout$winner[i] <- knockout$opponent[i]
      }
    }
    
    if(teams_left > 2) {
      winners <- knockout$winner[c(seq(1, teams_left/2, 2), seq(2, teams_left/2, 2))]
    }
    else{
      winners <- knockout$winner
    }
    index <- wc_sims$country %in% knockout$winner
    teams_left <- teams_left/2
    if(teams_left >= 1) {
      wc_sims[index, 6 + ko_round] <- wc_sims[index, 6 + ko_round] + 1/nsims
    }
    ko_round <- ko_round + 1
  }
}
wc_sims$expected_pts <- round(wc_sims$expected_pts, 2)
wc_sims[, 4:10] <- round(wc_sims[, 4:10], 3)
write.csv(wc_sims, "wc_sims.csv", row.names = F)
