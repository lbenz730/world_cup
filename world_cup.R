### world_cup.R
### By: Luke Benz (@recspecs730) and Daniel Tokarz (@_TheRealDanT)
### Yale Undergraduate Sports Analytics Group
### June 2018

### Load Packages
library(dplyr)
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

### Inverts Perspective of Data Frame from Team to Opponent
invert <- function(data, score = F) {
  data <- mutate(data, tmp = team, team = opponent, opponent = tmp)
  data$tmp[data$location == "H"] <- "A"
  data$tmp[data$location == "A"] <- "H"
  data$tmp[data$location == "N"] <- "N"
  data$location <- data$tmp
  if(score) {
    data$tmp <- data$team_score
    data$team_score <- data$opponent_score
    data$opponent_score <- data$tmp
  }
  return(select(data,-tmp))
}

### Obtain W, L, T probabilities
match_probs <- function(lambda_1, lambda_2) {
  max_goals <- 10
  score_matrix <- dpois(0:max_goals, lambda_1) %o% dpois(0:max_goals, lambda_2)
  tie_prob <- sum(diag(score_matrix))
  win_prob <- sum(score_matrix[lower.tri(score_matrix)])
  loss_prob <- sum(score_matrix[upper.tri(score_matrix)])
  return(c(win_prob, tie_prob, loss_prob))
}

### Model Fitting 
### Parameters: Team, Opponent, Match Type, Location, Days Since Previous World Cup
y <- filter(x, date >= "2014/01/01")
fixtures <- read.csv("fixtures.csv", as.is = T)

### Bind Completed Fixtures with existing Data Set
y <- rbind(y, fixtures %>% mutate("tournament" = "World Cup", "neutral" = location == "N", 
                                  "goals" = team_score,
                                  "date" = as.Date(date, "%m/%d/%y"),
                                  "days_since" = as.numeric(Sys.Date() - date), 
                                  "game_type" = "WC",
                                  "match_weight"= 6) %>% 
             select(team, opponent, tournament, neutral, goals,days_since, date, location, 
                    game_type, match_weight) %>%
             filter(days_since >= 0))

y <- rbind(y, invert(fixtures, T) %>% mutate("tournament" = "World Cup", "neutral" = location == "N", 
                                             "goals" = team_score,
                                             "date" = as.Date(date, "%m/%d/%y"),
                                             "days_since" = as.numeric(Sys.Date() - date), 
                                             "game_type" = "WC",
                                             "match_weight"= 6) %>% 
             select(team, opponent, tournament, neutral, goals,days_since, date, location, 
                    game_type, match_weight) %>%
             filter(days_since >= 0))

y$match_weight <- 
  mutate(y, "match_weight" = match_weight * exp(-days_since/max(days_since))) %>% 
  pull(match_weight)
glm.futbol <- glm(goals ~ team + opponent + location, 
                  family = "poisson",
                  data = y, 
                  weights = match_weight)
team_num <- (length(glm.futbol$coefficients) - 1)/ 2
rankings <- data.frame("team" = sort(unique(y$team)),
                       "offense" = rep(NA, team_num),
                       "defense" = rep(NA, team_num))
off_scale_factor <- mean(glm.futbol$coefficients[2:team_num])
def_scale_factor <- mean(glm.futbol$coefficients[(team_num + 1):(2*team_num - 1)])
rankings$offense <- c(0, glm.futbol$coefficients[2:team_num]) - off_scale_factor
rankings$defense <- c(0, glm.futbol$coefficients[(team_num + 1):(2*team_num - 1)]) - def_scale_factor
rankings$net_rating <- rankings$offense - rankings$defense


rankings <- rankings[order(rankings$net_rating, decreasing = T),]
rankings$rank <- 1:nrow(rankings)
write.csv(rankings, "rankings.csv", row.names = F)

############ Make Predictions ############
fixtures <- mutate(fixtures, "win" = NA, "tie" = NA, "loss" = NA)
index <- !is.na(fixtures$goal_diff)
fixtures[index & fixtures$goal_diff > 0, c("win", "tie", "loss")] <- rep(c(1,0,0), rep(sum(index & fixtures$goal_diff > 0), 3))
fixtures[index & fixtures$goal_diff == 0, c("win", "tie", "loss")] <-rep(c(0,1,0), rep(sum(index & fixtures$goal_diff == 0), 3))
fixtures[index & fixtures$goal_diff < 0, c("win", "tie", "loss")] <- rep(c(0,0,1), rep(sum(index & fixtures$goal_diff < 0), 3))

fixtures$team_score[is.na(fixtures$team_score)]<- 
  predict(glm.futbol, newdata = fixtures[is.na(fixtures$team_score),], type = "response")
fixtures$opponent_score[is.na(fixtures$opponent_score)]<- 
  predict(glm.futbol, newdata = invert(fixtures[is.na(fixtures$opponent_score),]), type = "response")
fixtures$goal_diff <- fixtures$team_score - fixtures$opponent_score

for(i in 1:nrow(fixtures)) {
  if(is.na(fixtures$win[i])) {
    fixtures[i, c("win", "tie", "loss")] <- match_probs(lambda_1 = fixtures$team_score[i],
                                                        lambda_2 = fixtures$opponent_score[i])
  }
}


######## Monte Carlo Simulations
sim_group <- function(group_name) {
  games <- filter(fixtures, group == group_name) %>% mutate(
    "team_goals" = NA, "opp_goals" = NA, "team_gd" = NA, "opp_gd" = NA,
    "team_points" = 0, "opp_points" = 0)
  for(i in 1:nrow(games)) {
    if(games$win[i] == 1 | games$loss[i] == 1 | games$tie[i] == 1) {
      games$team_goals[i] <- games$team_score[i]
      games$opp_goals[i] <- games$opponent_score[i]
      games$team_gd[i] <- games$goal_diff[i]
      games$opp_gd[i] <- -games$goal_diff[i]
      games$team_points[i] <- 3*games$win[i] + games$tie[i]
      games$opp_points[i] <- 3*games$loss[i] + games$tie[i]
    }
    else{
      lambda_1 <- games$team_score[i]
      lambda_2 <- games$opponent_score[i]
      games$team_goals[i] <- rpois(1, lambda_1)
      games$opp_goals[i] <- rpois(1, lambda_2)
      games$team_gd[i] <- games$team_goals[i] - games$opp_goals[i]
      games$opp_gd[i] <- games$opp_goals[i] - games$team_goals[i]
      if(games$team_gd[i] > 0) {
        games$team_points[i] <- 3
      }
      if(games$opp_gd[i] > 0) {
        games$opp_points[i] <- 3
      }
      if(games$team_gd[i] == 0) {
        games$team_points[i] <- 1
        games$opp_points[i] <- 1
      }
    }
  }
  ladder <- data.frame("country" = unique(c(games$team, games$opponent)),
                       "pts" = rep(NA, 4),
                       "goals_forced" = rep(NA, 4),
                       "goals_against" = rep(NA, 4),
                       "goal_diff" = rep(NA, 4),
                       stringsAsFactors = F)
  for(i in 1:4) {
    ladder$pts[i] <- sum(games$team_points[games$team == ladder$country[i]]) + 
      sum(games$opp_points[games$opponent == ladder$country[i]])
    ladder$goal_diff[i] <- sum(games$team_gd[games$team == ladder$country[i]]) + 
      sum(games$opp_gd[games$opponent == ladder$country[i]])
    ladder$goals_forced[i] <- sum(games$team_goals[games$team == ladder$country[i]]) + 
      sum(games$opp_goals[games$opponent == ladder$country[i]])
    ladder$goals_against[i] <- sum(games$opp_goals[games$team == ladder$country[i]]) + 
      sum(games$team_goals[games$opponent == ladder$country[i]])
  }
  ladder <- ladder[order(ladder$pts, ladder$goal_diff, 
                         ladder$goals_forced, -1 * ladder$goals_against, decreasing = T),]
  return(ladder)
}

### Real Sims Start Here
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
    index <- apply(as.data.frame(sim_ladder$country), 1, grep, wc_sims$country)
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
                           "team_goals" = rep(NA, teams_left/2),
                           "opp_goals" = rep(NA, teams_left/2),
                           "winner" = rep(NA, teams_left/2),
                           "location" = rep("N", teams_left/2),
                           stringsAsFactors = F)
    
    knockout$location[knockout$team == "Russia"] <- "H"
    knockout$location[knockout$opponent == "Russia"] <- "A"
    
    knockout$team_goals <- 
      predict(glm.futbol, newdata = knockout, type = "response")
    knockout$opp_goals <- 
      predict(glm.futbol, newdata = invert(knockout), type = "response")
    
    
    winners <- rep(NA, teams_left/2)
    for(i in 1:nrow(knockout)) {
      team_goals <- rpois(1, knockout$team_goals[i])
      opp_goals <- rpois(1, knockout$opp_goals[i])
      if(team_goals > opp_goals) {
        knockout$winner[i] <- knockout$team[i]
      }
      else if(team_goals < opp_goals) {
        knockout$winner[i] <- knockout$opponent[i]
      }
      else { 
        ## Penalty Shoutout 50-50
        knockout$winner[i] <- sample(c(knockout$team[i], knockout$opponent[i]), 1)
      }
    }
    
    if(teams_left > 2) {
      winners <- knockout$winner[c(seq(1, teams_left/2, 2), seq(2, teams_left/2, 2))]
    }else{
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
wc_sims[, 4:10] <- round(wc_sims[, 4:10], 4)
wc_sims <- wc_sims[order(wc_sims$champ, decreasing = T),]
write.csv(wc_sims, "wc_sims.csv", row.names = F)



### Goal Plot Function
goal_plot <- function(team1, team2, location, col1, col2) {
  lambda1 <- predict(glm.futbol, newdata = data.frame("team" = team1,
                                                      "opponent" = team2,
                                                      "location" = location,
                                                      stringsAsFactors = F), 
                     type = "response")
  lambda2 <- predict(glm.futbol, newdata = invert(data.frame("team" = team1,
                                                             "opponent" = team2,
                                                             "location" = location,
                                                             stringsAsFactors = F)), 
                     type = "response")
  max_goals <- 10
  score_matrix <- dpois(0:max_goals, lambda1) %o% dpois(0:max_goals, lambda2)
  tie_prob <- sum(diag(score_matrix))
  win_prob <- sum(score_matrix[lower.tri(score_matrix)])
  loss_prob <- sum(score_matrix[upper.tri(score_matrix)])
  
  
  z <- data.frame("Team" = rep(c(team1, team2), rep(4,2)),
                  "Goals" = rep(c("0", "1", "2", "3+"), 2),
                  "Probability" = c(dpois(0:2, lambda1), sum(dpois(3:10, lambda1)),
                                    dpois(0:2, lambda2), sum(dpois(3:10, lambda2))))
  
  vec <- c(team1, team2)
  vec <- sort(vec)
  if(vec[1] == team2) {
    tmp <- col1
    col1 <- col2
    col2 <- tmp
  }
  ggplot(z, aes(x = Goals, y = Probability, fill = Team)) + 
    geom_bar(stat = "identity", position='dodge', colour  = "black") + 
    labs(title = paste(team1, "vs.", team2, "Goal Distributions")) + 
    scale_fill_manual(values=c(col1, col2)) +
    annotate("text", x = 3.4, y = 0.4, label = paste(team1, "Expected Goals:", round(lambda1, 2))) +
    annotate("text", x = 3.4, y = 0.37, label = paste(team1, "Win Probability", round(win_prob, 2))) + 
    annotate("text", x = 3.4, y = 0.34, label = paste(team2, "Expected Goals:", round(lambda2, 2))) + 
    annotate("text", x = 3.4, y = 0.31, label = paste(team2, "Win Probability", round(loss_prob, 2))) + 
    annotate("text", x = 3.4, y = 0.28, label = paste("Tie Probability", round(tie_prob, 2))) + 
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          axis.title = element_text(size = 14)) 
  
}

goal_jgp <- function(team1, team2, location) {
  lambda1 <- predict(glm.futbol, newdata = data.frame("team" = team1,
                                                      "opponent" = team2,
                                                      "location" = location,
                                                      stringsAsFactors = F), 
                     type = "response")
  lambda2 <- predict(glm.futbol, newdata = invert(data.frame("team" = team1,
                                                             "opponent" = team2,
                                                             "location" = location,
                                                             stringsAsFactors = F)), 
                     type = "response")
  t1gprob <- dpois(0:4, lambda1)
  t2gprob <- dpois(0:4, lambda2)
  df <- expand.grid(t1gprob, t2gprob)
  df2 <- expand.grid(0:4, 0:4)
  names(df) <- c("t1gprob", "t2gprob")
  names(df2) <- c("t1g", "t2g")
  df <- cbind(df, df2)
  ggplot(df, aes(x = t1g, y = t2g, fill = t1gprob * t2gprob)) + 
    geom_raster(alpha  = 0.9) + 
    scale_fill_gradient2(low = "navy", mid = "white", midpoint = 0.05,  high = "red") + 
    labs(x = paste(team1, "Goals"), y = paste(team2, "Goals"), fill = "Joint\nProbability",
         title = paste(team1, "vs.", team2, "Joint Goal Probability Distribution")) + 
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          axis.title = element_text(size = 14)) + 
    scale_x_continuous(waiver(), breaks = 0:4, labels = as.character(0:4)) + 
    scale_y_continuous(waiver(), breaks = 0:4, labels = as.character(0:4))
}

grid.arrange(goal_plot("Germany", "Sweden", "N", "black", "yellow1"),
             goal_jgp("Germany", "Sweden", "N"))
grid.arrange(goal_plot("Mexico", "Korea Republic", "N", "forestgreen", "slateblue"),
             goal_jgp("Korea Republic", "Mexico", "N"))
grid.arrange(goal_plot("Belgium", "Tunisia", "N", "red2", "red4"),
             goal_jgp("Belgium", "Tunisia", "N"))
