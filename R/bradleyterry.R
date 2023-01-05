
# ----- Preamble -----

library(tidyverse)

rm(list = ls())
#setwd("...")


# ----- Data Prep -----

# ATP = Association of Tennis Professionals (Men's)
# WTA = Women's Tennis Association

year <- 1968

atp.data <- read.csv(paste( "tennis_atp_data/atp_matches_", as.character(year), ".csv", sep = "") )
wta.data <- read.csv(paste( "tennis_wta_data/wta_matches_", as.character(year), ".csv", sep = "") )

colnames(atp.data)


# ---- Identify all Players -----

# ATP Players

atp.all.winners <- atp.data[match(unique(atp.data$winner_id), atp.data$winner_id),] %>% select(winner_id, winner_name)
colnames(atp.all.winners) <- c("player_id", "player_name")

atp.all.losers  <- atp.data[match(unique(atp.data$loser_id), atp.data$loser_id),] %>% select(loser_id, loser_name)
colnames(atp.all.losers) <- c("player_id", "player_name")

atp.all.players <- union(atp.all.winners, atp.all.losers) %>% arrange(player_id)


# WTA Players

wta.all.winners <- wta.data[match(unique(wta.data$winner_id), wta.data$winner_id),] %>% select(winner_id, winner_name)
colnames(wta.all.winners) <- c("player_id", "player_name")

wta.all.losers  <- wta.data[match(unique(wta.data$loser_id), wta.data$loser_id),] %>% select(loser_id, loser_name)
colnames(wta.all.losers) <- c("player_id", "player_name")

wta.all.players <- union(wta.all.winners, wta.all.losers) %>% arrange(player_id)


# ----- Identify all Matches -----

atp.match.data <- atp.data %>%
                  select(match_num, winner_id, loser_id) %>%
                  arrange(match_num)

wta.match.data <- wta.data %>%
                  select(match_num, winner_id, loser_id) %>%
                  arrange(match_num)



# ----- Bradley-Terry Model Code -----

player.i.id        <- 202866
match.data         <- atp.match.data %>% filter((winner_id == player.i.id) | (loser_id == player.i.id))
player.i.indicator <- as.numeric(match.data$winner_id == player.i.id)

#model <- glm(winner_id ~ ., data = atp.match.data, family = binomial(link = "logit"))



#' Construct data matrix X for use in regression model estimation
#'
#' @param players A vector of player id codes
#' @return A design matrix X for use in the glm modelling of matches
#'
construct.design.matrix <- function(teams, match.data) {
  X <- NULL
  return (X)
}

#' Calculate the probability of player i winning against player j in a given match
#'
#' @param p.i A number, estimated regression parameter for player i, 0 <= p.i <= 1
#' @param p.j A number, estimated regression parameter for player j, 0 <= p.j <= 1
#' @return A number, probability of player i winning against player j
#' @examples
#' comparison(0.7, 0.4)
#' comparison(0.5, 0.5)
#'
comparison <- function(p.i = NULL, p.j = NULL) {
  win.probability <- (p.i / (p.i + p.j))
  return (win.probability)
}



