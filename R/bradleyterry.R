
# ----- Preamble -----

library(tidyverse)
rm(list = ls())

# ----- Data Prep -----

# ATP = Association of Tennis Professionals (Men's)
# WTA = Women's Tennis Association

# identify all years we have data for
atp.singles.years <- c(1968:2022)
wta.singles.years <- c(1920:2022)

league <- "atp"
years  <- atp.singles.years
if (league == "wta") {years <- wta.singles.years}

year <- years[1]
match.data <- read.csv( paste( "../data/tennis_atp_data/", league, "_matches_", as.character(year), ".csv", sep = "") )

# top much data, working on this (!)
#for (year in years[2:length(years)]) {
#  new.data   <- read.csv( paste( "../data/tennis_atp_data/", league, "_matches_", as.character(year), ".csv", sep = "") )
#  match.data <- rbind( match.data, new.data )
#}

dim( match.data )
colnames(match.data)


# ---- Identify all Players -----

all.winners <- match.data[match(unique(match.data$winner_id), match.data$winner_id),] %>% select(winner_id, winner_name)
colnames(all.winners) <- c("player_id", "player_name")

all.losers  <- match.data[match(unique(match.data$loser_id), match.data$loser_id),] %>% select(loser_id, loser_name)
colnames(all.losers) <- c("player_id", "player_name")

all.players <- union(all.winners, all.losers) %>% arrange(player_id)


# ----- Identify all Matches -----

match.data <- match.data %>%
                  select(match_num, winner_id, loser_id) %>%
                  arrange(match_num)


# ----- R Module Code -----

#' Construct a Bradley-Terry Model for use in performing comparisons
#' @param match.data  A data-frame, the observations of previous comparisons
#' @param all.players A data-frame, the players being compared
#' @param player.i.id An integer, id code present in all.players used for drawing comparison
#' @return A coefficient beta.i corresponding to the probability of player.i winning

construct.participant.matrix<- function(match.data, all.players) {
  # set up data matrix predictor (participation in match)
  participant.matrix <- matrix(0, 
                               nrow = nrow(match.data), 
                               ncol = nrow(all.players) )
  
  # fill data matrix with 1s indicating participation
  for (i in 1:nrow(match.data)) {
    # lookup results of match
    winner.id <- match.data[i, 2]
    loser.id  <- match.data[i, 3]
    
    # lookup list indexes corresponding to players
    winner.index <- which(all.players$player_id == winner.id)
    loser.index  <- which(all.players$player_id == loser.id)
    
    # mark participation of each player in matrix
    participant.matrix[i, winner.index] <- 1
    participant.matrix[i, loser.index] <- 1
  }
  return(participant.matrix)
}


construct.wins.indicator <- function(match.data, player.i.id){
  # extract corresponding index
  player.i.index <- which(all.players$player_id == player.i.id)
  
  # set-up vector response (winner of match) and vector of losses for error-checking
  player.i.wins.indicator <- as.numeric(match.data$winner_id == player.i.id)
  
  return(player.i.wins.indicator)
  
}


#' Construct a Bradley-Terry Model for use in performing comparisons
construct.bradley.terry.model <- function(match.data, participant.matrix, player1.wins.indicator) {
  
  # assemble data-frame appropriate for glm
  model.data <- data.frame( cbind(participant.matrix, player.i.wins.indicator) )
  
  # train glm
  model <- glm(player1.wins.indicator ~ ., data = model.data, family = binomial(link = "logit"))
  
  # extract required information
  player.coefficients  <- coef(model)
  
  # return  coefficient vector
  return (player.coefficients)}


#' Calculate the probability of player i winning against player j in a given match
#'
#' @param b.i A number, estimated regression parameter beta.i for player i winning
#' @param b.j A number, estimated regression parameter beta.j for player j winning
#' @return A number, probability of player i winning against player j
#'
comparison <- function(b.i = NULL, b.j = NULL) {
  i.wins.prob <- (exp(b.i) / (exp(b.i) + exp(b.j)))
  return (i.wins.prob)
}


# ----- Example of Model Running -----

# random player ids for testing
player.id <- all.players[sample(1:length(all.players[,1]), 2) , 1]

player.1.id <- player.id[1]
player.2.id <- player.id[2]

participant.matrix <- construnct.participant.matrix(match.data, all.players)
player.1.wins.indicator <- construct.wins.indicator(match.data, player.1.id)

BTmodel1.coef <- construct.bradley.terry.model(match.data, 
                                               participant.matrix, 
                                               player.1.wins.indicator)

beta.1 <- unname(BTmodel1.coef[which(all.players$player_id ==
                                       player.1.id)])/sum(na.omit(BTmodel1.coef)[-1])
beta.2 <- unname(BTmodel1.coef[which(all.players$player_id ==
                                       player.2.id)])/sum(na.omit(BTmodel1.coef)[-1])


prob.1.wins <- comparison(beta.1, beta.2)
prob.2.wins <- comparison(beta.2, beta.1)

message(paste("Player 1:", player.1.id))
message(paste("Player 2:", player.2.id))
message(paste("Probability Player 1 Wins:", prob.1.wins))
message(paste("Probability Player 2 Wins:", prob.2.wins))


