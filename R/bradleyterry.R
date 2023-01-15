
# ----- Dependencies -----

library(tidyverse)



# ----- R Module Code -----

#' Construct a Bradley-Terry Model for use in performing comparisons
#'
#' @param match.data  A data-frame, the observations of previous comparisons
#' @param all.players A data-frame, the players being compared
#' @param player.i.id An integer, id code present in all.players used for drawing comparison
#' @return A coefficient beta.i corresponding to the probability of player.i winning
#'
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



#' Construct a binary vector indicating wins across each match by a given player
#'
#' @param match.data
#' @param player.i.id
#' @return A binary vector, of length equal to the number of matches
#'
construct.wins.indicator <- function(match.data, player.i.id){
  # extract corresponding index
  player.i.index <- which(all.players$player_id == player.i.id)

  # set-up vector response (winner of match) and vector of losses for error-checking
  player.i.wins.indicator <- as.numeric(match.data$winner_id == player.i.id)

  return(player.i.wins.indicator)

}



#' Construct a numeric vector of ages of each player
#'
#' @param all.players
#' @return A numeric vector, of length equal to the number of matches
#'
construct.age.vector <- function(all.players) {
  player.age.vector <- as.numeric(all.players$player_age)
  return(player.age.vector)
}



#' onstruct a numeric vector of heights of each player
#'
#' @param all.players
#' @return A numeric vector, of length equal to the number of matches
#'
construct.height.vector <- function(all.players) {
  player.height.vector <- as.numeric(all.players$player_height)
  return(player.height.vector)
}



#' Construct a Bradley-Terry Model for use in performing comparisons
#'
#' @param match.data
#' @param participant.matrix
#' @return A vector of coefficients
#'
construct.bradley.terry.model <- function(match.data, participant.matrix, age.vector, height.vector, player.i.wins.indicator) {

  # assemble data-frame appropriate for glm
  model.data <- data.frame( cbind(participant.matrix, player.i.wins.indicator) )

  # train glm
  model <- glm(player.i.wins.indicator ~ ., data = model.data, family = binomial(link = "logit"))

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





