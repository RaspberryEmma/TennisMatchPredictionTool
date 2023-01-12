
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
#'
construct.bradley.terry.model <- function(match.data, all.players, player.i.id) {
  # extract corresponding index
  player.i.index <- which(all.players$player_id == player.i.id)

  # set-up vector response (winner of match) and vector of losses for error-checking
  player.i.wins.indicator <- as.numeric(match.data$winner_id == player.i.id)
  player.i.loss.indicator <- as.numeric(match.data$loser_id == player.i.id)

  # set up data matrix predictor (participation in match)
  participant.matrix      <- matrix(0, nrow = nrow(match.data), ncol   = nrow(all.players) )

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

  # sanity checks
  # for testing purposes only
  dim(match.data)  # input well conditioned
  dim(all.players) # input well conditioned

  length(player.i.wins.indicator) # = num matches
  sum(player.i.wins.indicator)    # = num matches player has won, may be zero

  length(player.i.loss.indicator) # = num matches
  sum(player.i.loss.indicator)    # = num matches player has lost, may be zero

  dim(participant.matrix) # = (num matches x num players)
  sum(participant.matrix) # = num matches x 2
  sum(participant.matrix[ , player.i.index ]) # = num wins + num losses


  # assemble data-frame appropriate for glm
  model.data <- data.frame( cbind(participant.matrix, player.i.wins.indicator) )

  # train glm
  model <- glm(player.i.wins.indicator ~ ., data = model.data, family = binomial(link = "logit"))

  # extract required information
  player.coefficients  <- coef(model)
  beta.i <- unname( player.coefficients[player.i.index] )

  # return corresponding coefficient
  return (beta.i)
}


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
player.1.id <- all.players[sample(1:length(all.players[,1]), 1) , 1]
player.2.id <- all.players[sample(1:length(all.players[,1]), 1) , 1]
while (player.1.id == player.2.id) { player.2.id <- all.players[sample(1:length(all.players[,1]), 1) , 1] }

beta.1 <- construct.bradley.terry.model(match.data  = match.data,
                                        all.players = all.players,
                                        player.i.id = player.1.id)

beta.2 <- construct.bradley.terry.model(match.data  = match.data,
                                        all.players = all.players,
                                        player.i.id = player.2.id)

prob.1.wins <- comparison(beta.1, beta.2)
prob.2.wins <- comparison(beta.2, beta.1)

message(paste("Player 1:", player.1.id))
message(paste("Player 2:", player.2.id))
message(paste("Probability Player 1 Wins:", prob.1.wins))
message(paste("Probability Player 2 Wins:", prob.2.wins))



