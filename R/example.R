
# ----- Preamble -----

rm(list = ls())
library(tidyverse)



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


# ----- Running the Model -----

# random player ids for testing
player.id <- all.players[sample(1:length(all.players[,1]), 2) , 1]

player.1.id <- player.id[1]
player.2.id <- player.id[2]

participant.matrix      <- construct.participant.matrix(match.data, all.players)
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


