
# ----- Preamble -----

rm(list = ls())
library(tidyverse)

devtools::install_github("https://github.com/RaspberryEmma/TennisMatchPredictionTool")
library(TennisMatchPredictionTool)

# ----- Data Prep -----

# ATP = Association of Tennis Professionals (Men's)
# WTA = Women's Tennis Association

# identify all years we have data for
atp.singles.years <- c(1968:2022)
wta.singles.years <- c(1920:2022)

league <- "atp"
years  <- atp.singles.years
if (league == "wta") {years <- wta.singles.years}

year <- years[50]
match.data <- read.csv( paste( "../data/tennis_", league, "_data/", league, "_matches_", as.character(year), ".csv", sep = "") )

dim( match.data )
colnames(match.data)


# ---- Identify all Players -----

all.winners <- match.data[match(unique(match.data$winner_id), match.data$winner_id),] %>% select(winner_id, winner_name, winner_age)
colnames(all.winners) <- c("player_id", "player_name", "player_age")

all.losers  <- match.data[match(unique(match.data$loser_id), match.data$loser_id),] %>% select(loser_id, loser_name, loser_age)
colnames(all.losers) <- c("player_id", "player_name", "player_age")

# we only consider players who have participated in at least one match
# additionally, since players age across the tournament, we default to the highest age
# we use player id numbers as unique identifiers
all.players <- union(all.winners, all.losers)
all.players <- all.players[order(all.players$player_age, decreasing = TRUE), ]
all.players <- all.players[!duplicated(all.players$player_id), ]
all.players <- na.omit(all.players) %>% arrange(player_id)


# ----- Identify all Matches -----

match.data <- match.data %>%
  select(match_num, winner_id, loser_id, winner_age, loser_age) %>%
  arrange(match_num)

match.data <- na.omit(match.data)



# ----- Running the Model -----

# random player ids for testing
player.id <- all.players[sample(1:length(all.players[,1]), 2) , 1]

player.1.id <- player.id[1]
player.2.id <- player.id[2]

participant.matrix      <- construct.participant.matrix(match.data, all.players)
player.1.wins.indicator <- construct.wins.indicator(match.data, player.1.id)
player.2.wins.indicator <- construct.wins.indicator(match.data, player.2.id)

BTmodel1.coef <- construct.bradley.terry.model(match.data              = match.data,
                                               participant.matrix      = participant.matrix,
                                               player.i.wins.indicator = player.1.wins.indicator)

BTmodel2.coef <- construct.bradley.terry.model(match.data              = match.data,
                                               participant.matrix      = participant.matrix,
                                               player.i.wins.indicator = player.2.wins.indicator)

beta.1 <- (unname(BTmodel1.coef[which(all.players$player_id ==
                                        player.1.id)]) ) / sum(na.omit(BTmodel1.coef)[-1])
beta.2 <- (unname(BTmodel2.coef[which(all.players$player_id ==
                                        player.2.id)]) ) / sum(na.omit(BTmodel2.coef)[-1])

prob.1.wins <- comparison(beta.1, beta.2)
prob.2.wins <- comparison(beta.2, beta.1)

message(paste("Player 1: ID", player.1.id, "who has won",
              sum(player.1.wins.indicator == 1), "matches"))
message(paste("Player 2: ID", player.2.id, "who has won",
              sum(player.2.wins.indicator == 1), "matches"))

message(paste("Player 1 Coefficient:", beta.1))
message(paste("Player 2 Coefficient:", beta.2))

message(paste("Probability Player 1 Wins:", prob.1.wins))
message(paste("Probability Player 2 Wins:", prob.2.wins))


