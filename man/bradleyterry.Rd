\name{bradleyterry}
\alias{bradleyterry}
\title{Bradley Terry Model Implementation}
\usage{
construct.bradley.terry.model(match.data, all.players, player.i.id)
}
\description{
Generates a parameter estimate beta.i corresponding to the likelihood of a given player i winning a match.
}
\examples{
beta.1 <- construct.bradley.terry.model(match.data  = match.data,
                                        all.players = all.players,
                                        player.i.id = player.1.id)

beta.2 <- construct.bradley.terry.model(match.data  = match.data,
                                        all.players = all.players,
                                        player.i.id = player.2.id)

comparison(beta.1, beta.2)
}
