\name{construct.bradley.terry.model}
\alias{construct.bradley.terry.model}
\title{Bradley Terry Model Implementation}
\usage{
construct.bradley.terry.model(match.data, all.players, player.i.id)
}
\description{
Generates a set of parameters beta.i corresponding to the likelihood of a given player i winning a match.
}
\examples{

coeff.1 <- construct.bradley.terry.model(match.data  = match.data,
                                        all.players = all.players,
                                        player.i.id = player.1.id)

coeff.2 <- construct.bradley.terry.model(match.data  = match.data,
                                        all.players = all.players,
                                        player.i.id = player.2.id)
}
