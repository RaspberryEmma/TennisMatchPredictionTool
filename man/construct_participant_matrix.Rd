\name{construct.participant.matrix}
\alias{construct.participant.matrix}
\title{Constructing Matrix of Player Participation in Matches}
\usage{
construct.participant.matrix(match.data, all.players)
}
\description{
Generates a matrix of size M by P, where M is the number of matches observed and P is the number of players listed.  This matrix is binary, whose entries (m, p) are equal to player p's age if player p participated in match m and 0 otherwise.
}
\examples{

participant.matrix <- construct.participant.matrix(match.data, all.players)

}
