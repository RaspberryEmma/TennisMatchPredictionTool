\name{construct.wins.indicator}
\alias{construct.wins.indicator}
\title{Constructing Binary Vector of Player Victory in Natches}
\usage{
construct.wins.indicator(match.data, player.i.id)
}
\description{
Generates a vector of size M, where M is the number of matches observed.  This vector is binary, whose entries (m) are 1 if our given player i won match m and 0 otherwise.
}
\examples{

player.1.wins.indicator <- construct.wins.indicator(match.data, player.1.id)
player.2.wins.indicator <- construct.wins.indicator(match.data, player.2.id)

}
