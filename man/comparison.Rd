\name{comparison}
\alias{comparison}
\title{Performing Bradley Terry Pairwise Comparisons}
\usage{
compare(beta.i,  beta.j)
}
\description{
Generates a number bounded between 0 and 1.  This result is the probability of player i winning against player j in a pairwise comparison, given the GLM parameters beta.i and beta.j.
}
\examples{

prob.1.wins <- comparison(beta.1, beta.2)
prob.2.wins <- comparison(beta.2, beta.1)

}
