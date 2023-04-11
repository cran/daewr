\name{Fpower2}
\alias{Fpower2}
\title{F-Distribution Power Calculation}
\description{ Calculates the power for a two-way ANOVA
}
\usage{
Fpower2(alpha,nlev,nreps,Delta,sigma)
}
\arguments{
  \item{alpha}{
input - significance level of the F-test.
}
  \item{nlev}{ 
input - vector of length two containing the number of levels of the factors. 
}
  \item{nreps}{
input - the the number of replicates in each combination of factor levels.
}
  \item{Delta}{
input - the size of a practical difference in two marginal factor level means.
}
  \item{sigma}{
input - the standard deviation of the experimental error.
}
}

\value{
probability of exceeding fcrit(alpha, nu1,nu2) with the non-central F-distribution
with nu1 and nu2 degrees of freedom and noncentrality parameter nc
}

\author{
John Lawson
}
\keyword{datagen}

