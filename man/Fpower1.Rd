\name{Fpower1}
\alias{Fpower1}
\title{F-Distribution Power Calculation}
\description{ Calculates the power for one-way ANOVA
}
\usage{
Fpower1(alpha,nlev,nreps,Delta,sigma)
}
\arguments{
  \item{alpha}{
input - significance level of the F-test.
}
  \item{nlev}{ 
input - the number of levels of the factor 
}
  \item{nreps}{
input - the number of replicates in each level of the factor.
}
  \item{Delta}{
input - the size of a practical difference in two cell means.
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
%% ~Make other sections like Warning with \section{Warning }{....} ~

\keyword{datagen}

