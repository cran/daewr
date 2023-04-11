\name{Fpower}
\alias{Fpower}
\title{F-Distribution Power Calculation}
\description{ Calculates the power for the non-central F-distribution
}
\usage{
Fpower(alpha, nu1, nu2, nc)
}
\arguments{
  \item{alpha}{
input - critical value alpha
}
  \item{nu1}{
input - degrees of freedom for numerator
}
  \item{nu2}{
input - degrees of freedom for denominator
}
  \item{nc}{
input - noncentrality parameter
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


