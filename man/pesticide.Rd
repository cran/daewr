\name{pesticide}
\alias{pesticide}
\docType{data}
\title{pesticide application experiment}
\description{
Data from the pesticide application experiment in chapter 5 of Design and Analysis
of Experiments with R
}
\usage{data(pesticide)}
\format{
  A data frame with 16 observations on the following 4 variables.
  \describe{
    \item{\code{form}}{a factor with levels \code{A} \code{B} }
    \item{\code{tech}}{a factor with levels \code{1} \code{2} }
	\item{\code{plot}}{a factor with levels \code{1} \code{2}}
    \item{\code{residue}}{a numeric vector}
  }
}

\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}

\examples{
data(pesticide)
}
\keyword{datasets}
