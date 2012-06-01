\name{Bdish}
\alias{Bdish}
\docType{data}
\title{Confounded Block Dishwashing Experiment}
\description{
Data from the Confounded Block Dishwashing Experiment in chapter 7 of Design and Analysis
of Experiments with R
}
\usage{data(Bdish)}
\format{
  A data frame with 16 observations on the following 5 variables.
  \describe{
    \item{\code{Blocks}}{a factor with levels \code{1} \code{2} \code{3} \code{4}}
    \item{\code{A}}{a factor with levels \code{-1} \code{1}}
	\item{\code{B}}{a factor with levels \code{-1} \code{1}}
	\item{\code{C}}{a factor with levels \code{-1} \code{1}}
	\item{\code{D}}{a factor with levels \code{-1} \code{1}}
    \item{\code{y}}{a numeric vector containing the response}
  }
}
\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}
\examples{
data(Bdish)
}
\keyword{datasets}
