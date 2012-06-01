\name{gear}
\alias{gear}
\docType{data}
\title{Unreplicated split-plot fractional-factorial experiment on geometric distortion of drive gears}
\description{
Data from the unreplicated split-plot fractional-factorial experiment on geometric distortion of drive gears in chapter 8 of Design and Analysis
of Experiments with R
}
\usage{data(gear)}
\format{
  A data frame with 16 observations on the following 6 variables.
  \describe{
    \item{\code{A}}{a factor with levels \code{-1} \code{1}}
	\item{\code{B}}{a factor with levels \code{-1} \code{1}}
	\item{\code{C}}{a factor with levels \code{-1} \code{1}}
	\item{\code{P}}{a factor with levels \code{-1} \code{1}}
	\item{\code{Q}}{a factor with levels \code{-1} \code{1}}
    \item{\code{y}}{a numeric vector containing the response}
  }
}

\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}

\examples{
data(gear)
}
\keyword{datasets}
