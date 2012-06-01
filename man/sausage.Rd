\name{sausage}
\alias{sausage}
\docType{data}
\title{Split-plot experiment on sausage casing with RCB in whole plot }
\description{
Data from the Split-plot experiment on sausage casing with RCB in whole plot in chapter 7 of Design and Analysis
of Experiments with R
}
\usage{data(sausage)}
\format{
  A data frame with 32 observations on the following 5 variables.
  \describe{
    \item{\code{Block}}{a factor with levels \code{1} \code{2}}
    \item{\code{Gbatch}}{a factor with levels \code{1} \code{2} \code{3} \code{4}}	
    \item{\code{A}}{a factor with levels \code{-1} \code{1}}
	\item{\code{B}}{a factor with levels \code{-1} \code{1}}
	\item{\code{C}}{a factor with levels \code{-1} \code{1}}
	\item{\code{D}}{a factor with levels \code{-1} \code{1}}
    \item{\code{ys}}{a numeric vector containing the response}
  }
}

\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}

\examples{
data(sausage)
}
\keyword{datasets}
