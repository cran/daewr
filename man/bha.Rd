\name{bha}
\alias{bha}
\docType{data}
\title{mouse liver enzyme experiment}
\description{
Data from the mouse liver enzyme experiment in chapter 4 of Design and Analysis
of Experiments with R
}
\usage{data(bha)}
\format{
  A data frame with 16 observations on the following 4 variables.
  \describe{ 
	\item{\code{block}}{a factor with levels \code{1} \code{2}}
    \item{\code{strain}}{a factor with levels \code{A/J} \code{129O1a}  \code{NIH}  \code{BALB/c}}
	\item{\code{treat}}{a factor with levels \code{treated} \code{control}}
	\item{\code{y}}{a numeric vector }
  }
}

\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}

\examples{
data(bha)
}
\keyword{datasets}
