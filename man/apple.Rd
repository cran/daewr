\name{apple}
\alias{apple}
\docType{data}
\title{Confounded apple slice browning experiment}
\description{
Data from the confounded apple slice browning experiment in chapter 7 of Design and Analysis
of Experiments with R
}
\usage{data(apple)}
\format{
  A data frame with 24 observations on the following 4 variables.
  \describe{
    \item{\code{Block}}{a factor with levels \code{1} \code{2} \code{3} \code{4}}
    \item{\code{A}}{a factor with levels \code{0} \code{1} \code{2} \code{3}}
	\item{\code{B}}{a factor with levels \code{0} \code{1} \code{2}}
    \item{\code{rating}}{a numeric vector containing the response}
  }
}

\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}

\examples{
data(apple)
}
\keyword{datasets}
