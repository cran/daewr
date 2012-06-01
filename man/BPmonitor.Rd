\name{BPmonitor}
\alias{BPmonitor}
\docType{data}
\title{blood pressure monitor experiment}
\description{
Data from the blood pressure monitor experiment experiment in Chapter 7 of Design and Analysis
of Experiments with R
}
\usage{data(BPmonitor)}
\format{
  A data frame with 12 observations on the following 3 variables.
  \describe{
    \item{\code{Block}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} }
    \item{\code{Treatment}}{a factor with levels \code{"P"} \code{"A"} \code{"B"} \code{"C"}}
    \item{\code{pressure}}{a numeric vector}
  }
}
\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}
\examples{
data(BPmonitor)
}
\keyword{datasets}
