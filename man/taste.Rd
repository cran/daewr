\name{taste}
\alias{taste}
\docType{data}
\title{ taste test panel experiment}
\description{
Data from the taste test panel experiment in Chapter 7 of Design and Analysis
of Experiments with R
}
\usage{data(taste)}
\format{
  A data frame with 24 observations on the following 3 variables.
  \describe{
    \item{\code{panelist}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9} \code{10} \code{11} \code{12}}
    \item{\code{recipe}}{a factor with levels \code{"A"} \code{"B"} \code{"C"} \code{"D"}}
    \item{\code{score}}{a numeric vector}
  }
}

\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}

\examples{
data(taste)
}
\keyword{datasets}
