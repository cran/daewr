\name{residue}
\alias{residue}
\docType{data}
\title{Herbicide degradation experiment}
\description{
Data from the Herbicide degradation experiment in chapter 9 of Design and Analysis
of Experiments with R
}
\usage{data(residue)}
\format{
  A data frame with 16 observations on the following 3 variables.
  \describe{
 	\item{\code{soil}}{a factor with levels \code{"C"} \code{"P"}}
 	\item{\code{moisture}}{a factor with levels \code{"L"} \code{"H"}}
	\item{\code{temp}}{a factor with levels \code{10} \code{30}}
    \item{\code{X1}}{a numeric vector }
    \item{\code{X2}}{a numeric vector }
    \item{\code{X3}}{a numeric vector }
    \item{\code{X4}}{a numeric vector }
    \item{\code{X5}}{a numeric vector }
  }
}

\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}

\examples{
data(residue)
}
\keyword{datasets}
