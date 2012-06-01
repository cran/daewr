\name{splitPdes}
\alias{splitPdes}
\docType{data}
\title{Split-plot cookie baking experiment}
\description{
Data from the Split-plot cookie baking experiment in chapter 8 of Design and Analysis
of Experiments with R
}
\usage{data(splitPdes)}
\format{
  A data frame with 24 observations on the following 5 variables.
  \describe{
    \item{\code{short}}{a factor with levels \code{100} \code{80} }
    \item{\code{trayT}}{a factor with levels \code{RoomT} \code{Hot} }
	\item{\code{bakeT}}{a factor with levels \code{low} \code{mid} \code{high}}	
	\item{\code{batch}}{a factor with levels \code{1} \code{2} }
    \item{\code{y}}{a numeric vector}
  }
}

\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}

\examples{
data(splitPdes)
}
\keyword{datasets}
