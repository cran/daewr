\name{rubber}
\alias{rubber}
\docType{data}
\title{Rubber Elasticity data}
\description{
Data from the Rubber Elasticity Study in chapter 5 of Design and Analysis
of Experiments with R
}
\usage{data(rubber)}
\format{
  A data frame with 96 observations on the following 4 variables.
  \describe{
    \item{\code{supplier}}{a factor with levels \code{A} \code{B} \code{C} \code{D} }
    \item{\code{batch}}{a factor with levels \code{I} \code{II} \code{III} \code{IV}}
	\item{\code{sample}}{a factor with levels \code{1} \code{2} }
    \item{\code{elasticity}}{a numeric vector}
  }
}

\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}

\examples{
data(rubber)
}
\keyword{datasets}
