\name{augm}
\alias{augm}
\docType{data}
\title{\eqn{2^{(7-3)}}{2^{(7-3)}} arsenic removal experiment augmented with mirror image}
\description{
Data from the  \eqn{2^{(7-3)}} arsenic removal experiment augmented with mirror image in chapter 6 of Design and Analysis
of Experiments with R
}
\usage{data(augm)}
\format{
  A data frame with 8 observations on the following 8 variables.
  \describe{
    \item{\code{A}}{a factor with levels \code{-1} \code{1} }
    \item{\code{B}}{a factor with levels \code{-1} \code{1} }
	\item{\code{C}}{a factor with levels \code{-1} \code{1} }
	\item{\code{fold}}{a factor with levels \code{original} \code{mirror} }	
	\item{\code{D}}{a factor with levels \code{-1} \code{1} }
	\item{\code{E}}{a factor with levels \code{-1} \code{1} }	
	\item{\code{F}}{a factor with levels \code{-1} \code{1} }
	\item{\code{G}}{a factor with levels \code{-1} \code{1} }
    \item{\code{y}}{a numeric vector}
  }
}

\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}

\examples{
data(augm)
}
\keyword{datasets}
