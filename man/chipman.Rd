\name{chipman}
\alias{chipman}
\docType{data}
\title{Williams' crossover design for sprinting experiment}
\description{
Data from the Williams' crossover design for sprinting experiment in chapter 9 of Design and Analysis
of Experiments with R
}
\usage{data(chipman)}
\format{
  A data frame with 36 observations on the following 5 variables.
  \describe{ 
 	\item{\code{Square}}{a factor with levels \code{1} \code{2}}
	\item{\code{Group}}{a factor with levels \code{1} \code{2} \code{3}}
	\item{\code{Subject}}{a factor with levels \code{1} \code{2} code{3} \code{4} \code{5} code{6} \code{7} \code{8} code{9} \code{10} \code{11} code{12}}	
	\item{\code{Period}}{a factor with levels \code{1} \code{2} \code{3}}  
	\item{\code{Treat}}{a factor with levels \code{1} \code{2} \code{3}}
	\item{\code{Carry}}{a factor with levels \code{0} \code{1} \code{2} \code{3}}
	\item{\code{Time}}{a numeric vector }
  }
}
\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}
\examples{
data(chipman)
}
\keyword{datasets}
