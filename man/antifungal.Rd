\name{antifungal}
\alias{antifungal}
\docType{data}
\title{Two-period crossover study of antifungal agent}
\description{
Data from the Two-period crossover study of an antifungal agent in chapter 9 of Design and Analysis
of Experiments with R
}
\usage{data(antifungal)}
\format{
  A data frame with 34 observations on the following 5 variables.
  \describe{ 
	\item{\code{Group}}{a factor with levels \code{1} \code{2}}
	\item{\code{Subject}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9} \code{10} \code{11} \code{12} \code{14} \code{15} \code{16} \code{17} \code{18}}	
	\item{\code{Period}}{a factor with levels \code{1} \code{2} }  
	\item{\code{Treat}}{a factor with levels \code{A} \code{B}}
	\item{\code{pl}}{a numeric vector }
  }
}

\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}

\examples{
data(antifungal)
}
\keyword{datasets}
