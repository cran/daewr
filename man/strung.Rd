\name{strung}
\alias{strung}
\docType{data}
\title{Repeated measures study with dairy cow diets}
\description{
Data from the Repeated measures study with dairy cow diets in chapter 9 of Design and Analysis
of Experiments with R (strung out format)
}
\usage{data(strung)}
\format{
  A data frame with 120 observations on the following 5 variables.
  \describe{ 
 	\item{\code{Diet}}{a factor with levels \code{"Barley"} \code{"Mixed"} \code{"Lupins"}}
	\item{\code{Cow}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9} \code{10}}	
	\item{\code{week}}{a factor with levels \code{1} \code{2} \code{3} \code{4}}  
	\item{\code{protein}}{a numeric vector }
  }
}

\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}

\examples{
data(strung)
}
\keyword{datasets}
