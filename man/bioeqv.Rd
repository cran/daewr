\name{bioeqv}
\alias{bioeqv}
\docType{data}
\title{Latin Square bioequivalence experiment}
\description{
Data from the Latin Square bioequivalence experiment in chapter 4 of Design and Analysis
of Experiments with R
}
\usage{data(bioeqv)}
\format{
  A data frame with 9 observations on the following 4 variables.
  \describe{ 
	\item{\code{Period}}{a factor with levels \code{1} \code{2} code{3}}  
	\item{\code{Subject}}{a factor with levels \code{1} \code{2} code{3}}
	\item{\code{Treat}}{a factor with levels \code{A} \code{B}  \code{C}}
	\item{\code{AUC}}{a numeric vector }
  }
}

\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}

\examples{
data(bioeqv)
}
\keyword{datasets}
