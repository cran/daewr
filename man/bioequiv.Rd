\name{bioequiv}
\alias{bioequiv}
\docType{data}
\title{Extra-period crossover bioequivalence study}
\description{
Data from the extra-period crossover bioequivalence study in chapter 9 of Design and Analysis
of Experiments with R
}
\usage{data(bioequiv)}
\format{
  A data frame with 108 observations on the following 5 variables.
  \describe{ 
	\item{\code{Group}}{a factor with levels \code{1} \code{2}}
	\item{\code{Subject}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9} \code{10} \code{11} \code{12} \code{13} \code{14} \code{15} \code{16} \code{17} \code{18} \code{19} \code{21} \code{23} \code{24} \code{25} \code{26} \code{27} \code{28} \code{30} \code{31} \code{32} \code{33} \code{34} \code{35} \code{36} \code{120} \code{122} \code{129}}	
	\item{\code{Period}}{a factor with levels \code{1} \code{2} \code{3}}  
	\item{\code{Treat}}{a factor with levels \code{A} \code{B}}
	\item{\code{Carry}}{a factor with levels \code{none} \code{A} \code{B}}
	\item{\code{y}}{a numeric vector }
  }
}

\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}

\examples{
data(bioequiv)
}
\keyword{datasets}
