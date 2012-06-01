\name{BoxM}
\alias{BoxM}
\docType{data}
\title{Box and Meyer's unreplicated \eqn{2^4} from Chapter 3}
\description{
Data from Box and Meyer's unreplicated \eqn{2^4}  in chapter 3 of Design and Analysis
of Experiments with R
}
\usage{data(BoxM)}
\format{
  A data frame with 16 observations on the following 4 variables.
  \describe{
    \item{\code{A}}{a numeric vector containing the coded (-1,1) levels of factor A}
	\item{\code{B}}{a numeric vector containing the coded (-1,1) levels of factor B}
	\item{\code{C}}{a numeric vector containing the coded (-1,1) levels of factor C}
	\item{\code{D}}{a numeric vector containing the coded (-1,1) levels of factor D}
    \item{\code{y}}{a numeric vector containing the response}
  }
}
\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}
\references{ Box, G. E. P. "George's Column", \emph{Quality Engineering}, Vol. 3, pp. 405-410.
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(BoxM)
}
\keyword{datasets}
