\name{FitDefSc}
\alias{FitDefSc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{An Effective Design Based Model Fitting Method for Definitive Screening Designs
}
\description{This function performs fits a model to a Definitive Screeing Design by first restricting  main effects to the smallest main effects and those significant at at least the .20 level in a main effects model. Next forward stepwise selection is used to enter 2 factor interactions and quadratic effects.}
\usage{
FitDefSc(y,design,alpha=.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
input - this is a vector containing a single numeric column of response data.
}
  \item{design}{
input - this is a data frame containing the numeric columns of the candidate independent variables created by the DefScreen function with only numerical factors i.e. c=0. The factor names or colnames(design) should always be of length 1 (for example letters of the alphabet "A", "B", etc.)}
  \item{alpha}{
input - alpha to enter in the forward stepwise regression with second order candidates should be between 0.05 and 0.20}
}
\author{John Lawson
}

\examples{
design<-DefScreen(m=5,c=0,randomize=FALSE)
Smeso<-c(241,295,260,338,320,265,275,248,66,383,313)
FitDefSc(Smeso,design,alpha=.12)
}
\keyword{htest}

