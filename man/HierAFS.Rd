\name{HierAFS}
\alias{HierAFS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RSM forward regression keeping model hierarchy
}
\description{This function performs a hierarchical forward stepwise regression. If an interaction or quadratic term is entered in the model, the parent main effects are also entered into the model.
}
\usage{
HierAFS(y,x,m,c,step) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
input - this is a vector containing a single numeric column of response data.
}
  \item{x}{
input - this is a data frame containing the numeric columns of the candidate independent variables. The m three-level factors always preceed the c two-level factors in the design. The factor names or colnames(x)
should always be of length (for example letters of the alphabet "A", "B", etc.)
}
  \item{m}{
input - this is an integer equal to the number of three-level factors in the design
}
  \item{c}{
input - this is an integer equal to the number of two-level factors in the design. Note m+c must be equal to the number of columns of des.
}
  \item{step}{
input - this is a single numeric value containing the n umber of steps requested.
}
}
\value{
returned data frame the first column is a factor variable containing the formula for the model fit at each step, the second numeric column is the R-square statistic for the model fit with each formula.
}
\author{
Gerhard Krennrich, and modified by John Lawson
}

\examples{
#Definitive Screening Design
library(daewr)
set.seed(1234)
x <- DefScreen(m=5,c=0)
x$y <- 3*x$A + 2*x$B + 3*x$A*x$B + 2*x$A^2 + 2*x$C+rnorm(nrow(x),0,.5) 
HierAFS(x$y,x[,-6],m=5,c=0,step=3)
# Alternate Screening Example
library(daewr)
Design<-Altscreen(nfac=6,randomize=FALSE)
Thickness<-c(4494,4592,4357,4489,4513,4483,4288,4448,4691,4671,4219,4271,4530,4632,4337,4391)
HierAFS(Thickness,Design,m=0,c=6,step=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}

