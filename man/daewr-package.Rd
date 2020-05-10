\name{daewr-package}
\alias{daewr-package}
\alias{daewr}
\docType{package}
\title{Data frames and functions for Design and Analysis of Experiments with R}
\description{
This package contains the data sets and functions from the book Design and Analysis of Experiments with R published by CRC in 2013.
}
\details{
\tabular{ll}{
Package: \tab daewr\cr
Type: \tab Package\cr
Version: \tab 1.2-5\cr
Date: \tab 2012-05-10\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
}
\author{
John Lawson

Maintainer: John Lawson <lawsonjsl7net@gmail.com>

}
\references{
J. Lawson, Design and Analysis of Experiments with R, CRC 2013.
}
\keyword{ package }
\examples{
Fcrit(.05,2,15)
Fpower1(alpha=.05,nlev=3,nreps=4,Delta=3,sigma=sqrt(2.1))
BIBsize(6,3)
}
