\name{PBDes}
\alias{PBDes}
\title{Plackett-Burman Designs}
\description{Creates a 12, 20, or 24 run Plackett-Burman design in a data frame with numeric factor levels by cyclically
rotating the factor leves in the first row
}
\usage{
PBDes(nruns, nfactors, randomize=FALSE)
}
\arguments{
  \item{nruns}{ input- an integer representing the number of runs in the design} 
  \item{nfactors}{input - in integer representing the number of factors in the design} 
  \item{randomize}{input - logical}
}

\value{ design
}
\references{
Lawson, J. (2015) "Design and Analysis of Experiments with R page 229",CRC Press, Boca Raton, 2015.
}
\author{
John Lawson
}

\examples{
PBDes(nruns=12,nfactors=11)
PBDes(nruns=12,nfactors=11,randomize=TRUE)
PBDes(nruns=12,nfactors=9)
PBDes(nruns=20,nfactors=19)
PBDes(nruns=24,nfact=16)
PBDes(nruns=24,nfactors=23)
     }
\keyword{ datagen }
