\name{OptPB}
\alias{OptPB}
\title{Optimum Plackett-Burman Designs}
\description{ Selects the columns from a Plackett-Burman Design Produced by FrF2 that will minimize 
model dependence for main effects and two factor interactions and returns the design in a data frame
}
\usage{
OptPB(nruns, nfactors, randomize=FALSE)
}
\arguments{
  \item{nruns}{ input- an integer representing the number of runs in the design} 
  \item{nfactors}{input - in integer representing the number of factors in the design} 
  \item{randomize}{input - logical}
}

\value{ design
}
\references{
Fairchild, K. (2011) "Screening Designs that Minimize Model Dependence", MS Project Department of Statistics Brigham Young University, Dec. 2011.
}
\author{
John Lawson
}

\examples{
OptPB(12,8)
     }
\keyword{ datagen }
