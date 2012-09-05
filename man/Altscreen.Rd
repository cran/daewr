\name{Altscreen}
\alias{Altscreen}
\title{Alternate 16 run screening designs}
\description{ Recalls Jones and Montgomery's 16 run screening designs from data frames
}
\usage{
Altscreen(nfac, randomize=FALSE)
}
\arguments{
  \item{nfac}{ input- an integer
}
  \item{randomize}{input - logical
}
}
\value{ 
a data frame containing the alternate screening design
}
\references{
Jones, B. and Montgomery, D. C.  (2010) "Alternatives to resolution IV screening designs in 16 runs", Int. J. Experimental Design and Process Optimization, Vol 1, No. 4, 2010.
}
\author{
John Lawson
}

\examples{
Altscreen(6)
Altscreen(6, randomize=TRUE)
     }
\keyword{ datagen }
