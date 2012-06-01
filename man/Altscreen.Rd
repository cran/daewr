\name{Altscreen}
\alias{Altscreen}
\title{ Jones and Montgomery's alternate 16-run screening designs 
}
\description{
This function retrieves Jones and Montgomery's alternate 16-run screening designs from a stored .rda file.
}
\usage{
Altscreen(nf)
}
\arguments{
  \item{nf}{ This is the number of factors} 
}
\value{ 
 \item{design}{This is a data frame containing the alternate screening design. The columns are labeled
 A, B ...H.}
}
\references{
Jones, B., Montgomery, D.C. (1998) "Alternatives to resolution IV screening designs in 16 runs", \emph{Int. J. Experimental Design and Process Optimisation}, 1, 285-295..

}

\author{ John S. Lawson \email{lawson@byu.edu}}
\examples{
Altscreen(6)

}
\keyword{datagen}
