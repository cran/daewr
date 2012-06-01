\name{ModelRobust}
\alias{ModelRobust}
\title{Model Robust Factorial Designs}
\description{ Recalls Li and Nachtsheim's model robust factorial designs from a catalog of data frames
}
\usage{
ModelRobust(des)
}
\arguments{
  \item{des}{input- a character variable containing the name of a design in the catalog. If left blank, the function prints a table
  showing all the design names in the catalog} 
}

\value{ design
}
\references{
Li, W. and Nachtsheim, C. J. (2000) "Model Robust factorial Designs", Technometrics, Vol 42, No. 4, pp345-352, 2000.
}
\author{
John Lawson
}

\examples{
ModelRobust()
ModelRobust('MR8m4g3')
     }
\keyword{ datagen }
