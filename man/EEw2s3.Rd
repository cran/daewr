\name{EEw2s3}
\alias{EEw2s3}
\title{D-efficient Estimation Equivalent Response Surface Designs}
\description{ Recalls Jones and Goos JQT Estimation Equivalent Response Surface Designs for 2 whole plot factors and 1 sub-plot factor from a catalog 
}
\usage{
EEw2s3(des, randomize=FALSE)
}
\arguments{
  \item{des}{input- a character variable containing the name of a design in the catalog. If left blank, the function prints a table
  showing all the design names in the catalog} 
  \item{randomize}{input- a logical} 
}

\value{ design
}
\references{
Jones, B. and Goos, P.(2012) "An Algorithm for Finding D-Efficient Equivalent-Estimation Second-Order Split Plot Designs", Journal of Quality Technology, Vol 44, No. 4, pp281-303, 2012.
}
\author{
John Lawson
}
\examples{
EEw2s3()
EEw2s3('EE24R8WP')
EEw1s3('EE24R8WP', randomize=TRUE)
     }
\keyword{ datagen }
