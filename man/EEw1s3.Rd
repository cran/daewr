\name{EEw1s3}
\alias{EEw1s3}
\title{D-efficient Estimation Equivalent Response Surface Designs}
\description{ Recalls Jones and Goos JQT Estimation Equivalent Response Surface Designs for 1 whole plot factor and 3 sub-plot factors from a catalog 
}
\usage{
EEw1s3(des, randomize=FALSE)
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
EEw1s3()
EEw1s3('EE18R6WP')
EEw1s3('EE18R6WP', randomize=TRUE)
     }
\keyword{ datagen }
