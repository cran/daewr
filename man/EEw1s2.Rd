\name{EEw1s2}
\alias{EEw1s2}
\title{D-efficient Estimation Equivalent Response Surface Designs}
\description{ Recalls Jones and Goos JQT Estimation Equivalent Response Surface Designs for 1 whole plot factor and 2 sub-plot factors from a catalog 
}
\usage{
EEw1s2(des, randomize=FALSE)
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
EEw1s2( )
EEw1s2('EE12R4WP')
EEw1s2('EE12R4WP', randomize=TRUE)
EEw1s2('EE12R6WP')
EEw1s2('EE12R6WP', randomize=TRUE)
EEw1s2('EE14R7WP')
EEw1s2('EE14R7WP', randomize=TRUE)
EEw1s2('EE15R5WP')
EEw1s2('EE15R5WP', randomize=TRUE)
EEw1s2('EE16R4WP')
EEw1s2('EE16R4WP', randomize=TRUE)
EEw1s2('EE18R6WP')
EEw1s2('EE18R6WP', randomize=TRUE)
EEw1s2('EE20R4WP')
EEw1s2('EE20R4WP', randomize=TRUE)
EEw1s2('EE20R5WP')
EEw1s2('EE20R5WP', randomize=TRUE)
EEw1s2('EE21R7WP')
EEw1s2('EE21R7WP', randomize=TRUE)
EEw1s2('EE24R4WP')
EEw1s2('EE24R4WP', randomize=TRUE)
EEw1s2('EE24R6WP')
EEw1s2('EE24R6WP', randomize=TRUE)
EEw1s2('EE25R5WP')
EEw1s2('EE25R5WP', randomize=TRUE)
EEw1s2('EE28R7WP')
EEw1s2('EE28R7WP', randomize=TRUE)
EEw1s2('EE30R6WP')
EEw1s2('EE30R6WP', randomize=TRUE)
EEw1s2('EE30R5WP')
EEw1s2('EE30R5WP', randomize=TRUE)
EEw1s2('EE35R7WP')
EEw1s2('EE35R7WP', randomize=TRUE)
EEw1s2('EE36R6WP')
EEw1s2('EE36R6WP', randomize=TRUE)
EEw1s2('EE42R7WP')
EEw1s2('EE42R7WP', randomize=TRUE)
     }
\keyword{ datagen }
