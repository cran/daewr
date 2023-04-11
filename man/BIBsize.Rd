\name{BIBsize}
\alias{BIBsize}
\title{Balanced incomplete blocksize}
\description{This function computes the number of blocks, treatment frequency and lambda for a potential BIB design
}
\usage{
BIBsize(t,k)
}
\arguments{
  \item{t}{
input - number of levels of the treatment factor
}
  \item{k}{
input - blocksize or number of experimental units per block
}
}

\value{
a list containing the b=number of blocks, r=number of treatment replicates and lambda for a potential BIB design with t levels of treatment factor and blocksize k.
}

\author{
John Lawson
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\keyword{datagen}