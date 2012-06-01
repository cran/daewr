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


\examples{
BIBsize(6,3)
## The function is currently defined as
BIBsize<-function(t,k)
{
  b<-t
  r<-0
  lambda<-0
  check<-0
  while (check==0) {
   while (r==0) {
     #cat("r=",r)
     testr<-(b*k)/t
     #cat("testr=",testr,"b=",b)
     if (testr==floor(testr)) {
       r<-testr
       } else {
       b<-b+1
       }
     }
      #cat("b=",b, "r=",r)
      testl<-(r*(k-1))/(t-1)
      #cat("testl=",testl,"b=",b)
      if (testl==floor(testl)) {
       lambda<-testl
       check=1
       } else {
       r<-0
       b<-b+1
      #cat("b=",b, "r=",r)
        }

    #cat("lambda=",lambda)
    }
  cat("Posible BIB design with b=",b," and r=",r," lambda=",lambda,"\n") 
}
       }
\keyword{datagen}