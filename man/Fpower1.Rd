\name{Fpower1}
\alias{Fpower1}
\title{F-Distribution Power Calculation}
\description{ Calculates the power for one-way ANOVA
}
\usage{
Fpower1(alpha,nlev,nreps,Delta,sigma)
}
\arguments{
  \item{alpha}{
input - significance level of the F-test.
}
  \item{nlev}{ 
input - the number of levels of the factor 
}
  \item{nreps}{
input - the number of replicates in each level of the factor.
}
  \item{Delta}{
input - the size of a practical difference in two cell means.
}
  \item{sigma}{
input - the standard deviation of the experimental error.
}
}

\value{
probability of exceeding fcrit(alpha, nu1,nu2) with the non-central F-distribution
with nu1 and nu2 degrees of freedom and noncentrality parameter nc
}

\author{
John Lawson
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
Fpower1(alpha=.05,nlev=3,nreps=4,Delta=3,sigma=sqrt(2.1))


rmin <-2 #smallest number of replicates considered
rmax <-6 # largest number of replicates considered
alpha <- rep(0.05, rmax - rmin +1) 
sigma <-rep(sqrt(2.1), rmax - rmin +1)
nreps <-c(rmin:rmax)
nlev <- rep(3,rmax - rmin +1)
nreps <- rmin:rmax
Delta <- rep(3,rmax - rmin +1)
power <- Fpower1(alpha,nlev,nreps,Delta,sigma)
data.frame(r=nreps,Power=power)


## The function is currently defined as
Fpower1<-function(alpha=NULL, nlev=NULL,nreps=NULL, Delta=NULL, sigma=NULL)
{
##### Power Calculation for one way ANOVA ###########
# Argument list
# alpha the significance level of the test
# nlev the number of levels of the factor 
# nreps the number of replicates in each level of the factor
# Delta the size of a practical difference in two cell means
# sigma the standard deviation of the experimental error
#####################################################
if (is.null(alpha)|is.null(nlev)|is.null(nreps)|is.null(Delta)|is.null(sigma))
  stop("you must supply alpha, nlev, nreps, Delta and sigma")
css<-(Delta^2)/2
nc<- (nreps*css)/(sigma^2)
df1<-nlev-1
df2<-(nreps-1)*nlev
power <- 1-pf(Fcrit(alpha,df1,df2),df1,df2,nc)
return(power)
}
}
\keyword{datagen}

