\name{Fpower2}
\alias{Fpower2}
\title{F-Distribution Power Calculation}
\description{ Calculates the power for a two-way ANOVA
}
\usage{
Fpower2(alpha,nlev,nreps,Delta,sigma)
}
\arguments{
  \item{alpha}{
input - significance level of the F-test.
}
  \item{nlev}{ 
input - vector of length two containing the number of levels of the factors. 
}
  \item{nreps}{
input - the the number of replicates in each combination of factor levels.
}
  \item{Delta}{
input - the size of a practical difference in two marginal factor level means.
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



\examples{
power <- Fpower2(.05, nlev = c(4,4), nreps=2, Delta= 1, sigma=.32)

rmin <- 2 # smallest number of replicates
rmax <- 4 # largest number of replicates
alpha <- .05
sigma <- .32
Delta <- 1.0
nlev <- c(4,4)
nreps <- c(rmin:rmax)
result <- Fpower2(alpha, nlev, nreps, Delta, sigma)
options(digits = 5)
result


## The function is currently defined as
Fpower2<-function(alpha=NULL, nlev=NULL,nreps=NULL, Delta=NULL, sigma=NULL)
{
##### Power Calculation for two way ANOVA ###########
# Argument list
# alpha the significance level of the test.
# nlev vector containing the number of levels of the factors. 
# nreps the number of replicates in each combination of factor levels.
# Delta the size of a practical difference in two marginal factor level means.
# sigma the standard deviation of the experimental error.
############################################################
if (is.null(alpha)|is.null(nlev)|is.null(nreps)|is.null(Delta)|is.null(sigma))
  stop("you must supply alpha, nlev, nreps, Delta and sigma")
if(length(nlev)<2)
  stop ("nlev must be a two component vecto containing levels of the 1st and 2nd factors")
a <- nlev[1]
b <- nlev[2]
cssb <- (Delta^2)/2
ncb <- a*(nreps*cssb)/(sigma^2)
cssa<-(Delta^2)/2
nca<- b*(nreps*cssa)/(sigma^2)
dfa<- a-1
dfb<- b-1
df2<-(nreps-1)*b*a
powera <- 1-pf(Fcrit(alpha,dfa,df2),dfa,df2,nca)
powerb <- 1-pf(Fcrit(alpha,dfb,df2),dfa,df2,nca)
result <-cbind(nreps,df2,powera,powerb)
}
}
\keyword{datagen}

