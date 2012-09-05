\name{bstep}
\alias{bstep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{This function performs Tukey's single degree of freedom test for interaction in an unreplicated two-factor design
}
\description{This function removes the term with the highest p-value from a model already created by ihstep or fhstep. 
}
\usage{
bstep(y,des,prvm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
input - this is a data frame containing a single numeric column of response data.
}
  \item{des}{
input - this is a data frame containing the numeric columns of the candidate independent variables.
}
  \item{prvm}{
input - this is a vector of text names of the terms in the model. This is created as the value resulting from running ihstep or fhstep.
}
}
\value{
returned vector of terms entered in the model at this step.
}
\author{
John Lawson
}

\examples{

library(daewr)
des <- DefScreen( m = 8 )
pd<-c(5.35,4.4,12.91,3.79,4.15,14.05,11.4,4.29,3.56,11.4,10.09,5.9,9.54,4.53,3.919,8.1,5.35)
trm<-ihstep(pd,des)
trm<-fhstep(pd,des,trm)
trm<-fhstep(pd,des,trm)
trm<-fhstep(pd,des,trm)
trm<-bstep(pd,des,trm)
trm<-bstep(pd,des,trm)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}

