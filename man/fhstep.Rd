\name{fhstep}
\alias{fhstep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Subsequent steps in a forward stepwise regression that preserves model hierarchy
}
\description{This function performs a single step of a hierarchical forward stepwise regression by entering additional
term(s) to a model already created by ihstep or fhstep. If an interaction or quadratic term is 
entered first, the parent main effects are also entered into the model. This function is called by HierAFS.R
}
\usage{
fhstep(y,des,m,c,prvm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
input - this is a data frame containing a single numeric column of response data.
}
  \item{des}{
input - this is a data frame containing the numeric columns of the candidate independent variables. The column names of des are of length 1 i.e., letters of the alphabet. The m three-level factors always preceed the c two-level factors in the design.
}
  \item{m}{
input - this is an integer equal to the number of three-level factors in the design
}
  \item{c}{
input - this is an integer equal to the number of two-level factors in the design. Note m+c must be equal to the number of columns of des.
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
trm<-ihstep(pd,des,m=8,c=0)
trm<-fhstep(pd,des,m=8,c=0,trm)
trm<-fhstep(pd,des,m=8,c=0,trm)
trm<-fhstep(pd,des,m=8,c=0,trm)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}

