\name{ihstep}
\alias{ihstep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{This function performs Tukey's single degree of freedom test for interaction in an unreplicated two-factor design
}
\description{This function performs the first step of a hierarchical forward stepwise regression. If an interaction or quadratic term is 
entered first, the parent main effects are also entered into the model.
}
\usage{
ihstep(y,des)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
input - this is a data frame containing a single numeric column of response data.
}
  \item{des}{
input - this is a data frame containing the numeric columns of the candidate independent variables.
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}

