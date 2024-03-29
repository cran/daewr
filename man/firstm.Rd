\name{firstm}
\alias{firstm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find first term to enter forward stepwise regression that preserves model hierarchy
}
\description{This function finds the first term to enter a hierarchical forward stepwise regression. If the term is an interaction or quadratic term, the parent main effects are also included. This function is called by ihstep.R
}
\usage{
firstm(y,des)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
input - this is a data frame containing a single numeric column of response data.
}
\item{des}{
input - this is a data frame containing the numeric columns of the candidate independent variables. The column names of des are of length 1. The m three-level factors always preceed the c two-level factors in the design.
}
}
\value{
returned vector of terms to be entered in the model at the first step.
}
\author{
John Lawson
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}

