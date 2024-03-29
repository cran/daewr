\name{ihstep}
\alias{ihstep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{First step in a forward stepwise regression that preserves model hierarchy
}
\description{This function performs the first step of a hierarchical forward stepwise regression. If an interaction or quadratic term is entered first, the parent main effects are also entered into the model. This function is called by HierAFS.R
}
\usage{
ihstep(y,des,m,c)
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
input - this is an integer equal to the number of three level factors in the design
}
  \item{c}{
input - this is an integer equal to the number of two level factors in the design. Note m+c must be equal to the number of columns of des.
}
}
\value{
returned vector of terms entered in the model at this step.
}
\author{
John Lawson
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}

