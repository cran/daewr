\name{Tukey1df}
\alias{Tukey1df}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{This function performs Tukey's single degree of freedom test for interaction in an unreplicated two-factor design
}
\description{This function performs Tukey's single degree of freedom test for interaction in an unreplicated two-factor design
}
\usage{
Tukey1df(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
input - this is a data frame with three variables, the first variable is a numeric response and next two variables are factors. There should be \eqn{ab} lines in the data frame where \eqn{a} is the number of levels of the first factor, and \eqn{b} is the number of levels of the second factor.
}
}

\author{
John Lawson
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}