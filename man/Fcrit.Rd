\name{Fcrit}
\alias{Fcrit}%- Also NEED an '\alias' for EACH other topic documented here.
\title{F-Distribution critical values}
\description{ Gets F-distribution critical values
}
\usage{
Fcrit(alpha, nu1, nu2)
}
\arguments{
  \item{alpha}{ input- right tail area beyond critical value
}
  \item{nu1}{ input - numerator degrees of freedom for F-distribution
}
  \item{nu2}{ input - denominator degrees of freedom for F-distribution
}
}

\value{ returned critical value
}

\author{
John Lawson
}

\examples{
Fcrit(.05,2,15)
## The function is currently defined as
function(alpha,nu1,nu2) qf(1-alpha,nu1,nu2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}

