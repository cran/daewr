\name{Gaptest}
\alias{Gaptest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{This function uses Daniel's Method to find an outlier in an unreplicated  \eqn{2^{(k-p)}} design.
}
\description{
This function uses Daniel's Method to find an outlier in an unreplicated  \eqn{2^{(k-p)}} design.
}
\usage{
Gaptest(DesY)
}
\arguments{
  \item{DesY}{
input this is a data frame containing an unreplicated \eqn{2^{(k-p)}} design. The last variable in the data frame should be the numeric response. 
}
}

\references{
Box, G.E.P. (1991) "George's column: Finding bad values in factorial designs", Quality Engineering, 3, 249-254.
}
\author{
John Lawson
}


%% ~Make other sections like Warning with \section{Warning }{....} ~



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}

