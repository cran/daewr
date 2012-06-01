\name{gapstat}
\alias{gapstat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{This function computes the gap statistic which is used to test for an outlier using Daniels method
}
\description{This function computes the gap statistic which is used to test for an outlier using Daniels method
}
\usage{
gapstat(beta, pse)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{beta}{
input - vector of coefficients from saturated model fit to the data
}
  \item{pse}{
input - Lenth's PSE statistic calculated from the elements of beta
}
}

\value{
returned gap statistic
}
\author{
John Lawson
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{

## The function is currently defined as
function (beta, pse) 
{
    p <- length(beta)
    psehe <- pse
    sel <- beta >= 0
    betap <- beta[sel]
    betap <- sort(betap)
    betas <- betap[1]
    sel <- beta < 0
    betan <- beta[sel]
    nn <- length(betan)
    betan <- sort(betan)
    betal <- betan[nn]
    zl <- qnorm((nn - 0.375)/(p + 0.25))
    zs <- qnorm((nn + 1 - 0.375)/(p + 0.25))
    gap <- ((betas - betal)/psehe)/(zs - zl)
    return(gap)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }

