\name{LGB}
\alias{LGB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{This function uses the LGB Method to detect significant effects in unreplicated fractional factorials.
}
\description{
This function uses the LGB Method to detect significant effects in unreplicated fractional factorials.
}
\usage{
LGB(Beta, alpha = 0.05, rpt = TRUE, plt = TRUE, pltl = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Beta}{
input - this is the numeric vector of effects or coefficients to be tested
}
  \item{alpha}{
input - This is the significance level of the test
}
  \item{rpt}{
input - this is a logical variable that controls whether the report is written (default is TRUE)
}
  \item{plt}{
input - this is a logical variable that controls whether a half-normal plot is made (default is TRUE)
}
  \item{pltl}{
input - this is a logical variable that controls whether the significance limit line is drawn on the half-normal plot (default is TRUE)
}
}


\references{
Lawson, J., Grimshaw, S., Burt, J. (1998) "A quantitative method for identifying active contrasts in unreplicated factorial experiments based on the half-normal plot", Computational Statistics and Data Analysis, 26, 425-436.
}
\author{
John Lawson
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
data(chem)
modf<-lm(y~A*B*C*D,data=chem)
LGB(coef(modf)[-1],rpt=FALSE)


## The function is currently defined as
LGB <- function(Beta, alpha=.05,rpt=TRUE, plt=TRUE, pltl=TRUE) {
sigLGB<-LGBc(Beta,alpha,rpt,plt,pltl)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
