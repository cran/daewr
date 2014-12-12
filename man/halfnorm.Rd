\name{halfnorm}
\alias{halfnorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{This function makes a half normal plot of the elements of the vector called effects
}
\description{
This function makes a half normal plot of the elements of the vector called effects
}
\usage{
halfnorm(effects, labs, alpha = 0.05, refline = "TRUE")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{effects}{
input - vector of effects to be plotted  
}
  \item{labs}{
input - vector of labels of the effects to be plotted
}
  \item{alpha}{
input - alpha level for labeling of significant effects using Lenth statistic
}
  \item{refline}{
input - logical variable that indicates whether a reference line is added to the plot (default is "TRUE")
}
}
\author{
John Lawson
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
# Example Separate Normal plots of whole and split plot effects from an unreplicated split-plot 
data(plasma)
sol<-lm(y~A*B*C*D*E,data=plasma)
# get whole plot effects and split plot effects
effects<-coef(sol)
effects<-effects[c(2:32)]
Wpeffects<-effects[c(1:4, 6:11, 16:19, 26)]
Speffects<-effects[c(5,12:15,20:25,27:31)]

#make separate half normal plots
library(BsMD)
halfnorm(Wpeffects,names(Wpeffects),alpha=.10)
halfnorm(Speffects,names(Speffects),alpha=.05)
        }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

