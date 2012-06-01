\name{vci}
\alias{vci}%- Also NEED an '\alias' for EACH other topic documented here.
\title{confidence limits for method of moments estimators of variance components}
\description{ function for getting confidence intervals on variance components estimated by the method of moments 
}
\usage{
vci(confl,c1,ms1,nu1,c2,ms2,nu2)
}
\arguments{
  \item{confl}{ input- confidence level
}
  \item{c1}{ input - linear combination coefficient of ms1 in the estimated variance component
}
  \item{ms1}{ input - Anova mean square 1
}
  \item{nu1}{ input - Anova degrees of freedom for mean square 1
}
  \item{c2}{ input - linear combination coefficient of ms2 in the estimated variance component
}
  \item{ms2}{ input - Anova mean square 2
}
  \item{nu2}{ input - Anova degrees of freedom for mean square 2
}
}
\value{ returned delta, Lower and Upper limits
}

\author{
John Lawson
}



\examples{
vci(.90,.05,.014852,2,.05,.026885,18)
## The function is currently defined as
vci<-function(confl,c1,ms1,nu1,c2,ms2,nu2){
  delta<-c1*ms1-c2*ms2
  alpha<-1-confl
  Falpha1<-qf(confl,nu1,10000000)
  Falpha12<-qf(confl,nu1,nu2)
  Fconf2<-qf(alpha,nu2,10000000)
  Fconf12<-qf(alpha,nu1,nu2)
  Falpha2<-qf(confl,nu2,10000000)
  Fconf1<-qf(alpha,nu1,10000000)
  Fconf12<-qf(alpha,nu1,nu2)
  G1<-1-(1/Falpha1)
  H2<-(1/Fconf2)-1
  G12<-((Falpha12-1)**2-G1**2*Falpha12**2-H2**2)/Falpha12
  VL<-G1**2*c1**2*ms1**2+H2**2*c2**2*ms2**2+G12*c1*c2*ms1*ms2
  H1<-(1/Fconf1)-1
  G2<-1-(1/Falpha2)
  H12<-((1-Fconf12)**2-H1**2*Fconf12**2-G2**2)/Fconf12
  VU<-H1**2*c1**2*ms1**2+G2**2*c2**2*ms2**2
  L<-delta-sqrt(VL)
  U<-delta+sqrt(VU)
  cat("delta=",delta," Lower Limit=",L," Upper Limit=",U,"\n")
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }

