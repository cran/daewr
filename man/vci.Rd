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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }

