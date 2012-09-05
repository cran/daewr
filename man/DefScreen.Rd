\name{DefScreen}
\alias{DefScreen}
\title{Definitive Screening Designs}
\description{ Recalls Jones and Nachtsheim's Definitive screening designs for 3-level factors and 3-level factors with added 2-level categorical factors.
}
\usage{
DefScreen(m, c=0, randomize=FALSE)
}
\arguments{
  \item{m}{ input- an integer, the m=number of 3-level factors
}
  \item{c}{ input- an integer, the m=number of 2-level categorical factors, default is zero if not supplied
}
  \item{randomize}{input - logical
}
}
\value{ 
a data frame containing the definitive screening design with 3-level factors first followed by 2-level factors.
}
\references{
Jones, B. and Nachtsheim, C. J.  (2011) "A Class of Three Level Designs for Definitive Screening in the Presence of Second-Order Effects", Journal of Quality Technology, Vol 43, No. 1, 2011, pp 1-15.
Jones, B. and Nachtsheim, C. J.  (2013) "Definitive Screening Designs with Added Two-Level Categorical Factors", Journal of Quality Technology, Vol 44, No. 2, 2013, pp. 121-129.
}
\author{
John Lawson
}

\examples{
DefScreen(m=8,c=2)
DefScreen(12)
DefScreen(m=4,c=4, randomize=TRUE)
     }
\keyword{ datagen }

