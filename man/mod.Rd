\name{mod}
\alias{mod}
\title{Mod function}
\description{ Gets mod of a to base b
}
\usage{
mod(a,b)
}
\arguments{
  \item{a}{ input- an integer
}
  \item{b}{ input - an integer
}
}

\value{ remainder of a/b or mod(a,b)
}

\author{
John Lawson
}

\examples{
mod(5,3)
## The function is currently defined as
mod<-function(a,b)
{a-b*floor(a/b)}

     }
\keyword{ datagen }
