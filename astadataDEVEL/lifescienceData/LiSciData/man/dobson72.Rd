\name{dobson72}
\alias{dobson72}


\non_function{}
\title{Beetles killed by disulphide}
\usage{data(dobson72)}
\description{
  Effect of gaseous carbon disulphide on beetles after 5 hours of exposure.
  \cr
  binary logistic regression
  }
\format{
   Data frame containing
  \describe{
    \item{dose:}{dose of disulphide: [log10 of CS2 measured in mg/l]}
    \item{nbeetle:}{number of beetles exposed}
    \item{nkilled:}{number of beetles died}
  } 
}
\details{
Data are from the exercise Table 7.2 of A. Dobson, page 119.
}
\source{
Dobson, A. (2002) \emph{An introduction to Generalized Linear Models},
London: Chapman \& Hall
        }
\references{
}
\examples{
data(dobson72)
summary(dobson72)
}





\keyword{datasets}
