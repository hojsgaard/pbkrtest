\name{dobson72Dis}
\docType{data}
\alias{dobson72Dis}

\non_function{}
\title{Beetles killed by disulphide}
\usage{data(dobson72Dis)}
\description{
  Same data as \eqn{dobson72}{dobson72}. Data is disaggregated such that
 for each individual beetle it is recorded whether it died ot not.
  Effect of gaseous carbon disulphide after 5 hours of exposure to
  beetles.
  \cr
  binary logistic regression
  }
\format{
   Data frame containing
  \describe{
    \item{dose:}{dose of disulphide: [log10 of CS2 measured in mg/l]}
    \item{y:}{1: beetle was killed, 0: beetle survived}
}
}
\details{
Data are from the Table 7.2, page 119
}
\source{
Dobson, A. (2002) \emph{An introduction to Generalized Linear Models},
London: Chapman \& Hall
        }
\references{
}
\examples{
data(dobson72Dis)
}





\keyword{datasets}
