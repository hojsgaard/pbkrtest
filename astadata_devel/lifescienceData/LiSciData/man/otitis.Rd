\name{otitis}
\alias{otitis}
\docType{data}
\non_function{}

\title{Treatment of otitis}
\usage{data(otitis)}
\description{
Children who had one or both ears infected with otitis were either treated with the antibiotic
cefaclor (CEF) or amoxicillin (AMO).
After 14 days the state of their ears were reevaluated.
}
\format{
  This data frame contains:
  \describe{
       \item{treat:}{antibiotic treatment: CEF or AMO}
       \item{entry:}{number of ears with otitits at entry into the study}
       \item{age:}{age of child}
       \item{nclear:}{number of ears clear of acute otitis}
       \item{nchild:}{number of children}
}}
\details{
}
\source{
Rosner (1989) Mutivariate methods for clustered binary data with more than one level of nesting
\emph{JASA}, 88, 373-380, Table 1.
        }
\references{
Dobson, A. (2002) \emph{An introduction to Generalized Linear Models},
London: Chapman \& Hall, 2nd edition, Exercise 11.3, p. 211
}
\examples{
data(otitis)
}
\keyword{datasets}
