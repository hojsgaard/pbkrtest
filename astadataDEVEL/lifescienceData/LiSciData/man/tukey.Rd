\name{tukey}
\alias{tukey}
\docType{data}
\non_function{}

\title{tukey}
\usage{data(tukey)}
\description{
  The weights  and the ages of 13 Thanksgiving turkeys were measured.
  The tukeys were reared at four different locations Georgia,
  Virginia and Wisconsin.
  }
\format{
  This data frame contains:
  \describe{
       \item{weight:}{weight of tukey in lbs.}
       \item{age:}{age of tukey in weeks}
        \item{loc:}{location of rearing: G: Georgia, V=Virginia, W=Wisconsin.}
   }}
\details{linear regression, several regression lines}
\source{
  Draper, N.R. and Smith, H. (1998). Applied Regression Analysis. John Wiley & Sons: New York.
}

\references{
}

\examples{
data(tukey)
require(lattice)
print(xyplot(weight~age|loc,data=tukey))
}
\keyword{datasets}
