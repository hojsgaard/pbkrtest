\name{orangesale}
\alias{orangesale}
\docType{data}
\non_function{}

\title{orangesale}
\usage{data(orangesale)}
\description{
  The sale for two varieties of oranges vere observed at 6 differentr
  stores
  for all six sales-days of a week.
    }
\format{
  This data frame contains:
  \describe{
       \item{store:}{label for the store.}
       \item{day:}{weekday of sale}
       \item{price1:}{Proce for variety 1.}
       \item{price2:}{Proce for variety 2.}
       \item{sale1:}{Sale per customer 1.}
       \item{sale2:}{Sale per customer 2.}
}}
\details{linear regression, several regression lines}
\source{
  Littel, R. C.,  Stroup, W.W and  Freund, R.J. (2002) SAS for linear models. Chapter 7.3.2,
  p. 141
}

\references{
}

\examples{
data(orangesale)
require(lattice)
print(xyplot(sale1~price2|day,groups=store,data=orangesale))
}
\keyword{datasets}
