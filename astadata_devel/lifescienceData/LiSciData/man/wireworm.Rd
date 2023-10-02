\name{wireworm}
\alias{wireworm}
\docType{data}
\title{Effect of fumigants on wireworms}
\description{
In a Latin Square the  number of wireworms were counted
for 5 different fumigants  applied the year before.
}
\usage{data(wireworm)}
\format{
  A data frame with 25 on the following 4 variables.
  \describe{
  row col treat count
    \item{row}{row of the design}
    \item{col}{clumns of the design}
    \item{treat}{different fumigants: K, M, N, O, P }
    \item{count}{number of wire worms counted in a plot}

}}
\details{
}
\source{
Snedecor G. W and Cochran, W. G (1980) Statistical Methods, 6th edition
}
\references{
Olsson, Ulff (2002) Generlized Linear Models, Studentlitteratur, Lund, p. 129
}
\examples{
data(wireworm)
## maybe str(wireworm) ; plot(wireworm) ...
}
\keyword{datasets}
