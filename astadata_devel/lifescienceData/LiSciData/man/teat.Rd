\name{teat}
\alias{teat}
\docType{data}
\non_function{}

\title{teat}
\usage{data(teat)}
\description{
During a year the teat [dansk=patte] lesions 
in a dairy herd were registered.
}
\format{
  This data frame contains:
  \describe{
       \item{teat:}{position of the teat on the udder [dansk= yver]:
                   1: right front, 2: right back,
                   3: left front, 4: left back}
       \item{position:}{position of the lesion: upper=upper part of the teat,
                        lower=lower part of the teat}
       \item{count:}{number of teats}
  }}
\details{log-linear model, two-ways table}
\source{
Agger, J. F. and Hesselholt, M. (1986) Epidemiology of teat leasions
in a dairy herd, I: Description of incidence, location and
clinical appearance, Nord. Vet.-Med. 38, 209-219
}
\references{
Søren Andersen (1989) Statistiske Analyse af Tælledata, Matematisk Institut, KVL
p. 2, Eks. 3
}

\examples{
data(teat)
}
\keyword{datasets}
