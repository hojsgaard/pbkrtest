\name{wandern}
\alias{wandern}
\docType{data}
\non_function{}

\title{wandern}
\usage{data(wandern)}
\description{
365 walkers were asked how frequently they had a walk in the wood 
and how long the tours were on average.
}
\format{
  This data frame contains:
  \describe{
       \item{distance:}{distance of walk}
       \item{frequency:}{weekly: each week, monthly: each month,
                      season: sveral times in season}
       \item{count:}{number of walkers }
  }}
\details{log-linear model, two-ways table}
\source{
Haakenstad (1975) Skogbehandling i utfartsomåde. En opinionsundersøgelse
om Oslomarkar, Meldinger fra Norges Landbrukshøgskole, Nr. 16}
\references{
Søren Andersen (1989) Statistiske Analyse af Tælledata, Matematisk Institut, KVL
p. 1, Eks. 1
}

\examples{
data(wandern)
}
\keyword{datasets}
