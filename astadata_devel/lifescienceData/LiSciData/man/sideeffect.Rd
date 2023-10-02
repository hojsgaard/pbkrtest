\name{sideeffect}
\alias{sideeffect}
\docType{data}
\non_function{}

\title{Sideffects of cancer operatiion}
\usage{data(sideeffect)}
\description{
Four different operation schemes for cancer of the duodenum [dansk= tolfingertarm kræft]
were compared with respect to the side effects.
The operation  schemes  were in different degree of the reduction of the stomach.
From operation type 1 to 4 the amount of reduction increased.
}
\format{
  This data frame contains:
  \describe{
       \item{operation:}{type of operation, 1: small part of the stomach was removed, 4: large part of the stomach was removed}
       \item{effect:}{degree side effect}
       \item{count:}{number patients }
  }}
\details{log-linear model, two-way table}
\source{
Grizzle, J. E., Starmer, C.F and Koch, G.G. (1969) Analysis of categorical data by linear models,
Biometrics, 40, pp. 393--408
}

\references{
Søren Andersen (1989) Statistiske Analyse af Tælledata, Matematisk Institut, KVL
}

\examples{
data(sideeffect)
}
\keyword{datasets}
