\name{zsystem}
\alias{zsystem}
\docType{data}
\non_function{}

\title{zsystem}
\usage{data(zsystem)}
\description{
Distribution of genotypes for blood types in the Z-system 
for three cattle races.
}
\format{
  This data frame contains:
  \describe{
       \item{race:}{cattle race : rdm: danish Red [dansk=Rød dansk mælkerace],
                                  sdm: Holstein [dansk=sort brogede mælkerace], jersey }
       \item{genotype:}{genotype of blood-type}
       \item{count:}{number of cows }
  }}
\details{log-linear model, two-ways table}
\source{
Neimann-Sørensen, A. (1958) Blood groups of cattle, Doktorafhandling,
KVL, København
}

\references{
Søren Andersen (1989) Statistiske Analyse af Tælledata, Matematisk Institut, KVL
p. 2, Eks.4
}

\examples{
data(zsystem)
}
\keyword{datasets}
