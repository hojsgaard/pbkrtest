\name{koagulat}
\alias{koagulat}
\docType{data}
\non_function{}

\title{koagulat}
\usage{data(koagulat)}
\description{
The status of immogubolin was measured with a glutaraldehyd coagulation test.
In one herad the results of the test was recored for heifers [dansk=kvier]
and cows.
}
\format{
  This data frame contains:
  \describe{
       \item{koag:}{results of the coagulation test at three levels, complete, incomplete and negative}
       \item{type:}{type of animal, heifer or cow}
       \item{count:}{the number of animals found in each category}
  }}
\details{Rang test (Wilcoxon)}
\source{
Blom, J.. (1981) Enzootisk pneumoni hos kalve. Epidemiologi og profylakse.
Licentatiatsafhandling, Institut for intern medicin, KVL Købehabvn}

\references{
Søren Andersen (1989) Statistiske Analyse af Tælledata, Matematisk Institut, KVL
p. 52, Kapitel 8, Eks. 1
}

\examples{
data(koagulat)
}
\keyword{datasets}
