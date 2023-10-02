\name{shepdog}
\alias{shepdog}
\docType{data}
\non_function{}

\title{Shepdogs}
\usage{data(shepdog)}
\description{
The hips of several shepdogs were x-rayed at the age of 1 year and the
the degree of the disease dysplasie [dansk: hofeledsdysplasi]
was evaluated on a scale of scores from 1 (=best) to 10 (=worst).
The offspring of 5 fathers are contained in the data.
}
\format{
  This data frame contains:
  \describe{
       \item{father:}{name of the father}
       \item{score:}{degree of disease: 1= lowest disease status, 10=severest disease status}
       \item{count:}{number of offspring }
  }}
\details{log-linear model, two-ways table}
\source{
Røgen, E. (1985) 2nd HD statistik for 1984. Schæferhunden, 13, 22-24
}

\references{
Søren Andersen (1989) Statistiske Analyse af Tælledata, Matematisk Institut, KVL
}

\examples{
data(shepdog)
}
\keyword{datasets}
