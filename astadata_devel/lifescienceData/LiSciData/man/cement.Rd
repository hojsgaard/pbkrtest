\name{cement}
\alias{cement}
\docType{data}
\non_function{}

\title{cement}
\usage{data(cement)}
\description{
  If cement hardens a certain amount of heats is produced.
  The heat produced was measured for different compositions of cement.
  }
\format{
  This data frame contains:
  \describe{
       \item{alum:}{amount of tricalcium aluminate}
       \item{silic3:}{amount of tricalcium silicate}
       \item{ferrit:}{amount of tetracalcium alumino ferrite}
       \item{silic2:}{Amount of dicalcium silicate}
       \item{heat:}{produced heat[calories]}
   }}
\details{linear regression, several regressors, model selection}
\source{
Wood, H., Steinour, H.H., and Starke, H.R. (1932). "Effect of
Composition of Portland cement on Heat Evolved During Hardening",
Industrial and Engineering Chemistry, 24, pp. 1207--1214.
}

\references{
}

\examples{
data(cement)
pairs(cement)
}
\keyword{datasets}
