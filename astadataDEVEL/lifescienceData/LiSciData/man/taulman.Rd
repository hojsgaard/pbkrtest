\name{taulman}
\alias{taulman}
\docType{data}
\non_function{}

\title{Squirrels}
\usage{data(taulman)}
\description{
The demographic distribution of caught flying squirrels in reponse to experimental logging (harvest)
and year of the eperiment.
}
\format{
  This data frame contains:
  \describe{
       \item{treat:}{treatment (hervest, control}
       \item{year:}{year if experiment}
       \item{age:}{age of the caught flying squirrel}
       \item{count:}{number of squirrels caught}

  }}
\details{Logistic regression, log-linear model, three-way table}
\source{
Taulman. J.F, Smith, K.G. and Thill, R.E. (1998)
Demographic and behavioral responses of southern flying squirrels to experimental logging in Arkansas
\emph{Ecological Applications}, 8, pp. 1144-1155   
}

\references{
Quinn, Gerry. P. and Keough, Michael J. (2002) \emph{Experimental Design and Data Analysis for Biologists}, University Press, Cambridge
Table 14-3 and Box 14-6
}

\examples{
data(taulman)
tab<-xtabs(count~treat+year+age,data=taulman)
print(ftable(tab))
}
\keyword{datasets}
