\name{ohio}
\alias{ohio}
\non_function{}
\title{Ohio Children Wheeze Status}
\usage{data(ohio)}
\description{
The \code{ohio} data frame has 2148 rows and 4 columns. The dataset is a
subset of the six-city study, a longitudinal study of the health effects
of air pollution. 
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{resp}{an indicator of wheeze status (1=yes, 0=no)}
    \item{id}{a numeric vector for subject id}
    \item{age}{a numeric vector of age, 0 is 9 years old}
    \item{smoke}{an indicator of maternal smoking at the first year of
      the study}
  }
}
\references{
  Fitzmaurice, G.M. and Laird, N.M. (1993) A likelihood-based method for
  analyzing longitudinal binary responses, \emph{Biometrika} \bold{80}: 141--151.
}
\examples{
data(ohio)
fit <- geese(resp ~ age + smoke + age:smoke, id=id, data=ohio,
             family=binomial, corstr="exch", scale.fix=TRUE)
summary(fit)
fit.ar1 <- geese(resp ~ age + smoke + age:smoke, id=id, data=ohio,
                 family=binomial, corstr="ar1", scale.fix=TRUE)
summary(fit.ar1)
}
\keyword{datasets}
