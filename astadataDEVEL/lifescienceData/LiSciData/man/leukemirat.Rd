\name{leukemirat}
\alias{leukemirat}
\docType{data}
\title{Leukemi treatments in rats}
\description{
In an experiment 30 rats had a leukemic condition induced.
Three chemotherapy drugs were applied and as a response the number of cancer cell colonies was
observed.
As covariates the number of red and white blood cells were collected.
Each animal was observed at 4 times.
}
\usage{data(leukemirat)}
\format{
  A data frame with 6 variables.
  \describe{
    \item{ratid:}{rat identification number}
    \item{drug:}{number identifying which drug used}
    \item{time:}{time period when measurements were taken }
    \item{wbc:}{count of white blood cells}
    \item{rbc:}{count of red blood cells}
    \item{y:}{count of cell colonies}
  }
}
\details{
}
\source{
Myers, H.R. , Montgomery D.C., Vining, G.G. (2002) Generalized Linear Models, Exercise 6.6
}
\references{
  ~~ possibly secondary sources and usages ~~
}
\examples{
data(leukemirat)
leukemirat$drug<-factor(leukemirat$drug)
library(lattice)
trellis.device('windows',theme='col.whitebg')
xyplot(y~time|drug,group=ratid,data=leukemirat,type=c('p','l'),span=1,
       strip=strip.custom(var.name='Drug',style=3,strip.name=TRUE))

}
\keyword{datasets}
