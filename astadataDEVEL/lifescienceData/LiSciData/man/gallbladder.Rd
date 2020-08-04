	\name{gallbladder}
\alias{gallbladder}
\docType{data}
\title{Gallbladder volume of dogs}
\description{
Study on   effect  of chalagogues on changes in gallbladder volume of dogs.
The experiment used as treatment two chalagogues and one control.
Six dogs were assigned to each treatment.
The gallbladder volumes were measured by ultrasound every ten minutes for two hours.
}
\usage{data(gallbladder)}
\format{
  A data frame with following 4 variables.
  \describe{
    \item{treat:}{8Clanobutin, Cholechystokynin or water as control}
    \item{dogid:}{dog indendtification number whithin each treatment}
    \item{time:}{time measured}
    \item{gbv:}{gallbladdervolume}
  }
}
\details{
}
\source{
Myers, H.R. , Montgomery D.C., Vining, G.G. (2002) Generalized Linear Models, Exercise 6.4
}
\references{
  ~~ possibly secondary sources and usages ~~
}
\examples{
data(gallbladder)
library(lattice)
trellis.device('windows',theme='col.whitebg')
xyplot(gbv~time|treat,group=dogid,data=gallbladder,type=c('p','l'))
}
\keyword{datasets}
