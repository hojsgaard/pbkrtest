\name{tomat}
\alias{tomat}
\docType{data}
\non_function{}
\title{Germination of tomatoes}
\usage{data(tomat)}

\description{
In an experiment to analyse the effect of watering on the germination of seeds,
five watering regimes were applied to seeds on ten trays for each regime.
On three successive dates the percentage of germinating seeds were measured.
}
\format{
   Data frame containing
  \describe{
    \item{day:}{number of day of measurement}
    \item{water:}{watering level, -2,-1,0}
    \item{tray:}{number of tray whtinin a watering regime}
    \item{germ:}{percentage of germinated seeds}
  }
}
\details{
}
\source{
Grafen, A. and Hails, R. (2002), \emph{Modern Statistics for the Life Sciences}, Oxford University Press, London, 
chapter 8.4, p. 149
}

\references{
}


\examples{
data(tomat)
tomat<-transform(tomat,datef=factor(date),trayf=factor(tray),waterf=factor(water))
library(lattice)
print(
xyplot(germ~date|waterf,groups=tray, type='l',data=tomat,
strip = strip.custom ( var.name='Water', strip.names=c(TRUE,TRUE),style=3),layout=c(5,1))
)

}


\keyword{datasets}
