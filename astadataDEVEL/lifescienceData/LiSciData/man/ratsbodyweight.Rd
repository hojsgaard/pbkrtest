\name{ratsbodyweight}
\alias{ratsbodyweight}
\docType{data}
\title{Development of body weight of rats.}
\description{
Two treatments were compared to a  control on their effect on the growth 
of rats.
}
\usage{data(ratsbodyweight)}
\format{
A dataframe with 6 variables. 
  ratid   treat   week0 week1 week2  week3 week4
\describe{
\item{ratid}{id of rat}
\item{treat}{treatment}
\item{week0}{weight at start of experiment}
\item{week1}{weight at week 1}
\item{week2}{weight at week 2}
\item{week3}{weight at week 3}
\item{week4}{weight at week 4}
}
}

\source{
Box, G.E.P. (1950) Problems in the Analysis of Growth and Wear Curves, 
Biometrics, 6(4), 362-389
}

\references{
}



\examples{
library(lattice)
data(ratsbodyweight)
rat<-reshape(ratsbodyweight,direction='long',varying=list(paste('week',c(0:4),sep='')),v.name='weight',idvar='ratid',timevar='week',time=c(0:4))
 print(
xyplot(weight~week|treat,groups=ratid,data=rat,type='l')
 )  
}
\keyword{datasets}




