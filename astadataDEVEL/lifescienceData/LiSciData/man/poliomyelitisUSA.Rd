\name{poliomyelitisUSA}
\alias{poliomyelitisUSA}
\docType{data}
\title{Poliomyelitis in the USA}
\description{
cases of poliomyelitis in the USA over from 1970-1983}
\usage{data(poliomyelitisUSA)}
\format{
A dataframe with 13 variables 
\describe{
\item{year:}{year the observation}
\item{JAN-DEC:}{months of the year}
}
}

\source{
Zeger, S.L. (1988) A regression model for time series of counts,
Biometrika 75, 621-629
}

\references{
Lindsey, J.K. (2002) Modelling Frequency and Count Data, Oxford: Claredon Press, Exercise 7.10.(4)
}



\examples{
data(poliomyelitisUSA)
month.names<-names(poliomyelitisUSA[-1])
#read the data inot long format
# more suitable for regression modelling
long <- reshape(poliomyelitisUSA, idvar="year", ids=g$year,
                     times=names(poliomyelitisUSA[-1]), timevar="month",v.names='case',
                     varying=list(names(poliomyelitisUSA[-1])), direction="long")

long$month<-factor(long$month,levels=month.names)

long<-long[order(long$year,long$month),]
}
\keyword{datasets}




