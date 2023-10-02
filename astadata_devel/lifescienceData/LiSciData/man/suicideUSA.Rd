\name{suicideUSA}
\alias{suicideUSA}
\docType{data}
\title{Suicides in the USA in the years 1968-1970}
\description{
Suicides inm the USA
}
\usage{data(suicideUSA)}
\format{
A dataframe with 3 variables. 
\describe{
\item{year:}{year of suicide}
\item{month:}{month of the year}
\item{n:}{number of recorded suicides}
}
}

\source{
Haberman, S.J. (1978) Analysis of Qualitative Data, Vol. I,
Introductory Topics, New York: Academic Press
}

\references{
Lindsey, J.K. (2002) Modelling Frequency and Count Data, Oxford: Claredon Press, Exercise 1.6(4)
}



\examples{
data(suicideUSA)
attach(suicideUSA)
month<-ordered(month,levels=month[1:12])
w<-xtabs(n~month+year)
print(w)
days<-rep(31,12)
days[c(4,6,9,11,9)]<-30
days[2]<-28
plot(w[,1]/days,xlab='month',ylab='suicides per day',axes=FALSE)
axis(2)
axis(1,labels=levels(month),at=1:12,cex.axis=0.9)
box()
title('suicides in the USA in 1968')


}
\keyword{datasets}




