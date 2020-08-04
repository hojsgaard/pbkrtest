\name{lungcancerDk}
\alias{lungcancerDk}
\title{lung cancer i danish citites}
\description{
Lung cancer in four Danish cities in 1968-1971
}
\usage{data(lungcancerDk)}
\format{
A dataframe with 4 variables. 
\describe{
\item{age:}{age group}
\item{city:}{city in which the case ocurred}
\item{deaths:}{number of deaths}
\item{population:}{size of population}
}
}

\source{
Andersen, E.B. (1977) 
\emph{Multiplicative Poisson models with unequal cell rates},
Scandinavian Journal of Statistics, 4, 153-158
}
\references{Lindsey, J.K. (1995) \emph{Modelling Frequency and Count Data},
Oxford Science Publications: Oxford, p. 157
}


\examples{
data(lungcancerDk)
attach(lungcancerDk)
rate<-deaths/population *1e5
dat<-data.frame(rate,lungcancerDk)
w<-reshape(dat,v.names=c('rate'),idvar='age',direction='wide',timevar='city')

age1<-c(40,55,60,65,70)
age2<-c(55,60,65,70,75)
Age<-c((age1+age2)/2,80)

 matplot(Age,w[,4:7],type='l',ylab='death /(100.000*year)',
                 lwd=c(1:4),col=c(1:4),lty=c(1:4))
 legend(65,500,legend=c('Fredericia','Horsens','Kolding','Vejle'),lty=c(1:4),lwd=c(1:4),col=c(1:4))
title('lung cancer death in four danish cities')
}

\keyword{datasets}




