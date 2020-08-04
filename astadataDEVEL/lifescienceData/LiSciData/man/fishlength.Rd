\name{fishlength}
\alias{fishlength}
\docType{data}
\non_function{}

\title{fishlength}
\usage{data(fishlength)}
\description{
  Fingerlings of a particular species of fish were put into four tanks
  that were maintained at respective temperatures of 25, 27, 29 and 31
  degrees Celsius. After 14 days, and thereafter, every week
  for 21 weeks, one fish was randomly selected from each tank and its
  length was measured. (1 thou = 1/1000 inch)
}
\format{
  This data frame contains:
  \describe{
       \item{age:}{age of fish in days}
       \item{length25:}{length [thou] at \eqn{25^o}{25
	   degrees}C}
        \item{length27:}{length [thou] at \eqn{27^o}{27
	    degrees}C}	
 \item{length29:}{length [thou] at \eqn{29^o}{29
	   degrees}C}
 \item{length31:}{length [thou] at \eqn{31^o}{31
     degrees}C}
   }}
\details{linear regression, quadratic regression, several regression lines}
\source{
  Freund, R.J., and Littell, R.C. (1991). SAS System for Regression. SAS Institute Inc.
}

\references{
}

\examples{
##fish<-read.table('fishlength.txt',header=TRUE)
data(fishlength)
age<-fishlength$age
temp<-c(25,27,29,31)
par(mfrow=c(2,2))
for (i in 2:5)  {
plot(fishlength[,i]~fishlength[,1],xlab='age',ylab='length',type='l',
main=paste('temperature ',temp[i-1]))
g<-lm(fishlength[,i]~ age + I(age^2))
lines(fishlength[,1],fitted(g),type='l')
}

fishLong<-reshape(fishlength,direction='long',varying=list(paste('length',c(25,27,29,31),sep='')),
v.name='length',time=c(25,27,29,31),timevar='temp')

}
\keyword{datasets}
