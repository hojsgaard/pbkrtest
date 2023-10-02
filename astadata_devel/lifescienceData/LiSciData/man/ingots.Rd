\name{ingots}
\alias{ingots}
\title{rolling property of ingots}
\description{
the  rolling property of ingots is examined under several heating and 
soaking times.
}
\usage{data(ingots)}
\format{
A dataframe with 4 variables. 
\describe{
\item{heat:}{heating time}
\item{soak:}{soaking time}
\item{m:}{number of ingots not ready for rolling}
\item{n:}{number of ingots tested}
}
}

\source{
Cox, D. R.  and  Snell, E. J.(1989), Analysis of Binary Data, 2nd ed. London, U.K.: Chapman and Hall, p10
        }



\examples{
#function to caclulate the empirical logits
empirical.logit<- function(event,total) {
y<-log ((event+0.5)/(total-event+0.5))
y
}

data(ingots)
attach(ingots)

emp.logits<-empirical.logit(m,n)
par(mfrow=c(2,2))

plot(heat,m/n,ylab='proportion non rolling igots')
plot(soak,m/n,ylab='proportion non rolling igots')
plot(heat,emp.logits)
plot(soak,emp.logits)

  



}
\keyword{datasets}




