\name{budworm}
\alias{budworm}


\non_function{}
\title{Effect of Insecticide on survivial of tobacco budworms}
\usage{data(budworm)}
\description{
 number of killed budworms exposed to an insecticide 
}
\format{
  This data frame contains  12 rows and 4 columns:
  \describe{
    \item{sex:}{sex of the budworm}
    \item{dose:}{dose of the insecticide trans-cypermethrin in [\eqn{\mu}{mu}g]}
    \item{ndead:}{budworms killed in a trial}
    \item{ntotal:}{total number of budworms exposed per trial }
  }
}
\details{
mortality of the moth  tobacco budworm 'Heliothis virescens'
for 6 doses of the pyrethroid  trans-cypermethrin differentiated with
respect to sex
}
\source{
 Collet, D. (1991) Modelling Binary Data, Chapman & Hall, London, 
Example 3.7
        }
\references{
Venables, W.N; Ripley, B.D.(1999) Modern Applied Statistics with S-Plus,
Heidelberg, Springer, 3rd edition, chapter 7.2
}
\examples{
data(budworm)

#function to caclulate the empirical logits
empirical.logit<- function(nevent,ntotal) {
y<-log ((nevent+0.5)/(ntotal-nevent+0.5))
y
}


#plot the empirical logits against log-dose

log.dose<-log(budworm$dose)
emp.logit<-empirical.logit(budworm$ndead,budworm$ntotal)
plot(log.dose,emp.logit,type='n',xlab='log-dose',ylab='emprirical logit')
title('budworm: emprirical logits of probability to die ')
male<-budworm$sex=='male'
female<-budworm$sex=='female'
lines(log.dose[male],emp.logit[male],type='b',lty=1,col=1)
lines(log.dose[female],emp.logit[female],type='b',lty=2,col=2)
legend(0.5,2,legend=c('male','female'),lty=c(1,2),col=c(1,2))





\dontrun{
* SAS example;
data budworm;
infile 'budworm.txt' firstobs=2;
input sex dose ndead ntotal;
run;
}

}
\keyword{datasets}
