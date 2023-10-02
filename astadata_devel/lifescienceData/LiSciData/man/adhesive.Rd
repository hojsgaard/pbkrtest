\name{adhesive}
\alias{adhesive}
\title{
  Adhesivness of a product}
\description{
  Experiment to investigate the effect of temperature and
  humidity on the force required to separate an adhesive product from a
  certain material. Four specific temperatures and two specific
  humidities were analysed.
\cr
 To way ANOVA
\cr

}
\usage{data(adhesive)}
\format{
A dataframe with 3 variables. 
\describe{
\item{force:}{force to separate the adhesive from the material}
\item{temperature:}{4 levels}
\item{humidity:}{2 levels}
}
}

\source{
Milton, J. S. and Arnold, Jesse, C. (1995)
Introduction to Probability and Statistics, 
McGrwa Hill, New York, p. 607, table 14.2
}



\examples{
library(doBy)
library(lattice)
data(adhesive)
ad<-summaryBy(force~temperature+humidity,data=adhesive,FUN=list(mean))
plot(force~temperature,data=adhesive,type='p',
 pch=c(17,18)[humidity], col= c(2,3)[humidity])
lines(force.mean~temperature,data=ad,subset=humidity==1,type='l',lty=2,col=2)
lines(force.mean~temperature,data=ad,subset=humidity==2,type='l',lty=1,col=3)
title('Adhesive force dependency on temperature and humidity',cex.main=0.9)
legend(1.0,25,legend=c('humidity=1','humidity=2'),pch=c(17,18),lty=c(2,1),
                 col=c(2,3))
   
              

# the above plot by trelluis graphic. The smoothing seems to be too flexible  
xyplot(force~temperature,groups=humidity,data=adhesive,
                   type=c('p','smooth'),auto.key=TRUE,
         main='Adhesive force dependency on temperature and humidity'
                  )


# the same plot with a bit mor contrl over the smoothing with the loess functionm and the parameter span
xyplot(force~temperature,data=adhesive,groups=humidity,
         panel=panel.superpose,panel.groups=function(x,y,...){
                panel.loess(x,y,span=1,...)
                panel.xyplot(x,y,...)},
          auto.key=TRUE,
         main='Adhesive force dependency on temperature and humidity'
                  )

}
\keyword{datasets}




