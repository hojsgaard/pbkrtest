\name{prostaticCancer}
\alias{prostaticCancer}
\non_function{}
\title{Nodal involvement of prostatic cancer}
\usage{data(prostaticCancer)}
\description{
The \code{prostaticCancer} data frame is about a study on the involvement of
lymph nodes in prostatic cancer for 53 men.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{age:}{ age of men in years}
    \item{acid:}{level of serum acid phosphatase in King-Amstrong units}
    \item{Xray}{result of X-ray examinnation (0=negative, 1=positive)}
    \item{Tsize}{size of tumour by rectal examination (0=small, 1=large)}
    \item{Tgrade}{summary of pathological grade of tumour from biopsy (0=less serious, 1=serious)}
    \item{nodal}{involvement of lymph node (0=no, 1=yes)}
  } 
}
\details{
For 53 men with prostatic cancer it was determined via  a surgical
incision  into the abdominal cavity (laparotomy), whether the lymph nodes 
were effected by the cancer. Several prognostic variables were collected at the same time.
}

\source{
Brown(1980)
}

\references{
Collet, D. (1991) Modelling Binary Data, Chapman & Hall, London, 
     Example 1.9, p. 11
}
\examples{
data(prostaticCancer)
#
library(MASS)
#
# plot of nodal against acid
# and of smoothed relation
# simple smooth: average of nodal formintervals of acid
# loess smooth

data(prostaticCancer)
# plotting a 'simple smoothing
nodal<-prostaticCancer$nodal
acid<-prostaticCancer$acid
plot(acid,nodal,xlab='acid',ylab='nodal indicator',
main='nodal involvment: simple and loess smooth',pch=1,col=1)
cu<-seq(min(acid)-0.0001,max(acid)+0.001,l=10)
a<-cut(acid,cu,left.include=T)
e<-tapply(nodal,a,mean)
cu.oben<-cu[-1]
cu.unten<-cu[-length(cu)]
cu.mitte<-(cu.oben+cu.unten)/2
lines(cu.mitte,e,col=2,type='b',pch=16)
abline(v=cu)
k<-loess(nodal~acid,span=0.9)
lines(sort(acid),k$fit[order(acid)],lwd=2,pch=17,type='b',col=3)

se<-predict(k,se=TRUE)$se.fit
lines(sort(acid),(k$fit+2*se)[order(acid)],lwd=1,col=3)
lines(sort(acid),(k$fit-2*se)[order(acid)],lwd=1,col=3)


legend(1.2,0.8, legend=c('nodal indicator','average per acid interval',
                     'loess smooth'),pch=c(1,16,17),lwd=c(-1,1,2),
                          lty=c(0,1,1),col=c(1,2,3))



# logistic regression
logacid=log(acid)
g<-glm(nodal~ age + logacid*Xray + logacid*Tsize + logacid*Tgrade +
                        Xray*Tsize + Xray*Tgrade 
                        + Tsize*Tgrade, data=prostaticCancer,
                          family=binomial(link=logit))
#analysis of deviance
print(anova(g,test='Chisq'))



\dontrun{
* SAS example;
data prostaticCancer; 
infile "prostaticCancer.txt" firstobs=2;
input  age acid Xray Tsize  Tgrade  nodal;
proc print; run;
} 

}
\keyword{datasets}
