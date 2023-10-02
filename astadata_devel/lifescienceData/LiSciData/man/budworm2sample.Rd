\name{budworm2sample}
\alias{budworm2sample}


\non_function{}
\title{Effect of Insecticide on survivial of tobacco budworm}
\usage{data(budworm2sample)}
\description{
 number of killed budworm exposed to an insecticide 
}
\format{
  This data frame contains  2 rows and 4 columns:
  \describe{
    \item{sex:}{sex of the budworm}
    \item{dose:}{dose of the insecticide trans-cypermethrin in [\eqn{\mu}g]}
    \item{ndead:}{budworms killed in a trial}
    \item{ntotal:}{total number of budworms exposed per trial }
  }
}
\details{
mortality of the moth  tobacco budworm 'Heliothis virescens'
for dose 8 [\eqn{\mu}g] of the pyrethroid  trans-cypermethrin differentiated with
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
data(budworm2sample)
attach(budworm2sample)
print(budworm2sample)

#calculating Pearsons X2:
y<-ndead
phat<- sum(y)/sum(ntotal)
expected<-phat*ntotal
varianc<- ntotal*phat*(1-phat)
X2<- sum( (y-expected)^2/ varianc )

#p-value for testing  H_0:  p(male)=p(female)
p.val<-1-pchisq(X2,df=1)
#95% quantile of the chi-square distribution with 1 degree of freedom
x.95<-qchisq(0.95,df=1)


#Alternative way to calculate the Pearson's X2
# by 
#    fitting a glm model with p(male)=p(female)
#     X2= sum of squares of the pearson residuals for that model

 # defining the two-column matrix containing the
# number of events in the first rowm number of non-event in the other
yb<-cbind(y,ntotal-y)

 # the model woth p(male==p(female) is the model woth only 
 # the intercept term fitted:

g<-glm(yb~1,family=binomial)
X2.glm<-sum(residuals(g,type='pearson')^2)

print(c(X2,X2.glm))

    

}
\keyword{datasets}
