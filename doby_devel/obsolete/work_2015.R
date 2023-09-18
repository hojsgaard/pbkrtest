load_all("doBy")
library(MASS)


M <- matrix(c(1,1,1,1,1,1,0,0,0,0,1,1), nrow=4)
null_basis(M)
Null(t(M))

M <- c(1,1,1,1)
null_basis(M)
Null(t(M))

m0 <- lm(breaks ~ wool + tension, data=warpbreaks)
null_basis(m0)
Null(t(model.matrix(m0)))


## Make balanced dataset
dat.bal   <- expand.grid(list(A=factor(1:2), B=factor(1:3), C=factor(1:3)))
dat.bal$y <- rnorm(nrow(dat.bal))

## Make unbalanced dataset: 'B' is nested within 'C' so B=1 is only
## found when C=1 and B=2,3 are found in each C=2,3,4
dat.nst <- dat.bal
dat.nst$C <-factor(c(1,1,2,2,2,2,1,1,3,3,3,3,1,1,4,4,4,4))
xtabs(y ~ C+B+A , data=dat.nst)

mod.bal  <- lm(y ~ A + B*C,    data=dat.bal)
mod.nst  <- lm(y ~ A + B*C, data=dat.nst)

null_basis( mod.bal )
null_basis( mod.nst )

null_basis( model.matrix(mod.bal) )
null_basis( model.matrix(mod.nst) )

Null( t(model.matrix(mod.bal)) )
Null( t(model.matrix(mod.nst)) )



K <- LSmatrix(mod.nst, effect=c("B","C"))
linest( mod.nst, K )




LSmeans(m0)

get_null_basis(m0)

nullBasis(m0)

nullBasis(c(1,1,1))

MASS::Null(m0)

m <- MASS::Null(t(model.matrix(m0)))

m1 <- lm(breaks ~ wool * tension, data=warpbreaks)

m <-  matrix(c(1,0,0,1,0,0),nr=2)

nullBasis

nullBasis(1:10)


m <- model.matrix(~wool+tension, data=warpbreaks)
nullBasis(m)

m <- lm(breaks~wool+tension, data=warpbreaks)
nullBasis(m)



library(doBy)

data(dietox)
dietox12    <- subset(dietox,Time==12)
     
summaryBy(Weight+Feed~Evit+Cu, data=dietox12, FUN=c(mean,var,length))
aggregate(cbind(Weight, Feed) ~ Evit+Cu, data=dietox12, FUN=function(x)c(mean(x),var(x),length(x)))

summaryBy(list(c("Weight","Feed"), c("Evit","Cu")), data=dietox12,
          FUN=c(mean,var,length))

f <- list(c("Weight","Feed"), c("Evit","Cu"))
form <- as.formula(paste0("cbind( ", toString( f[[1]] ), ") ~ ", paste(f[[2]], collapse="+")) )
aggregate(form, data=dietox12, FUN=function(x)c(mean(x),var(x),length(x)))

summaryBy(log(Weight)+Feed~Evit+Cu, data=dietox12)

aggregate(cbind(logW = log(Weight), Feed) ~ Evit+Cu, data=dietox12, FUN=mean)

summaryBy(.~Evit+Cu, data=dietox12,  id=~Litter, FUN=mean)

aggregate(.~Evit+Cu, data=dietox12,  id=~Litter, FUN=mean)

aggregate(.~1, data=dietox12,  id=~Litter, FUN=mean)



summaryBy(Weight+Feed~Evit+Cu+Time, data=subset(dietox,Time>1),
        FUN=c(mean,var,length))
     
     ## Calculations on transformed data:
     
     summaryBy(log(Weight)+Feed~Evit+Cu, data=dietox12)
     
     ## Calculations on all numerical variables (not mentioned elsewhere):
     
     summaryBy(.~Evit+Cu,                data=dietox12,
        id=~Litter, FUN=mean)
     
     ## There are missing values in the 'airquality' data, so we remove these
     ## before calculating mean and variance with 'na.rm=TRUE'. However the
     ## length function does not accept any such argument. Hence we get
     ## around this by defining our own summary function in which length is
     ## not supplied with this argument while mean and var are:
     
     sumfun <- function(x, ...){
       c(m=mean(x, ...), v=var(x, ...), l=length(x))
     }
     summaryBy(Ozone+Solar.R~Month, data=airquality, FUN=sumfun, na.rm=TRUE)
     
     ## Using '.' on the right hand side of a formula means to stratify by
     ## all variables not used elsewhere:
     
     data(warpbreaks)
     summaryBy(breaks ~ wool+tension, warpbreaks)
     summaryBy(breaks ~., warpbreaks)
     summaryBy(.~ wool+tension, warpbreaks)
     
     ## Keep the names of the variables (works only if FUN only returns one
     ## value):
     
     summaryBy(Ozone+Wind~Month, data=airquality,FUN=c(mean),na.rm=TRUE,
       keep.names=TRUE)
     
     ## Using full.dimension=TRUE
     
     ## Consider:
     summaryBy(breaks~wool, data=warpbreaks)
     ## Rows of result are replicated below
     summaryBy(breaks~wool, data=warpbreaks, full.dimension=TRUE)
     ## Notice: Previous result is effectively the same as
     with(warpbreaks, ave(breaks, wool))
     ## A possible application of full.dimension=TRUE is if we want to
     ## standardize (center and scale) data within groups:
     ss <- summaryBy(breaks~wool, data=warpbreaks, full.dimension=TRUE, FUN=c(mean,sd))
     (warpbreaks$breaks-ss$breaks.mean)/ss$breaks.sd



























data(potatoes)


load_all("doBy")

potatoes <- read.table("doBy/data/potatoes.txt", header=T)



require(lme4)
class(warp.mm)

warp.mm <- lmer(breaks ~ -1 + tension + (1|wool), data=warpbreaks)
 LSmeans(warp.mm, effect="tension")
 class(warp.mm)
 fixef(warp.mm)
 coef(summary(warp.mm))
 vcov(warp.mm)
 #if (requireNamespace("pbkrtest", quietly=TRUE ))
 #vcovAdj(warp.mm)

