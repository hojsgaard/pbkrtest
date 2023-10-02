##
## Using smooth.discrete
##

library(mhsmm)
source("smooth.discrete-src.R")

dpmf <- function(x,j,model) {
   ret <- model$parms.emission$pmf[j,x]
   ret[is.na(ret)]=1
   ret
  }

mstep.pmf <- function(x,wt) {
   ans <- matrix(ncol=ncol(wt),nrow=ncol(wt))
   for(i in 1:ncol(wt))
     for(j in 1:ncol(wt))
       ans[i,j] <- sum(wt[which(x[!is.na(x)]==j),i])/sum(wt[!is.na(x),i])
   list(pmf=ans)
}

y1 <- c(1,1,1,1,2,1,1,1,1,2,1,1,1,2,1,1,1,1,1,2,2,2,2,1,2,2,2,1,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1)
y2 <- c(1,1,1,1,2,2,2,1,1,2,1,1,1,2,1,1,1,1,1,2,2,2,1,1,1,2,2,1,2,2,2)

y1 <- c(1,1,1,1,2,1,1,NA,1,2,1,1,2,1,1,1,1,1,2,2,2,2,1,2,2,2,1,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1)
y2 <- c(1,1,1,1,2,2,2,1,1,2,1,NA,1,2,1,1,1,1,1,2,2,2,1,1,1,2,2,1,2,2,2)

obj <- smooth.discrete(y1)
print(obj)
summary(obj)

predict(obj)
predict(obj,x=y2)
predict(obj,x=y2,method="smoothed")












Hi Jared,
 
I wanted to create a higher-level utility smooth.discrete() which should act a little like smooth.spline() or so: Basically, throw in some data, have the function choose "sensible" defaults and off we go. I do:
 
.createP <- function(Pvec, J){
  Pvec <- rep(Pvec, J)[1:J]
  
  PP <- lapply(Pvec, function(xx)
    rep((1-xx)/(J-1),J)
  )
  PP <- do.call(rbind,PP)
  diag(PP) <- Pvec
  PP
}
y <- c(1,1,1,1,2,1,NA,1,1,2,1,1,1,2,1,1,1,1,1,2,2,2,2,1,2,2,2,1,2,2,2)
Pvec <- c(.9,.8)
Bvec <- .9
init <- as.numeric(table(y))
init <- init/sum(init)
J    <- length(init)
P    <- .createP(Pvec,J)
B    <- list(pmf=.createP(Bvec,J))
init.spec <- hmmspec(init,trans=P,parms.emission=B,dens.emission=dpmf)
init.spec
train   <- list(s=y, x=y, N=length(y))
hmm.obj <- hmmfit(train, init.spec,mstep=mstep.pmf)
summary(hmm.obj)
vit <- predict(hmm.obj, train)
vit <- predict(hmm.obj, train,method="smoothed")

- And get this problem
===============
> vit <- predict(hmm.obj, train)
Error: NA/NaN/Inf in foreign function call (arg 3)
> vit <- predict(hmm.obj, train,method="smoothed")
Error in .estep.hmm(x, object) : 
  NA/NaN/Inf in foreign function call (arg 3)
 

Can that be fixed? I would say so, but I dont know how difficult it is going to be...

 

Cheers

Søren 





##
## SANDBOX
##

Pvec <- c(.9,.8)
Bvec <- .9

init <- as.numeric(table(y))
init <- init/sum(init)
J    <- length(init)
P    <- .createP(Pvec,J)
B    <- list(pmf=.createP(Bvec,J))

init.spec <- hmmspec(init,trans=P,parms.emission=B,dens.emission=dpmf)
init.spec
train   <- list(s=y, x=y, N=length(y))

hmm.obj <- hmmfit(train, init.spec,mstep=mstep.pmf)
summary(hmm.obj)

vit <- predict(hmm.obj, train)
vit <- predict(hmm.obj, train,method="smoothed")


