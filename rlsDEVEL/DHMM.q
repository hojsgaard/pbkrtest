###################################################
### chunk number 1: 
###################################################
options("width"=80)


###################################################
### chunk number 2: 
###################################################


load("clsX.RData")
length(cls0) # True states
length(cls1) # Observed states
library(mhsmm)


###################################################
### chunk number 3: clsfig1
###################################################
plot(cls1[1:2000], type='l', ylim=c(.8,2))
addStates(cls0[1:2000])


###################################################
### chunk number 4: 
###################################################
dpmf <- function(x,j,model) model$parms.emission$pmf[j,x]


###################################################
### chunk number 5: 
###################################################
J <- 2
init <- c(.5,.5)
P <- matrix(c(.9,.1,.1,.9),nrow=J)
B <- list(pmf=matrix(.1,ncol=J,nrow=J))
diag(B$pmf) <- .9
init.spec <- hmmspec(init,trans=P,parms.emission=B,dens.emission=dpmf)
init.spec


###################################################
### chunk number 6: 
###################################################
mstep.pmf <- function(x,wt) {
  ans <- matrix(ncol=ncol(wt),nrow=ncol(wt))
  for(i in 1:ncol(wt))
    for(j in 1:ncol(wt))
      ans[i,j] <- sum(wt[which(x==j),i])/sum(wt[,i])
  list(pmf=ans)
}


###################################################
### chunk number 7: 
###################################################
samp <- 1:2640
train <- list(s=cls0[samp], x=cls1[samp], N=length(cls0[samp]))
valid <- list(x=cls1[-samp], N=length(cls1[-samp]))


###################################################
### chunk number 8: 
###################################################
hmm.obj <- hmmfit(train, init.spec,mstep=mstep.pmf)
summary(hmm.obj)


###################################################
### chunk number 9: 
###################################################
vit <- predict(hmm.obj, valid)


###################################################
### chunk number 10: 
###################################################
smo <- predict(hmm.obj, valid, method="smoothed")


###################################################
### chunk number 11: 
###################################################
normtab <- function(tt) round(sweep(tt,1,rowSums(tt),"/"),2)
SS <- cls0[-samp]
XX <- cls1[-samp]
cls2.vit <- vit$s 
cls2.smo <- smo$s 
normtab(table(SS,XX))
normtab(table(SS,cls2.vit))
normtab(table(SS,cls2.smo))


###################################################
### chunk number 12: clsfig2
###################################################
show <- 1:2000
cls0b <- cls0[-samp]
cls1b <- cls1[-samp]
c(length(SS),length(cls2.vit),length(cls2.smo), length(cls0b), length(cls1b))
plot(cls1b[show], type='l', ylim=c(.8,2))
addStates(list(cls0b[show],cls2.vit[show], cls2.smo[show]))
























