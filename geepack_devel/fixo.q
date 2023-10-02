## If one uses  corstr="fixed"
## and  cluster sizes are different one must construct
## the 'zcor'-vector for all clusters of size larger than 1
## from the fixed correlation matrix of a complete cluster
## it is then not necessary to use the 'wave' argument.

library(geepack)
data(seizure)
## Diggle, Liang, and Zeger (1994) pp166-168, compare Table 8.10 
seiz.l <- reshape(seizure,
                  varying=list(c("base","y1", "y2", "y3", "y4")),
                  v.names="y", times=0:4, direction="long") 
seiz.l <- seiz.l[order(seiz.l$id, seiz.l$time),] 
seiz.l$t <- ifelse(seiz.l$time == 0, 8, 2) 
seiz.l$x <- ifelse(seiz.l$time == 0, 0, 1)

## Construction of a  fixed correlation matrix 
cor.fixed <- matrix(c(1    , 0.5  , 0.25,  0.125, 0.125,
                      0.5  , 1    , 0.25,  0.125, 0.125,
                      0.25 , 0.25 , 1   ,  0.5  , 0.125,
                      0.125, 0.125, 0.5  , 1    , 0.125,
                      0.125, 0.125, 0.125, 0.125, 1     ), 5, 5)

## creating an unbalanced data set for cluster 1, 3, 58 with only 2 
## observations 
## and clusters 10, 15, 23 and 44 only 1 observation:
seiz<-subset(seiz.l,!( (id==1 | id==3 | id==58) & time>=2))
seiz<-subset(seiz,!( (id==10 | id==15 | id==23 | id==44) & time!=2))

## adding 1 to seiz$time so time starts with 1: 
seiz$time<-seiz$time+1

## The zcor-vector is constructed only for clusters 
## of size larger than 1 
## it is assumed that seiz$time is a vector of integer times, 
## with inital time 1 and time unit of 1.

zcor<-NULL
id<-unique(seiz$id)
for (i in  id) {
  tim<-seiz$time[seiz$id==i]
  if (length(tim)>1) {
    for (k in 1: (length(tim)-1)) {
      for (m in (k+1) : length(tim))  {
        vvv <- cor.fixed[tim[m],tim[k]]
        zcor<-c(zcor,vvv)
      }
    }}
}
length(zcor)



zcor <- deriveZcor(cor.fixed, id=seiz$id, waves=seiz$time)
length(zcor)


g1<- geeglm(y ~ offset(log(t)) + x + trt + x:trt, id = id,
            data = seiz, family = poisson,
            corstr = "fixed", zcor = zcor)

g2<- geeglm(y ~ offset(log(t))  + trt + x:trt, id = id,
            data = seiz, family = poisson,
            corstr = "fixed", zcor = zcor)




### check check: 
## the id are relabelled to check whehter the results are
## the same 


id<-seiz$id
u<-sample(1:59,size=59)
names(u)<-1:59
idnew<-u[id]
seiz$idnew<-idnew
seiz <- seiz[order(seiz$idnew, seiz$time),]
zcor<-NULL
id<-unique(seiz$id)
for (i in  id) {
  tim<-seiz$time[seiz$id==i]
  if (length(tim)>1) {
    for (k in 1: (length(tim)-1)) {
      for (m in (k+1) : length(tim))  {
        zcor<-c(zcor,cor.fixed[tim[m],tim[k]])
      }
    }}}

q1<- geeglm(y ~ offset(log(t)) + x + trt + x:trt, id = idnew,
            data = seiz, family = poisson,
            corstr = "fixed", zcor = zcor)

## you see there is a slight differene in etsimation of the SE's

coef(summary(g1))
coef(summary(q1))



##
## Simulated data
##

library(geepack)

timeorder <- rep(1:5, 6)
tvar      <- timeorder + rnorm(length(timeorder))
idvar <- rep(1:6, each=5)
uuu   <- rep(rnorm(6), each=5)
yvar  <- 1 + 2*tvar + uuu + rnorm(length(tvar))
simdat <- data.frame(idvar, timeorder, tvar, yvar)
sapply(f,source)

## Construction of a  fixed correlation matrix 
cor.fixed <- matrix(c(1    , 0.5  , 0.25,  0.125, 0.125,
                      0.5  , 1    , 0.25,  0.125, 0.125,
                      0.25 , 0.25 , 1   ,  0.5  , 0.125,
                      0.125, 0.125, 0.5  , 1    , 0.125,
                      0.125, 0.125, 0.125, 0.125, 1     ), 5, 5)

set.seed(123)
simdatPerm <- simdat[sample(nrow(simdat)),]
simdatPerm <- orderBy(~idvar, simdatPerm)

mod2 <- geeglm(yvar~tvar, id=idvar, data=simdatPerm, corstr="ar1")

wav <- simdatPerm$timeorder
mod3 <- geeglm(yvar~tvar, id=idvar, data=simdatPerm, corstr="ar1", waves=wav)

mod2 <- geeglm(yvar~tvar, id=idvar, data=simdat, waves=tvar)

sapply(f,source)
mod1 <- geeglm(yvar~tvar, id=idvar, data=simdat, corstr="fixed")

zcor <- fixed2Zcor(cor.fixed, id = simdat$idvar, waves=simdat$tvar)

mod1 <- geeglm(yvar~tvar, id=idvar, data=simdat, corstr="fixed", zcor=zcor)

