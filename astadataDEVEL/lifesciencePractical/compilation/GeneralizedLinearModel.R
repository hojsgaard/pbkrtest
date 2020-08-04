### R code from vignette source 'GeneralizedLinearModel.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: GeneralizedLinearModel.Rnw:135-136
###################################################
options("width"=80, "prompt"=" ", continue="  ")


###################################################
### code chunk number 2: GeneralizedLinearModel.Rnw:140-146
###################################################
xyplot2 <- function (x, data, ...)
{
    cl <- match.call()
    cl[[1]] <- as.name("xyplot")
    print(eval(cl))
}


###################################################
### code chunk number 3: GeneralizedLinearModel.Rnw:201-203
###################################################
library(LiSciData)
data(shuttleOrings)


###################################################
### code chunk number 4: GeneralizedLinearModel.Rnw:307-312
###################################################
set.seed(148)
p.boy<-0.55
p.girl<-1-p.boy
a<-rbinom(1,size=10,prob=p.boy)
b<-rmultinom(1,size=17,prob=c(p.girl^2,2*p.girl*p.boy,p.boy^2))


###################################################
### code chunk number 5: GeneralizedLinearModel.Rnw:324-325
###################################################
dboys<-data.frame(boys=c(0,0,1,1,2),girls=c(1,2,0,1,0),nfam=c(9,2,1,10,5))


###################################################
### code chunk number 6: GeneralizedLinearModel.Rnw:329-330
###################################################
m<-glm(cbind(boys,girls)~1,data=dboys,family=binomial,weights=nfam)


###################################################
### code chunk number 7: GeneralizedLinearModel.Rnw:336-348
###################################################
explode<-function(dat,number){
#explodes data by copying each row in data with the entry i  number
# number is a column of dat!
numb<-dat[,number]
dat<-dat[! (is.na(numb) | numb==0),]
numb<-dat[,number]
dat$inde<-1:nrow(dat)
numb<-dat[,number]
indec<-data.frame(inde=rep(dat$inde,numb))
datexploded<-merge(dat,indec,by='inde')
}
dboysE<-explode(dboys,'nfam')


###################################################
### code chunk number 8: GeneralizedLinearModel.Rnw:351-352
###################################################
m.a<-glm(cbind(boys,girls)~1,data=dboysE,family=binomial)


###################################################
### code chunk number 9: GeneralizedLinearModel.Rnw:361-363
###################################################
library(MASS)
data(birthwt)


###################################################
### code chunk number 10: GeneralizedLinearModel.Rnw:367-368 (eval = FALSE)
###################################################
## help(birthwt)


###################################################
### code chunk number 11: GeneralizedLinearModel.Rnw:374-375
###################################################
example(birthwt)


###################################################
### code chunk number 12: GeneralizedLinearModel.Rnw:398-399
###################################################
data(carrotfly,package='LiSciData')


###################################################
### code chunk number 13: GeneralizedLinearModel.Rnw:415-417
###################################################
library(LiSciData)
example(carrotfly)


###################################################
### code chunk number 14: abel
###################################################
carrot<-transform(carrot,replicate=factor(replicate))


###################################################
### code chunk number 15: GeneralizedLinearModel.Rnw:433-434
###################################################
carrot<-transform(carrot,alltreat=interaction(depth,insecticide,drop=T))


###################################################
### code chunk number 16: GeneralizedLinearModel.Rnw:443-444
###################################################
g<-glm(cbind(dam,exam-dam)~replicate+alltreat,data=carrot,family=binomial)


###################################################
### code chunk number 17: GeneralizedLinearModel.Rnw:450-452
###################################################
X2<-sum(residuals(g,type='pearson')^2)/g$df.residual
X2


###################################################
### code chunk number 18: GeneralizedLinearModel.Rnw:458-460
###################################################
g.quasi<-glm(cbind(dam,exam-dam)~replicate+alltreat,data=carrot,family=quasibinomial)
anova(g.quasi,test='F')


###################################################
### code chunk number 19: GeneralizedLinearModel.Rnw:463-465
###################################################
g.quasi.0<-glm(cbind(dam,exam-dam)~replicate,data=carrot,family=quasibinomial)
anova(g.quasi,g.quasi.0,test='F')


###################################################
### code chunk number 20: GeneralizedLinearModel.Rnw:488-489
###################################################
coef(g.quasi)


###################################################
### code chunk number 21: GeneralizedLinearModel.Rnw:496-503
###################################################
lamb1<-lamb2<-lamb5<-lamb10<-lamb25<-0*coef(g.quasi)
lamb1[c('alltreat1.diazinon','alltreat1.disulfoton')]<-c(-1,1)
lamb2[c('alltreat2.5.diazinon','alltreat2.5.disulfoton')]<-c(-1,1)
lamb5[c('alltreat5.diazinon','alltreat5.disulfoton')]<-c(-1,1)
lamb10[c('alltreat10.diazinon','alltreat10.disulfoton')]<-c(-1,1)
lamb25[c('alltreat25.diazinon','alltreat25.disulfoton')]<-c(-1,1)
lamb<-rbind(lamb1,lamb2,lamb5,lamb10,lamb25)


###################################################
### code chunk number 22: GeneralizedLinearModel.Rnw:506-507
###################################################
esticon(g.quasi,cm=lamb,joint.test=T)


###################################################
### code chunk number 23: GeneralizedLinearModel.Rnw:526-527
###################################################
data(esoph)


###################################################
### code chunk number 24: GeneralizedLinearModel.Rnw:557-558
###################################################
data(discoveries)


###################################################
### code chunk number 25: GeneralizedLinearModel.Rnw:581-583
###################################################
library(LiSciData)
data(bactsucrose)


###################################################
### code chunk number 26: GeneralizedLinearModel.Rnw:618-632
###################################################
bactsucrose<-transform(bactsucrose,y=density/max(density),day=factor(day),sucrose=factor(sucrose),leucine=factor(leucine))
library(MASS)
boxcox(y~day+sucrose+leucine+sucrose:leucine,data=bactsucrose)
m.normal.log<- glm(density~day+sucrose+leucine+sucrose:leucine,data=bactsucrose,family=gaussian(link=log))
res<-residuals(m.normal.log,type='response')
fit<-predict(m.normal.log,type='response')
g<-glm(log(abs(res))~log(fit))
m.normal.log<- glm(density~day+sucrose+leucine+sucrose:leucine,data=bactsucrose,family=gaussian(link=log))
m.gamma.log<-glm(density~day+sucrose+leucine+sucrose:leucine,data=bactsucrose,family=Gamma(link=log))
library(boot)
AIC(m.normal.log)
AIC(m.gamma.log)
cv.glm(bactsucrose,m.normal.log)$delta
cv.glm(bactsucrose,m.gamma.log)$delta


###################################################
### code chunk number 27: GeneralizedLinearModel.Rnw:663-664
###################################################
pop<-rep(c(10,100,1000),each=c(1,1,1))


###################################################
### code chunk number 28: GeneralizedLinearModel.Rnw:672-673
###################################################
b<-rpois(length(pop),lambda=0.03*pop)


###################################################
### code chunk number 29: GeneralizedLinearModel.Rnw:676-681
###################################################
set.seed(89)
b<-matrix(NA,length(pop),1000)
for (i in 1:1000) {
b[,i]<-rpois(length(pop),lambda=0.03*pop)
}


###################################################
### code chunk number 30: GeneralizedLinearModel.Rnw:684-685
###################################################
b<-b/pop


###################################################
### code chunk number 31: GeneralizedLinearModel.Rnw:688-692
###################################################
par(mfrow=c(2,2))
 hist(b[1,],xlim=c(0,0.25),ylim=c(0,65),main='village',probability=TRUE)
 hist(b[2,],xlim=c(0,0.25),ylim=c(0,65),main='town',probability=TRUE)
 hist(b[3,],xlim=c(0,0.25),ylim=c(0,65),main='large city',probability=TRUE)	


###################################################
### code chunk number 32: GeneralizedLinearModel.Rnw:714-719
###################################################
v<-get(data(poissonviolence,package='LiSciData'))
v$pop<-with(v,number/ratio*1000)
is.amt<-grep('Amt',v$kommune)
v<-v[-is.amt,]
v<-subset(v, !(kommune %in% c('Hele_landet')))


###################################################
### code chunk number 33: GeneralizedLinearModel.Rnw:744-749
###################################################
M0<-glm(number~offset(log(pop)),data=v,family=poisson)
X2pearson<-function(m) sum(residuals(m,type='pearson')^2)/m$df.residual
X2pearson(M0)
M1<-glm(number~offset(log(pop)),data=v,family=quasipoisson)
M2<-glm(number~log(pop)+offset(log(pop)),data=v,family=quasipoisson)


###################################################
### code chunk number 34: GeneralizedLinearModel.Rnw:803-804
###################################################
x<-seq(0,20,l=50)


###################################################
### code chunk number 35: GeneralizedLinearModel.Rnw:806-807
###################################################
plot(x,dexp(x,rate=1/4.7),type='l')


###################################################
### code chunk number 36: GeneralizedLinearModel.Rnw:842-845
###################################################
library(LiSciData)
data(whitebloodcell)
g<-glm(time~logcount,data=whitebloodcell,family=Gamma(link=log))


###################################################
### code chunk number 37: GeneralizedLinearModel.Rnw:852-853
###################################################
summary(g,dispersion=1)$coef


###################################################
### code chunk number 38: GeneralizedLinearModel.Rnw:864-865
###################################################
predict(g,newdata=data.frame(logcount=4.5),type='response')


###################################################
### code chunk number 39: GeneralizedLinearModel.Rnw:876-877
###################################################
e<-predict(g,newdata=data.frame(logcount=4.5),dispersion=1,se=TRUE)


###################################################
### code chunk number 40: GeneralizedLinearModel.Rnw:881-883
###################################################
ci.log<-e$fit+ 1.96* e$se.fit *c(-1,1)
ci<-exp(ci.log)


###################################################
### code chunk number 41: GeneralizedLinearModel.Rnw:888-891
###################################################
e<-predict(g,newdata=data.frame(logcount=4.5),se=TRUE)
ci.log<-e$fit+ 1.96* e$se.fit *c(-1,1)
ci<-exp(ci.log)


###################################################
### code chunk number 42: GeneralizedLinearModel.Rnw:893-894
###################################################
ci<-exp(ci.log)


###################################################
### code chunk number 43: GeneralizedLinearModel.Rnw:907-908
###################################################
data(motorins,package='faraway')


###################################################
### code chunk number 44: GeneralizedLinearModel.Rnw:921-924
###################################################
g.quasi<-glm(cbind(dam,exam-dam)~replicate+alltreat,data=carrot,family=quasibinomial)
carrot$p<-carrot$dam/carrot$exam
g<-glm(log(p/(1-p))~replicate+alltreat,data=carrot,family=gaussian)


###################################################
### code chunk number 45: GeneralizedLinearModel.Rnw:939-940
###################################################
g.lognormal<-lm(log(time)~logcount,data=whitebloodcell)


###################################################
### code chunk number 46: GeneralizedLinearModel.Rnw:955-958
###################################################
exp(
predict(g.lognormal,newdata=data.frame(logcount=4.5))+
0.5*summary(g.lognormal)$sigma^2)


###################################################
### code chunk number 47: GeneralizedLinearModel.Rnw:974-976
###################################################
library(LiSciData)
data(waistloss)


###################################################
### code chunk number 48: GeneralizedLinearModel.Rnw:983-988
###################################################
library(lattice)
waistL<-reshape(waistloss,direction='long',varying=list(c('before','after')),v.names='weight',time=c('before','after'),
timevar='time')
waistL$timenum<-with(waistL,ifelse(time=='before',-1,1))
xyplot(weight~timenum,groups=id,type='l',data=waistL)


###################################################
### code chunk number 49: GeneralizedLinearModel.Rnw:1005-1011
###################################################
library(geepack)
waistL<-with(waistL,waistL[order(id,time),])
m.un<-glm(weight~time,data=waistL)
m.gee<-geeglm(weight~time,data=waistL,id=id,corstr='exchangeable')
waistloss$diff<-with(waistloss,before-after)
m.diff<-glm(diff~1,data=waistloss)


###################################################
### code chunk number 50: GeneralizedLinearModel.Rnw:1069-1070
###################################################
data(soreThroat,package='LiSciData')


###################################################
### code chunk number 51: GeneralizedLinearModel.Rnw:1102-1104
###################################################
v<-get(data(plasmaRetinol,package='LiSciData'))
v<-subset(v,betaplasma>0)


###################################################
### code chunk number 52: GeneralizedLinearModel.Rnw:1107-1111
###################################################
v<-transform(v,sex=factor(sex,labels=c('M','F')),
smokstat=factor(smokstat,labels=c('nev','for','cur')),
vituse=factor(vituse))
names(v)[names(v)=='beteadiet']<-'betadiet'


###################################################
### code chunk number 53: hunde
###################################################
m.normal.id<-glm(betaplasma~vituse+sex +age + kcal+fat+fiber+alcohol+cholesterol+betadiet,data=v)	
#
m.normal.log<-glm(betaplasma~vituse+sex +age + kcal+fat+fiber+alcohol+cholesterol+betadiet,data=v,family=gaussian(link=log))	
#
m.gamma.log<-glm(betaplasma~vituse+sex +age + kcal+fat+fiber+I(fiber^2)+alcohol+cholesterol+betadiet,data=v,family=Gamma(link=log))


###################################################
### code chunk number 54: hammer
###################################################
library(mgcv)
mm<-gam(betaplasma~vituse+sex +s(age) + s(kcal)+s(fat)+s(fiber)+s(alcohol)+
s(cholesterol)+s(betadiet),
       data=v)	


###################################################
### code chunk number 55: rassel
###################################################
data(fruitfly,package='faraway')
sapply(fruitfly,class)


###################################################
### code chunk number 56: blatt
###################################################
v<-get(data(bactsucrose,package='LiSciData'))
v$y<-with(v,density/quantile(density,0.9))


###################################################
### code chunk number 57: GeneralizedLinearModel.Rnw:1216-1218
###################################################
m.gamma.log<-glm(density~day+sucrose+leucine,data=v,family=Gamma(link=log))
m.gamma.id<-glm(density~day+sucrose+leucine,data=v,family=Gamma(link=identity))


###################################################
### code chunk number 58: ss
###################################################
library(boot)
extractAIC(m.gamma.id)
extractAIC(m.gamma.log)
cv.glm(v,m.gamma.id)$delta
cv.glm(v,m.gamma.log)$delta


###################################################
### code chunk number 59: umte
###################################################
library(LiSciData)
data(hodkin)
v<-reshape(hodkin,direction='long',varying=list(c('hodk','nonhodk')),v.names='number',timevar='hodkin',times=c('yes','non'))
v$hodkin<-factor(v$hodkin)


###################################################
### code chunk number 60: GeneralizedLinearModel.Rnw:1248-1250
###################################################
m<-glm(number~hodkin,data=v,family=poisson)
m.ov<-glm(number~hodkin,data=v,family=quasipoisson)


###################################################
### code chunk number 61: GeneralizedLinearModel.Rnw:1255-1257
###################################################
library(MASS)
m.neg<-glm.nb(number~hodkin,data=v)


