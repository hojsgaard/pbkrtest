###################################################
### chunk number 1: 
###################################################
f <- list.files("../SHtools/R",pattern="\\.R",full.names=T)
f <- f[(1:length(f))[-grep("~",f)]]
sapply(f,source)
library(geepack)


###################################################
### chunk number 2: 
###################################################
options(width=70,prompt='>',continue=' ')


###################################################
### chunk number 3: 
###################################################
x <- 1:10
x


###################################################
### chunk number 4: 
###################################################
library(doBy)
data(dietox)
dietox[1:5,]


###################################################
### chunk number 5: 
###################################################
dietox$Cu     <- as.factor(dietox$Cu)


###################################################
### chunk number 6: 
###################################################
dietox <- read.csv("dietox.csv")


###################################################
### chunk number 7: 
###################################################
par(mfrow=c(1,3))


###################################################
### chunk number 8: 
###################################################
plotBy(Weight~Time,subject=Pig,group=Cu,title="Cu=", data=dietox,col=1:100,lines=T)


###################################################
### chunk number 9: 
###################################################
m.dietox <- summaryBy(Weight~Cu+Time, data=dietox, FUN=c(mean,var))
m.dietox[1:5,]


###################################################
### chunk number 10: 
###################################################
par(mfrow=c(1,1))
plotBy(mean.Weight~Time,subject=Cu, data=m.dietox, lines=T,
col=c("black","red","green"), silent=F)


###################################################
### chunk number 11: 
###################################################
lm1 <- lm(mean.Weight~Cu*Time, data=m.dietox)
lm2 <- lm(mean.Weight~Cu*(Time+I(Time^2)), data=m.dietox)
lm3 <- lm(mean.Weight~Cu*(Time+I(Time^2)+I(Time^3)), data=m.dietox)


###################################################
### chunk number 12: residplots01
###################################################
 par(mfrow=c(1,3))
 plotBy(resid(lm1)~Time, subject=Cu, data=m.dietox,lines=T,col=1:3)
 plotBy(resid(lm2)~Time, subject=Cu, data=m.dietox,lines=T,col=1:3)
 plotBy(resid(lm3)~Time, subject=Cu, data=m.dietox,lines=T,col=1:3)


###################################################
### chunk number 13: residplots02
###################################################
mf <- formula(Weight~Cu*(Time+I(Time^2)+I(Time^3)))
lm4 <- lm(mf, data=dietox)
plotBy(resid(lm4)~Time, subject=Pig, data=dietox,lines=T,col=1:3)


###################################################
### chunk number 14: 
###################################################
plot(log(var.Weight)~log(mean.Weight), data=m.dietox)
l <- lm(log(var.Weight)~log(mean.Weight), data=m.dietox)
abline(l,lwd=2,col="red")
l


###################################################
### chunk number 15: varmeanplots01
###################################################
plot(log(fitted(lm4)),log(resid(lm4)^2))
l <- lm(log(resid(lm4)^2)~log(fitted(lm4)))
abline(l, col="red")
l


###################################################
### chunk number 16: 
###################################################
gee1 <- geeglm(mf, data=dietox, id=Pig, family=poisson("identity"),corstr="ar1")


###################################################
### chunk number 17: 
###################################################
qpo1 <-glm(mf, data=dietox, family=quasipoisson("identity"))


###################################################
### chunk number 18: 
###################################################
sgee1 <- summary(gee1)
sqpo1 <- summary(qpo1)
slm4 <- summary(lm4)
Egee <- sgee1$coef[,1]
Eqpo <- sqpo1$coef[,1]
Elm <- slm4$coef[,1]
SEgee <- sgee1$coef[,2]
SEqpo <- sqpo1$coef[,2]
SElm <- slm4$coef[,2]
Rgee  <-  sgee1$coef[,2]/slm4$coef[,2]
Rqpo <- sqpo1$coef[,2]/slm4$coef[,2]

round(cbind(Egee,Eqpo,Elm,SEgee,SEqpo,SElm,Rgee,Rqpo),3)


###################################################
### chunk number 19: 
###################################################
anovalm4  <- anova(lm4)
anovaqpo1 <- anova(qpo1,test="F")
anovagee1 <- anova(gee1)


###################################################
### chunk number 20: 
###################################################
anovalm4
anovaqpo1
anovagee1


###################################################
### chunk number 21: 
###################################################
gee2 <- update(gee1,.~.-Cu:I(Time^3))
summary(gee2)


###################################################
### chunk number 22: 
###################################################
dnew <- data.frame(Time=c(7,7),Cu=as.factor(c(2,3)))
predict(gee2,dnew)


###################################################
### chunk number 23: 
###################################################
L.Cu2 <- c(1,1,0,7,7^2,7^3,7,0,7^2,0)
L.Cu3 <- c(1,0,1,7,7^2,7^3,0,7,0,7^2)
L.Cu2
L.Cu3


###################################################
### chunk number 24: 
###################################################
sum(gee2$coef * L.Cu2)
sum(gee2$coef * L.Cu3)
sum(gee2$coef * (L.Cu3-L.Cu2))


###################################################
### chunk number 25: 
###################################################
L <- rbind(L.Cu2,L.Cu3,diff=L.Cu3-L.Cu2)
L
esticon(gee2, L)


###################################################
### chunk number 26: 
###################################################
lsmean(gee2)


###################################################
### chunk number 27: 
###################################################
dietox2 <- dietox
dietox2$Time2 <- dietox$Time^2
dietox2$Time3 <- dietox$Time^3

gee3 <- geeglm(Weight~Cu*Time+Cu*Time2+Time3, data=dietox2, id=Pig, family=poisson("identity"),corstr="ar1")
lsmean(gee3)


###################################################
### chunk number 28: 
###################################################
mT <- mean(dietox$Time)
mT2 <- mean(dietox$Time^2)
mT3 <- mean(dietox$Time^3)

L1 <- c(1,0,0,mT,mT2,mT3,0,0,0,0)
L2 <- c(1,1,0,mT,mT2,mT3,mT,0,mT2,0)
L3 <- c(1,0,1,mT,mT2,mT3,0,mT,0,mT2)
esticon(gee2,rbind(L1,L2,L3))


###################################################
### chunk number 29: 
###################################################
d <- unique(dietox$Pig)
v1 <- v2 <- v3 <- NULL
for (i in 1:length(d)){
 print(c(i,d[i]))
 dsub <- subset(dietox,Pig!=d[i])
 gee1 <- geeglm(mf, data=dsub, id=Pig, family=poisson("identity"),corstr="ar1")
 qpo1 <-glm(mf, data=dsub, family=quasipoisson("identity"))
 lm4 <- lm(mf, data=dsub)
 v1 <- rbind(v1,summary(gee1)$coef[,1])
 v2 <- rbind(v2,summary(qpo1)$coef[,1])
 v3 <- rbind(v3,summary(lm4)$coef[,1]) 
 }
SEgeej <- sqrt(apply(v1,2,var)*(nrow(v1)-1))
SEqpoj <- sqrt(apply(v2,2,var)*(nrow(v2)-1))
SElmj <- sqrt(apply(v3,2,var)*(nrow(v3)-1))


###################################################
### chunk number 30: 
###################################################
round(cbind(SEqpoj,SEgeej,SElmj,SEgee,SEqpo, SElm),3)


###################################################
### chunk number 31: 
###################################################
options(width=140,prompt='> ')


