###################################################
### chunk number 1: 
###################################################
library(doBy)
data(dietox)
dietox[1:5,]


###################################################
### chunk number 2: 
###################################################
dietox$Cu     <- as.factor(dietox$Cu)


###################################################
### chunk number 3: 
###################################################
dietox <- read.csv("dietox.csv")


###################################################
### chunk number 4: 
###################################################
par(mfrow=c(1,3))


###################################################
### chunk number 5: 
###################################################
plotBy(Weight~Time,subject=Pig,group=Cu,title="Cu=", data=dietox,col=1:100,lines=T)


###################################################
### chunk number 6: 
###################################################
m.dietox <- summaryBy(Weight~Cu+Time, data=dietox, FUN=c(mean,var))
m.dietox[1:5,]


###################################################
### chunk number 7: 
###################################################
par(mfrow=c(1,1))
plotBy(mean.Weight~Time,subject=Cu, data=m.dietox, lines=T,
col=c("black","red","green"), silent=F)


###################################################
### chunk number 8: 
###################################################
plot(log(var.Weight)~log(mean.Weight), data=m.dietox)
lm1 <- lm(log(var.Weight)~log(mean.Weight), data=m.dietox)
abline(lm1,lwd=2,col="red")
lm1


###################################################
### chunk number 9: 
###################################################
fm0 <- lm (Weight ~ Time + Cu + Cu * Time, data = dietox)


###################################################
### chunk number 10: 
###################################################
par(mfrow=c(2,3))
plotBy(resid(fm0)~Time,subject=Pig,group=Cu, data=dietox, lines=T, col=1:100)
plotBy(fitted(fm0)~Time,subject=Pig,group=Cu, data=dietox, lines=T, col=1:3)


###################################################
### chunk number 11: 
###################################################
library(nlme)
fm1 <- lme(Weight ~ Time + Cu + Cu * Time, data = dietox, random=~1|Pig)


###################################################
### chunk number 12: 
###################################################
anova(fm1)


###################################################
### chunk number 13: 
###################################################
fm2 <- lme(Weight ~ Cu * Time, data = dietox, random = ~ 1 + Time| Pig)


###################################################
### chunk number 14: 
###################################################
anova(fm2)


