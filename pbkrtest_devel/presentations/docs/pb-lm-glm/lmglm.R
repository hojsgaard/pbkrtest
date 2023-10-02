### R code from vignette source 'lmglm.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: lmglm.Rnw:14-15
###################################################
library("pbkrtest")


###################################################
### code chunk number 2: src-lmglm.Rnw:10-17
###################################################
data(budworm, package='LiSciData')

library(lattice)
par(mfrow=c(1,2))
print(xyplot(ndead/20~log(dose), groups=sex, data=budworm,
             type="b", auto.key=T))



###################################################
### code chunk number 3: src-lmglm.Rnw:24-28
###################################################
lm1 <- lm(ndead/20~sex+log(dose), data=budworm) 
lm0 <- update(lm1,.~.-sex)
PBmodcomp(lm1, lm0, nsim=999)
anova(lm1,lm0)


###################################################
### code chunk number 4: src-lmglm.Rnw:36-39
###################################################
library(lattice)
print(xyplot(log((ndead+.5)/(20-ndead+.5))~log(dose), groups=sex, data=budworm,
             type="b", auto.key=T))


###################################################
### code chunk number 5: src-lmglm.Rnw:44-49
###################################################
budworm <- transform(budworm, logdose=log(dose))
lreg1 <- glm(cbind(ndead,ntotal-ndead)~sex+logdose,
        data=budworm, family=binomial(link=logit))
lreg0 <- update(lreg1,.~.-sex)
PBmodcomp(lreg1, lreg0, nsim=999)


