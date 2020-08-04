### R code from vignette source '2011-ISMLS-exam.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: 2011-ISMLS-exam.Rnw:135-136
###################################################
options("width"=80, "prompt"=" ", continue="  ")


###################################################
### code chunk number 2: 2011-ISMLS-exam.Rnw:140-146
###################################################
xyplot2 <- function (x, data, ...)
{
    cl <- match.call()
    cl[[1]] <- as.name("xyplot")
    print(eval(cl))
}


###################################################
### code chunk number 3: 2011-ISMLS-exam.Rnw:223-225
###################################################
library(doBy)
data(milkman)


###################################################
### code chunk number 4: 2011-ISMLS-exam.Rnw:230-231
###################################################
milkman$lactno <- factor(milkman$lactno)


###################################################
### code chunk number 5: 2011-ISMLS-exam.Rnw:252-254
###################################################
mm2 <- summaryBy(my~cowlact+dfc, data=milkman, FUN=sum, id=~race+lactno,keep=T)
head(mm2)


###################################################
### code chunk number 6: 2011-ISMLS-exam.Rnw:269-272
###################################################
ss  <- summaryBy(dfc~cowlact, data=mm2, FUN=max)
cc  <- subset(ss, dfc.max>=305, select=cowlact)$cowlact
mm3 <- subset(mm2, cowlact %in% cc & dfc<=305)


###################################################
### code chunk number 7: 2011-ISMLS-exam.Rnw:282-284
###################################################
xx <- summaryBy(cowlact~race+lactno, data=mm3, FUN=function(x){c(N=length(unique(x)))})
xtabs(cowlact.N~race+lactno, data=xx)


###################################################
### code chunk number 8: 2011-ISMLS-exam.Rnw:294-296
###################################################
mm4 <- summaryBy(my~cowlact, data=mm3, id=~race+lactno,keep=T, FUN=sum, na.rm=T)
head(mm4)


###################################################
### code chunk number 9: 2011-ISMLS-exam.Rnw:306-307
###################################################
print(qqmath(~my|race+lactno, data=mm4))


###################################################
### code chunk number 10: 2011-ISMLS-exam.Rnw:311-312
###################################################
print(bwplot(my~race:lactno, data=mm4))


###################################################
### code chunk number 11: 2011-ISMLS-exam.Rnw:316-317
###################################################
print(histogram(~my|race:lactno, data=mm4))


###################################################
### code chunk number 12: 2011-ISMLS-exam.Rnw:321-322
###################################################
print(densityplot(~my|race:lactno, data=mm4))


###################################################
### code chunk number 13: 2011-ISMLS-exam.Rnw:333-335
###################################################
mm5 <- summaryBy(my~race+lactno, data=mm4, FUN=c(mean,sd))
mm5


###################################################
### code chunk number 14: 2011-ISMLS-exam.Rnw:365-366
###################################################
print(xyplot(my.mean~lactno, groups=race, data=mm5, type='b'))


###################################################
### code chunk number 15: 2011-ISMLS-exam.Rnw:375-376
###################################################
lm1 <- lm(my~race*lactno, data=mm4)


###################################################
### code chunk number 16: 2011-ISMLS-exam.Rnw:380-382
###################################################
par(mfrow=c(2,2))
plot(lm1)


###################################################
### code chunk number 17: 2011-ISMLS-exam.Rnw:391-392
###################################################
anova(lm1)


###################################################
### code chunk number 18: 2011-ISMLS-exam.Rnw:399-401
###################################################
lm2 <- update(lm1, .~.-race:lactno)
summary(lm2)


###################################################
### code chunk number 19: 2011-ISMLS-exam.Rnw:412-419
###################################################
library(multcomp)
ddd1 <- glht(lm2, mcp(race='Tukey'))
summary(ddd1, test=univariate())
confint(ddd1)
ddd2 <- glht(lm2, mcp(lactno='Tukey'))
summary(ddd2, test=univariate())
confint(ddd2)


###################################################
### code chunk number 20: 2011-ISMLS-exam.Rnw:429-430
###################################################
popMeans(lm2, effect='race')


###################################################
### code chunk number 21: 2011-ISMLS-exam.Rnw:464-466
###################################################
lmb1 <- lmBy(log(my)~log(dfc)+dfc|cowlact, data=mm3)
lmb1


###################################################
### code chunk number 22: 2011-ISMLS-exam.Rnw:474-476
###################################################
head(coef(lmb1))
colnames(coef(lmb1))


###################################################
### code chunk number 23: 2011-ISMLS-exam.Rnw:484-486
###################################################
lmb1 <- lmBy(log(my)~log(dfc)+dfc|cowlact, data=mm3, id=~race+lactno)
lmb1


###################################################
### code chunk number 24: 2011-ISMLS-exam.Rnw:490-492
###################################################
head(coef(lmb1, augment=TRUE))
colnames(coef(lmb1, augment=TRUE))


###################################################
### code chunk number 25: 2011-ISMLS-exam.Rnw:511-513
###################################################
bb1 <- coef(lmb1, augment=TRUE)
bb1$t.max <- -bb1[,2]/bb1[,3]


###################################################
### code chunk number 26: 2011-ISMLS-exam.Rnw:569-571
###################################################
lme1 <- lmer(log(my)~race+log(dfc)+dfc+race:log(dfc)+race:dfc+(1|cowlact), data=mm3, 
             REML=FALSE)


###################################################
### code chunk number 27: 2011-ISMLS-exam.Rnw:575-577
###################################################
lme2 <- update(lme1, .~.-race)
anova(lme1, lme2)


###################################################
### code chunk number 28: 2011-ISMLS-exam.Rnw:641-642
###################################################
data(esoph)


