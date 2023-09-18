### R code from vignette source 'doBy-lsmeans-etc.Rnw'

###################################################
### code chunk number 1: doBy-lsmeans-etc.Rnw:27-29
###################################################
prettyVersion <- packageDescription("doBy")$Version
prettyDate <- format(Sys.Date(), "%b %d, %Y")


###################################################
### code chunk number 2: doBy-lsmeans-etc.Rnw:37-38
###################################################
options(prompt = "R> ", continue = "+  ", width = 80, useFancyQuotes = FALSE)


###################################################
### code chunk number 3: doBy-lsmeans-etc.Rnw:65-66
###################################################
library(doBy)


###################################################
### code chunk number 4: doBy-lsmeans-etc.Rnw:106-110
###################################################
data(airquality)
airquality <- transform(airquality, Month=factor(Month))
m <- lm(Ozone~Month*Wind, data=airquality)
coef(summary(m))


###################################################
### code chunk number 5: doBy-lsmeans-etc.Rnw:126-131
###################################################
Lambda <- rbind(
  c(0, -1,  0,  0,  0, 0, -10,   0,   0,   0),
  c(0,  1, -1,  0,  0, 0,  10, -10,   0,   0),
  c(0,  0,  1, -1,  0, 0,   0,  10, -10,   0),
  c(0,  0,  0,  1, -1, 0,   0,   0,  10, -10))


###################################################
### code chunk number 6: doBy-lsmeans-etc.Rnw:140-141
###################################################
esticon(m, Lambda)


###################################################
### code chunk number 7: doBy-lsmeans-etc.Rnw:150-155
###################################################
Lambda <- rbind(
  c(0,0,0,0,0,0,1,0,0,0),
  c(0,0,0,0,0,0,0,1,0,0),
  c(0,0,0,0,0,0,0,0,1,0),
  c(0,0,0,0,0,0,0,0,0,1))


###################################################
### code chunk number 8: doBy-lsmeans-etc.Rnw:160-161
###################################################
esticon(m, Lambda, joint.test=TRUE)


###################################################
### code chunk number 9: doBy-lsmeans-etc.Rnw:168-170
###################################################
m2 <- update(m, .~.-Month:Wind)
anova(m, m2)


###################################################
### code chunk number 10: doBy-lsmeans-etc.Rnw:192-198
###################################################
library(doBy)
dd <- expand.grid(A=factor(1:3),B=factor(1:3),C=factor(1:2))
dd$y <- rnorm(nrow(dd))
dd$x <- rnorm(nrow(dd))^2
dd$z <- rnorm(nrow(dd))
head(dd,10)


###################################################
### code chunk number 11: doBy-lsmeans-etc.Rnw:209-211
###################################################
mm <- lm(y~A+B+C, data=dd)
coef(mm)


###################################################
### code chunk number 12: doBy-lsmeans-etc.Rnw:264-267
###################################################
w <- c(1, 0, 0, 1/3, 1/3, 1/2)
coef(mm)*w
sum(coef(mm)*w)


###################################################
### code chunk number 13: doBy-lsmeans-etc.Rnw:273-276
###################################################
W <- matrix(c(1, 0, 0, 1/3, 1/3, 1/2,
              1, 1, 0, 1/3, 1/3, 1/2,
              1, 0, 1, 1/3, 1/3, 1/2),nr=3, byrow=TRUE)


###################################################
### code chunk number 14: doBy-lsmeans-etc.Rnw:290-291
###################################################
esticon(mm, W)


###################################################
### code chunk number 15: doBy-lsmeans-etc.Rnw:304-306
###################################################
pma <- popMatrix(mm,effect='A')
summary(pma)


###################################################
### code chunk number 16: doBy-lsmeans-etc.Rnw:316-318
###################################################
pme <- popMeans(mm, effect='A')
pme


###################################################
### code chunk number 17: doBy-lsmeans-etc.Rnw:324-325
###################################################
summary(pme)


###################################################
### code chunk number 18: doBy-lsmeans-etc.Rnw:331-332
###################################################
popMatrix(mm,effect=c('A','C'))


###################################################
### code chunk number 19: doBy-lsmeans-etc.Rnw:340-342
###################################################
popMatrix(mm)
popMeans(mm)


###################################################
### code chunk number 20: doBy-lsmeans-etc.Rnw:355-356
###################################################
popMatrix(mm, effect='A', at=list(C='1'))


###################################################
### code chunk number 21: doBy-lsmeans-etc.Rnw:362-363
###################################################
popMatrix(mm, effect='A', at=list(C=c('1','2')))


###################################################
### code chunk number 22: doBy-lsmeans-etc.Rnw:368-369
###################################################
popMatrix(mm, effect='A', at=list(C=c('1','2'), B='1'))


###################################################
### code chunk number 23: doBy-lsmeans-etc.Rnw:378-379
###################################################
popMatrix(mm, effect=c('A','C'), at=list(C='1'))


###################################################
### code chunk number 24: doBy-lsmeans-etc.Rnw:390-391
###################################################
popMatrix(mm, effect='A', at=list(C='1'))


###################################################
### code chunk number 25: doBy-lsmeans-etc.Rnw:398-400
###################################################
mm2 <- lm(y~A+B+C+C:x, data=dd)
coef(mm2)


###################################################
### code chunk number 26: doBy-lsmeans-etc.Rnw:404-405
###################################################
popMatrix(mm2,effect='A', at=list(C='1'))


###################################################
### code chunk number 27: doBy-lsmeans-etc.Rnw:412-413
###################################################
popMatrix(mm2,effect='A', at=list(C='1',x=12))


###################################################
### code chunk number 28: doBy-lsmeans-etc.Rnw:417-419
###################################################
mm22 <- lm(y~A+B+C+C:x+I(x^2), data=dd)
coef(mm22)


###################################################
### code chunk number 29: doBy-lsmeans-etc.Rnw:423-424
###################################################
popMatrix(mm22,effect='A', at=list(C='1'))


###################################################
### code chunk number 30: doBy-lsmeans-etc.Rnw:429-433
###################################################
dd <- transform(dd, x.sq=x^2)
mm23 <- lm(y~A+B+C+C:x+x.sq, data=dd)
coef(mm23)
popMatrix(mm23,effect='A', at=list(C='1'))


###################################################
### code chunk number 31: doBy-lsmeans-etc.Rnw:441-447
###################################################
mm3 <- lm(y~A+B+C+C:I(log(x)), data=dd)
coef(summary(mm3))

mm3 <- lm(y~A+B+C+C:I(log(x)), data=dd)
coef(summary(mm3))
popMatrix(mm3, effect='A', at=list(C='1'))


###################################################
### code chunk number 32: doBy-lsmeans-etc.Rnw:455-458
###################################################
dd <- transform(dd, log.x = log(x))
mm32 <- lm(y~A+B+C+C:log.x, data=dd)
popMatrix(mm32, effect='A', at=list(C='1'))


###################################################
### code chunk number 33: doBy-lsmeans-etc.Rnw:474-477
###################################################
 library(multcomp)
g<-popMeans(mm,effect='A', at=list(C='1'),engine="glht")
g


###################################################
### code chunk number 34: doBy-lsmeans-etc.Rnw:481-483
###################################################
summary(g,test=univariate())
confint(g,calpha=univariate_calpha())


###################################################
### code chunk number 35: doBy-lsmeans-etc.Rnw:488-490
###################################################
summary(g)
confint(g)


