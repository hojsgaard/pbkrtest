### R code from vignette source 'doBy-JSS.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: doBy-JSS.Rnw:85-88
###################################################
library("doBy")
library("xtable")
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)


###################################################
### code chunk number 2: src-doBy.Rnw:8-10
###################################################
dir.create("figures")
oopt <- options()


###################################################
### code chunk number 3: src-doBy.Rnw:26-27
###################################################
library(doBy)


###################################################
### code chunk number 4: src-doBy.Rnw:46-52
###################################################
data(CO2)
CO2 <- transform(CO2, Treat=Treatment, Treatment=NULL)
levels(CO2$Treat) <- c("nchil","chil")
levels(CO2$Type)  <- c("Que","Mis")
CO2 <- subset(CO2, Plant %in% c("Qn1", "Qc1", "Mn1", "Mc1"))
head(CO2)


###################################################
### code chunk number 5: src-doBy.Rnw:65-67
###################################################
airquality <- subset(airquality, Month %in% c(5,6))
head(airquality)


###################################################
### code chunk number 6: src-doBy.Rnw:95-97
###################################################
myfun1 <- function(x){c(m=mean(x), s=sd(x), sem=sd(x)/length(x))}
summaryBy(conc + uptake ~ Plant, data=CO2, FUN=myfun1)


###################################################
### code chunk number 7: src-doBy.Rnw:108-110
###################################################
myfun2 <- function(x){c(mean(x), var(x), sd(x)/length(x))}
summaryBy(conc + uptake ~ Plant, data=CO2, FUN=myfun2)


###################################################
### code chunk number 8: src-doBy.Rnw:121-123
###################################################
sem <- function(x){sd(x)/length(x)}
summaryBy(uptake~Plant, data=CO2, FUN=list(mean,sd,sem,N=length))


###################################################
### code chunk number 9: src-doBy.Rnw:164-165
###################################################
summaryBy(conc+uptake~Plant, data=CO2, FUN=myfun1, id=~Type+Treat)


###################################################
### code chunk number 10: src-doBy.Rnw:175-177
###################################################
summaryBy(log(uptake)+I(conc/uptake)+ conc+uptake~Plant, data=CO2,
FUN=myfun1)


###################################################
### code chunk number 11: src-doBy.Rnw:202-203
###################################################
summaryBy(log(uptake) + I(conc/uptake) + . ~Plant, data=CO2, FUN=myfun1)


###################################################
### code chunk number 12: src-doBy.Rnw:210-211
###################################################
summaryBy(log(uptake) ~ Plant + ., data=CO2, FUN=myfun1)


###################################################
### code chunk number 13: src-doBy.Rnw:218-219
###################################################
summaryBy(log(uptake) ~ 1, data=CO2, FUN=myfun1)


###################################################
### code chunk number 14: src-doBy.Rnw:232-234
###################################################
summaryBy(conc+uptake+log(uptake)~Plant,
data=CO2, FUN=mean, id=~Type+Treat, keep.names=TRUE)


###################################################
### code chunk number 15: src-doBy.Rnw:248-250
###################################################
x <- orderBy(~ -Temp + Month, data=airquality)
head(x)


###################################################
### code chunk number 16: src-doBy.Rnw:262-266
###################################################
airquality <- transform(airquality, wim=Day>15)

ss<-splitBy(~Month + wim, data=airquality)
ss


###################################################
### code chunk number 17: src-doBy.Rnw:272-273
###################################################
ss[['5|FALSE']]


###################################################
### code chunk number 18: src-doBy.Rnw:279-280
###################################################
attr(ss,"groupid")


###################################################
### code chunk number 19: src-doBy.Rnw:292-293
###################################################
subsetBy(~Month, subset=Wind > mean(Wind), data=airquality)


###################################################
### code chunk number 20: src-doBy.Rnw:306-309
###################################################
zz<- transformBy(~Month, data=airquality, minW=min(Wind), maxW=max(Wind),
     chg=sum(range(Wind)*c(-1,1)))
head(zz)


###################################################
### code chunk number 21: src-doBy.Rnw:325-329
###################################################
CO2 <- orderBy(~Plant + conc, data=CO2)
vv <- lapplyBy(~ Plant, data=CO2, 
               FUN = function(x){c(NA, diff(x$uptake) / diff(x$conc))})
str(vv)


###################################################
### code chunk number 22: src-doBy.Rnw:341-342
###################################################
sampleBy(~1, frac=0.5, data=airquality)


###################################################
### code chunk number 23: src-doBy.Rnw:348-349
###################################################
sampleBy(~Month, frac=0.2, data=airquality,systematic=T)


###################################################
### code chunk number 24: src-doBy.Rnw:406-409
###################################################
x <- c(1,1,1,2,2,2,1,1,1,3)
firstobs(x)
lastobs(x)


###################################################
### code chunk number 25: src-doBy.Rnw:414-416
###################################################
firstobs(~Plant, data=CO2)
lastobs(~Plant, data=CO2)


###################################################
### code chunk number 26: src-doBy.Rnw:425-428
###################################################
x <- c(1:4,0:5,11,NA,NA)
which.maxn(x,3)
which.minn(x,5)


###################################################
### code chunk number 27: src-doBy.Rnw:437-442
###################################################
x <- c(1,1,2,2,2,1,1,3,3,3,3,1,1,1)
subSeq(x)
subSeq(x, item=1)
subSeq(letters[x])
subSeq(letters[x],item="a")


###################################################
### code chunk number 28: src-doBy.Rnw:450-454
###################################################
x <- c("dec","jan","feb","mar","apr","may")
src1 <- list(c("dec","jan","feb"), c("mar","apr","may"))
tgt1 <- list("winter","spring")
recodeVar(x,src=src1,tgt=tgt1)


###################################################
### code chunk number 29: src-doBy.Rnw:461-463
###################################################
head(renameCol(CO2, 1:2, c("kk","ll")))
head(renameCol(CO2, c("Plant","Type"), c("kk","ll")))


###################################################
### code chunk number 30: src-doBy.Rnw:471-473
###################################################
yvar <- c(0,0,1,0,0,0,0,0,1,0,0,0,1,1,0,0,0)
(tvar <- seq_along(yvar) + c(0.1,0.2))


###################################################
### code chunk number 31: src-doBy.Rnw:482-483
###################################################
tse<- timeSinceEvent(yvar,tvar)


###################################################
### code chunk number 32: src-doBy.Rnw:525-526
###################################################
tse$tvar[tse$abs.tse<=1]


###################################################
### code chunk number 33: src-doBy.Rnw:533-536
###################################################
lynx <- as.numeric(lynx)
tvar <- 1821:1934
plot(tvar,lynx,type='l')


###################################################
### code chunk number 34: src-doBy.Rnw:542-545
###################################################
yyy <- lynx>mean(lynx)
head(yyy)
(sss <- subSeq(yyy,item=TRUE))


###################################################
### code chunk number 35: src-doBy.Rnw:549-551
###################################################
plot(tvar,lynx,type='l')
rug(tvar[sss$midpoint],col='blue',lwd=4)


###################################################
### code chunk number 36: src-doBy.Rnw:556-560
###################################################
yvar <- rep(0,length(lynx))
yvar[sss$midpoint] <- 1
tse <- timeSinceEvent(yvar,tvar)
head(tse,20)


###################################################
### code chunk number 37: src-doBy.Rnw:566-569
###################################################
len1 <- tapply(tse$ewin, tse$ewin, length)
len2 <- tapply(tse$run,  tse$run, length)
c(median(len1),median(len2),mean(len1),mean(len2))


###################################################
### code chunk number 38: src-doBy.Rnw:574-577
###################################################
tse$lynx <- lynx
tse2 <- na.omit(tse)
plot(lynx~tae, data=tse2)


###################################################
### code chunk number 39: src-doBy.Rnw:581-584
###################################################
plot(tvar,lynx,type='l',lty=2)
mm <- lm(lynx~tae+I(tae^2)+I(tae^3), data=tse2)
lines(fitted(mm)~tvar, data=tse2, col='red')


###################################################
### code chunk number 40: src-doBy.Rnw:597-602
###################################################
data(airquality)
airquality <- subset(airquality, Month %in% c(5,6,7))
airquality <- transform(airquality, Month=factor(Month))
m<-lm(Ozone ~ Month * Wind, data=airquality)
coefficients(m)


###################################################
### code chunk number 41: src-popMeans.Rnw:27-33
###################################################
library(doBy)
dd <- expand.grid(A=factor(1:3),B=factor(1:3),C=factor(1:2))
dd$y <- rnorm(nrow(dd))
dd$x <- rnorm(nrow(dd))^2
dd$z <- rnorm(nrow(dd))
head(dd,10)


###################################################
### code chunk number 42: src-popMeans.Rnw:44-46
###################################################
mm <- lm(y~A+B+C, data=dd)
coef(mm)


###################################################
### code chunk number 43: src-popMeans.Rnw:69-71
###################################################
w <- c(0,-1,1,0,0,0)
sum(coef(mm)*w)


###################################################
### code chunk number 44: src-popMeans.Rnw:77-78
###################################################
esticon(mm, w)


###################################################
### code chunk number 45: src-popMeans.Rnw:128-131
###################################################
w <- c(1, 0, 0, 1/3, 1/3, 1/2)
coef(mm)*w
sum(coef(mm)*w)


###################################################
### code chunk number 46: src-popMeans.Rnw:137-142
###################################################
W <- matrix(c(1, 0, 0, 1/3, 1/3, 1/2,
              1, 1, 0, 1/3, 1/3, 1/2,
              1, 0, 1, 1/3, 1/3, 1/2),nr=3, byrow=TRUE)
W
W %*% coef(mm)


###################################################
### code chunk number 47: src-popMeans.Rnw:157-158
###################################################
esticon(mm, W)


###################################################
### code chunk number 48: src-popMeans.Rnw:171-173
###################################################
pma <- popMatrix(mm,effect='A')
summary(pma)


###################################################
### code chunk number 49: src-popMeans.Rnw:181-183
###################################################
pme <- popMeans(mm, effect='A')
pme


###################################################
### code chunk number 50: src-popMeans.Rnw:189-190
###################################################
summary(pme)


###################################################
### code chunk number 51: src-popMeans.Rnw:200-201
###################################################
popMatrix(mm,effect=c('A','C'))


###################################################
### code chunk number 52: src-popMeans.Rnw:207-208
###################################################
popMeans(mm)


###################################################
### code chunk number 53: src-popMeans.Rnw:219-220
###################################################
popMatrix(mm,effect='A', at=list(C='1'))


###################################################
### code chunk number 54: src-popMeans.Rnw:225-226
###################################################
popMatrix(mm,effect='A', at=list(C=c('1','2')))


###################################################
### code chunk number 55: src-popMeans.Rnw:232-233
###################################################
popMatrix(mm,effect='A', at=list(C=c('1','2'), B='1'))


###################################################
### code chunk number 56: src-popMeans.Rnw:242-243
###################################################
popMatrix(mm,effect=c('A','C'), at=list(C='1'))


###################################################
### code chunk number 57: src-popMeans.Rnw:254-255 (eval = FALSE)
###################################################
## popMatrix(mm,effect='A', at=list(C='1'))


###################################################
### code chunk number 58: src-popMeans.Rnw:262-264
###################################################
mm2 <- lm(y~A+B+C+C:x, data=dd)
coef(mm2)


###################################################
### code chunk number 59: src-popMeans.Rnw:268-269
###################################################
popMatrix(mm2,effect='A', at=list(C='1'))


###################################################
### code chunk number 60: src-popMeans.Rnw:276-277
###################################################
popMatrix(mm2,effect='A', at=list(C='1',x=12))


###################################################
### code chunk number 61: src-popMeans.Rnw:285-287
###################################################
mm3 <- lm(y~A+B+C+C:log(x), data=dd)
coef(mm3)


###################################################
### code chunk number 62: src-popMeans.Rnw:295-298
###################################################
dd <- transform(dd, log.x = log(x))
mm3 <- lm(y~A+B+C+C:log.x, data=dd)
popMatrix(mm3,effect='A', at=list(C='1'))


###################################################
### code chunk number 63: src-popMeans.Rnw:314-317
###################################################
 library(multcomp)
g<-popMeans(mm,effect='A', at=list(C='1'),engine="glht")
g


###################################################
### code chunk number 64: src-popMeans.Rnw:321-323
###################################################
summary(g,test=univariate())
confint(g,calpha=univariate_calpha())


###################################################
### code chunk number 65: src-popMeans.Rnw:328-330
###################################################
summary(g)
confint(g)


