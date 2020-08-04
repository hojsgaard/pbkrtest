### R code from vignette source 'doBy-main.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: doBy-main.Rnw:29-31
###################################################
prettyVersion <- packageDescription("doBy")$Version
prettyDate <- format(Sys.Date(), "%b %d, %Y")


###################################################
### code chunk number 2: doBy-main.Rnw:57-61
###################################################
dir.create("figures")
oopt <- options()
options("digits"=4, "width"=80, "prompt"=" ", "continue"="  ")
options(useFancyQuotes="UTF-8")


###################################################
### code chunk number 3: doBy-main.Rnw:83-84
###################################################
library(doBy)


###################################################
### code chunk number 4: doBy-main.Rnw:101-106
###################################################
data(CO2)
CO2 <- transform(CO2, Treat=Treatment, Treatment=NULL)
levels(CO2$Treat) <- c("nchil","chil")
levels(CO2$Type)  <- c("Que","Mis")
CO2 <- subset(CO2, Plant %in% c("Qn1", "Qc1", "Mn1", "Mc1"))


###################################################
### code chunk number 5: doBy-main.Rnw:118-119
###################################################
airquality <- subset(airquality, Month %in% c(5,6))


###################################################
### code chunk number 6: doBy-main.Rnw:147-150
###################################################
myfun1 <- function(x){c(m=mean(x), v=var(x))}
summaryBy(conc+uptake~Plant, data=CO2,
FUN=myfun1)


###################################################
### code chunk number 7: doBy-main.Rnw:161-163
###################################################
myfun2 <- function(x){c(mean(x), var(x))}
summaryBy(conc+uptake~Plant, data=CO2,FUN=myfun2)


###################################################
### code chunk number 8: doBy-main.Rnw:175-176
###################################################
summaryBy(uptake~Plant, data=CO2, FUN=c(mean,var,median))


###################################################
### code chunk number 9: doBy-main.Rnw:181-183
###################################################
mymed <- function(x)c(med=median(x))
summaryBy(uptake~Plant, data=CO2, FUN=c(mean,var,mymed))


###################################################
### code chunk number 10: doBy-main.Rnw:226-227
###################################################
summaryBy(conc+uptake~Plant, data=CO2, FUN=myfun1, id=~Type+Treat)


###################################################
### code chunk number 11: doBy-main.Rnw:238-240
###################################################
summaryBy(log(uptake)+I(conc+uptake)+ conc+uptake~Plant, data=CO2,
FUN=myfun1)


###################################################
### code chunk number 12: doBy-main.Rnw:247-249
###################################################
summaryBy(log(uptake)+I(conc+uptake)~Plant, data=CO2, p2d=TRUE,
FUN=myfun1)


###################################################
### code chunk number 13: doBy-main.Rnw:266-268
###################################################
summaryBy(log(uptake)+I(conc+uptake)+. ~Plant, data=CO2,
FUN=myfun1)


###################################################
### code chunk number 14: doBy-main.Rnw:279-281
###################################################
summaryBy(log(uptake) ~Plant+., data=CO2,
FUN=myfun1)


###################################################
### code chunk number 15: doBy-main.Rnw:290-292
###################################################
summaryBy(log(uptake) ~ 1, data=CO2,
FUN=myfun1)


###################################################
### code chunk number 16: doBy-main.Rnw:304-306
###################################################
summaryBy(conc+uptake+log(uptake)~Plant,
data=CO2, FUN=mean, id=~Type+Treat, keep.names=TRUE)


###################################################
### code chunk number 17: doBy-main.Rnw:319-320
###################################################
x<-orderBy(~Temp+Month, data=airquality)


###################################################
### code chunk number 18: doBy-main.Rnw:324-325
###################################################
head(x)


###################################################
### code chunk number 19: doBy-main.Rnw:331-333
###################################################
x<-orderBy(~-Temp+Month, data=airquality)
head(x)


###################################################
### code chunk number 20: doBy-main.Rnw:344-346
###################################################
x<-splitBy(~Month, data=airquality)
x


###################################################
### code chunk number 21: doBy-main.Rnw:352-353
###################################################
x[['5']]


###################################################
### code chunk number 22: doBy-main.Rnw:358-359
###################################################
attr(x,"groupid")


###################################################
### code chunk number 23: doBy-main.Rnw:369-370
###################################################
sampleBy(~1, frac=0.5, data=airquality)


###################################################
### code chunk number 24: doBy-main.Rnw:376-377
###################################################
sampleBy(~Month, frac=0.2, data=airquality,systematic=T)


###################################################
### code chunk number 25: doBy-main.Rnw:388-389
###################################################
subsetBy(~Month, subset=Wind>mean(Wind), data=airquality)


###################################################
### code chunk number 26: doBy-main.Rnw:402-404
###################################################
transformBy(~Month, data=airquality, minW=min(Wind), maxW=max(Wind),
    chg=sum(range(Wind)*c(-1,1)))


###################################################
### code chunk number 27: doBy-main.Rnw:418-422
###################################################
data(dietox)
dietox <- orderBy(~Pig+Time, data=dietox)
v<-lapplyBy(~Pig, data=dietox, function(d) c(NA, diff(d$Weight)/diff(d$Feed)))
dietox$FE <- unlist(v)


###################################################
### code chunk number 28: doBy-main.Rnw:427-431
###################################################
dietox <- orderBy(~Pig+Time, data=dietox)
wdata <- splitBy(~Pig, data=dietox)
v <- lapply(wdata, function(d) c(NA, diff(d$Weight)/diff(d$Feed)))
dietox$FE <- unlist(v)


###################################################
### code chunk number 29: doBy-main.Rnw:567-570
###################################################
x <- c(1,1,1,2,2,2,1,1,1,3)
firstobs(x)
lastobs(x)


###################################################
### code chunk number 30: doBy-main.Rnw:575-577
###################################################
firstobs(~Plant, data=CO2)
lastobs(~Plant, data=CO2)


###################################################
### code chunk number 31: doBy-main.Rnw:586-589
###################################################
x <- c(1:4,0:5,11,NA,NA)
which.maxn(x,3)
which.minn(x,5)


###################################################
### code chunk number 32: doBy-main.Rnw:598-601
###################################################
x <- c(1,1,2,2,2,1,1,3,3,3,3,1,1,1)
subSeq(x)
subSeq(x, item=1)


###################################################
### code chunk number 33: doBy-main.Rnw:605-607
###################################################
subSeq(letters[x])
subSeq(letters[x],item="a")


###################################################
### code chunk number 34: doBy-main.Rnw:615-619
###################################################
x <- c("dec","jan","feb","mar","apr","may")
src1 <- list(c("dec","jan","feb"), c("mar","apr","may"))
tgt1 <- list("winter","spring")
recodeVar(x,src=src1,tgt=tgt1)


###################################################
### code chunk number 35: doBy-main.Rnw:626-628
###################################################
head(renameCol(CO2, 1:2, c("kk","ll")))
head(renameCol(CO2, c("Plant","Type"), c("kk","ll")))


###################################################
### code chunk number 36: doBy-main.Rnw:636-637
###################################################
yvar <- c(0,0,0,1,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0)


###################################################
### code chunk number 37: doBy-main.Rnw:644-645
###################################################
tvar <- seq_along(yvar) + c(0.1,0.2)


###################################################
### code chunk number 38: doBy-main.Rnw:650-651
###################################################
tse<- timeSinceEvent(yvar,tvar)


###################################################
### code chunk number 39: doBy-main.Rnw:666-671
###################################################
plot(sign.tse~tvar, data=tse, type="b")
grid()
rug(tse$tvar[tse$yvar==1], col='blue',lwd=4)
points(scale(tse$run), col=tse$run, lwd=2)
lines(abs.tse+.2~tvar, data=tse, type="b",col=3)


###################################################
### code chunk number 40: doBy-main.Rnw:675-680
###################################################
plot(tae~tvar, data=tse, ylim=c(-6,6),type="b")
grid()
lines(tbe~tvar, data=tse, type="b", col='red')
rug(tse$tvar[tse$yvar==1], col='blue',lwd=4)
lines(run~tvar, data=tse, col='cyan',lwd=2)


###################################################
### code chunk number 41: doBy-main.Rnw:684-688
###################################################
plot(ewin~tvar, data=tse,ylim=c(1,4))
rug(tse$tvar[tse$yvar==1], col='blue',lwd=4)
grid()
lines(run~tvar, data=tse,col='red')


###################################################
### code chunk number 42: doBy-main.Rnw:694-695
###################################################
tse$tvar[tse$abs<=1]


###################################################
### code chunk number 43: doBy-main.Rnw:703-706
###################################################
yvar <- as.numeric(lynx)
tvar <- 1821:1934
plot(tvar,yvar,type='l')


###################################################
### code chunk number 44: doBy-main.Rnw:713-717
###################################################
yyy <- yvar>mean(yvar)
head(yyy)
sss <- subSeq(yyy,TRUE)
sss


###################################################
### code chunk number 45: doBy-main.Rnw:721-723
###################################################
plot(tvar,yvar,type='l')
rug(tvar[sss$midpoint],col='blue',lwd=4)


###################################################
### code chunk number 46: doBy-main.Rnw:728-731
###################################################
yvar2 <- rep(0,length(yvar))
yvar2[sss$midpoint] <- 1
str(yvar2)


###################################################
### code chunk number 47: doBy-main.Rnw:735-737
###################################################
tse <- timeSinceEvent(yvar2, tvar)
head(tse,20)


###################################################
### code chunk number 48: doBy-main.Rnw:743-746
###################################################
len1 <- tapply(tse$ewin, tse$ewin, length)
len2 <- tapply(tse$run, tse$run, length)
c(median(len1),median(len2),mean(len1),mean(len2))


###################################################
### code chunk number 49: doBy-main.Rnw:751-754
###################################################
tse$yvar <- yvar
tse2 <- na.omit(tse)
plot(yvar~tae, data=tse2)


###################################################
### code chunk number 50: doBy-main.Rnw:758-761
###################################################
plot(tvar,yvar,type='l',lty=2)
mm <- lm(yvar~tae+I(tae^2)+I(tae^3), data=tse2)
lines(fitted(mm)~tvar, data=tse2, col='red')


###################################################
### code chunk number 51: doBy-main.Rnw:774-775
###################################################
options(oopt)


###################################################
### code chunk number 52: doBy-main.Rnw:790-791
###################################################
CO2


###################################################
### code chunk number 53: doBy-main.Rnw:796-797
###################################################
head(airquality, n=20)


