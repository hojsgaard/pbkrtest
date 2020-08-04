data(milkman, package="doBy")
dim(milkman)
head(milkman)
library(doBy)

source("lmBy.R")




mm <- lmBy(log(my)~dfc+log(dfc)|cowlact, data=mm3, na.action=na.exclude)

mm <- lmBy(log(my)~dfc+log(dfc)|cowlact, id=~race+lactno, data=mm3)

coef(mm,aug=T)

fitted(mm, augment=T)



fm1 <- lmList(Reaction ~ Days | Subject, sleepstudy)

lml1 <- lmList(my~dfc|cowlact, data=mm3, pool=F)
lml1







wdl <- splitBy(~cowlact, data=mm3)
lapply(wdl, function(wd) {
	m <- lm(log(my)~log(dfc)+dfc, data=wd)
	cbind(as.numeric(coef(m)), wd[1,])
	})
	

	m <- lm(my~dfc, data=wd)
	cbind(as.numeric(coef(m)), wd[1,])
	

bb <- as.data.frame(coef(m))

rownames(bb) <- paste(rownames(bb), ".coef", sep="")
bb <- t(bb)
rownames(bb) <- NULL

cbind(bb, wd[1,])



ff <- log(my)~dfc+log(dfc)|cowlact

mff <- modelFormula(ff)

idf <- ~race+lactno














wdl  <- splitBy(mff$groupFormula, data=mm3)
mm <- lapply(wdl, function(wd) {lm(mff$model, data=wd)})

do.call(rbind, lapply(mm,coef))
	
id.vars <- all.vars(idf)
do.call(rbind, lapply(wdl, function(wd) {wd[1,id.vars]}))





	
	










