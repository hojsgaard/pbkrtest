library(SHtools)
library(dataRep)
library(doBy)
library(glmFunctions)
library(geepack)
data(sitka89)
par(mfrow=c(1,2))
plotBy(size~time,subject=tree, group=treat, data=sitka89,lines=TRUE,title="Treatment=",col=1:30)
#shsavePlot("fig/sitka-fig01")

sitkasum <- summaryBy(size~time+treat, data=sitka89, FUN=mean)

par(mfrow=c(1,1))
plotBy(mean.size~time, subject=treat,lines=TRUE, data=sitkasum, col=1:2)
shsavePlot("fig/sitka-fig02")
sitka89$time <- as.factor(sitka89$time)
sapply(f,source)

g1 <- geese(size~treat+time+treat:time, id=tree, data=sitka89, family=gaussian, corstr="ind")

options( contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))

g1w <- geew(size~-1+treat:time, id=tree, data=sitka89, family=gaussian, corstr="ind")
summary(g1w)
sapply(f,source)
plot(g1w)

g2w <- geew(size~treat:time, id=tree, data=sitka89, family=gaussian, corstr="ind")
summary(g2w)

g <- glm(size~treat:time, data=sitka89)
summary(g)

par(mfrow=c(2,2))
plot(g)

g2w <- geew(size~treat:time, id=tree, data=sitka89, family=gaussian, corstr="ind")
summary(g2w)
sapply(f,source)
#par(mfrow=c(1,3))
plot(g2w)







residuals(g,"pearson")-residuals(g,"working")

summary(g1w)
esticon(g1w,c(0,0,1,-1,0,0,0,0,0,0,0,0,0,0,0,0))

options( contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))
g1w <- geew(size~treat+time+treat:time, id=tree, data=sitka89, family=gaussian, corstr="ind")
summary(g1w)
esticon(g1w,c(0,0,1,-1,0,0,0,0,0,0,0,0,0,0,0,0))


options( contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
g1w <- geew(size~time, id=tree, data=sitka89, family=gaussian, corstr="ind")

options( contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))
g2w <- geew(size~time, id=tree, data=sitka89, family=gaussian, corstr="ind")



summary(g1w)


g12w <- geew(size~treat+time, id=tree, data=sitka89, family=gaussian, corstr="ind")
anova(g1w,g12w)

summary(g1)
L1 <- cbind(matrix(0,7,9),diag(rep(1,7)))
esticon(g1, L1, joint.test=TRUE)

g1 <- geese(size~time+treat:time, id=tree, data=sitka89, family=gaussian, corstr="ind")

g1 <- geese(size~-1+treat:time, id=tree, data=sitka89)

L2 <- matrix(0,8,16)
L2[cbind(c(1,2,3,4,5,6,7,8),c(1,3,5,7,9,11,13,15))] <- 1
L2[cbind(c(1,2,3,4,5,6,7,8),c(2,4,6,8,10,12,14,16))] <- -1

esticon(g1,L2,conf.int=0.95)

g1 <- geese(size~treat+time, id=tree, data=sitka89)
