### R code from vignette source 'geepack-manual.Rnw'

###################################################
### code chunk number 1: geepack-manual.Rnw:16-19
###################################################
require( geepack )
prettyVersion <- packageDescription("geepack")$Version
prettyDate <- format(Sys.Date())


###################################################
### code chunk number 2: geepack-manual.Rnw:74-76
###################################################
library(geepack)
citation("geepack")


###################################################
### code chunk number 3: geepack-manual.Rnw:107-118
###################################################
library(geepack)
n_cluster <- 6
n_time    <- 5
set.seed(1213)
timeorder <- rep(1:n_time, n_cluster)
tvar      <- timeorder + rnorm(length(timeorder))
idvar  <- rep(1:n_cluster, each=n_time)
uuu    <- rep(rnorm(n_cluster), each=n_time) # A 'random intercept'
yvar   <- 1 + 2 * tvar + uuu + rnorm(length(tvar))
simdat <- data.frame(idvar, timeorder, tvar, yvar)
head(simdat, 12)


###################################################
### code chunk number 4: geepack-manual.Rnw:127-129
###################################################
mod1 <- geeglm(yvar~tvar, id=idvar, data=simdat, corstr="ar1")
mod1


###################################################
### code chunk number 5: geepack-manual.Rnw:145-149
###################################################
set.seed(123)
simdatPerm <- simdat[sample(nrow(simdat)),]
simdatPerm <- simdatPerm[order(simdatPerm$idvar),]
head(simdatPerm)


###################################################
### code chunk number 6: geepack-manual.Rnw:158-160
###################################################
mod2 <- geeglm(yvar~tvar, id=idvar, data=simdatPerm, corstr="ar1")
mod2


###################################################
### code chunk number 7: geepack-manual.Rnw:167-170
###################################################
simdatPerm2 <- simdat[order(simdat$timeorder),]
head(simdatPerm2)
geeglm(yvar~tvar, id=idvar, data=simdatPerm2, corstr="ar1")


###################################################
### code chunk number 8: geepack-manual.Rnw:176-180
###################################################
wav <- simdatPerm$timeorder
wav
mod3 <- geeglm(yvar~tvar, id=idvar, data=simdatPerm, corstr="ar1", waves=wav)
mod3


###################################################
### code chunk number 9: geepack-manual.Rnw:189-195
###################################################
cor.fixed <- matrix(c(1    , 0.5  , 0.25,  0.125, 0.125,
                      0.5  , 1    , 0.25,  0.125, 0.125,
                      0.25 , 0.25 , 1   ,  0.5  , 0.125,
                      0.125, 0.125, 0.5  , 1    , 0.125,
                      0.125, 0.125, 0.125, 0.125, 1     ), 5, 5)
cor.fixed


###################################################
### code chunk number 10: geepack-manual.Rnw:203-205
###################################################
zcor <- fixed2Zcor(cor.fixed, id=simdatPerm$idvar, waves=simdatPerm$timeorder)
zcor


###################################################
### code chunk number 11: geepack-manual.Rnw:214-216
###################################################
mod4 <- geeglm(yvar~tvar, id=idvar, data=simdatPerm, corstr="fixed", zcor=zcor)
mod4


