library(doBy)
library(geepack)

timeorder <- rep(1:5, 6)
tvar <- timeorder + rnorm(length(timeorder))
idvar <- rep(1:6, each=5)
uuu <- rep(rnorm(6), each=5)
yvar <- 1 + 2*tvar + uuu + rnorm(length(tvar))
simdat <- data.frame(idvar, timeorder, tvar, yvar)

simdatPerm <- simdat[sample(nrow(simdat)),]
simdatPerm <- orderBy(~idvar, simdatPerm)

cor.fixed <- matrix(c(1 , 0.5 , 0.25, 0.125, 0.125,
0.5 , 1 , 0.25, 0.125, 0.125,
0.25 , 0.25 , 1 , 0.5 , 0.125,
0.125, 0.125, 0.5 , 1 , 0.125,
0.125, 0.125, 0.125, 0.125, 1 ), 5, 5)


zcor <- fixed2Zcor(cor.fixed, id=simdatPerm$idvar, waves=simdatPerm$timeorder)

mod4 <- geeglm(yvar~tvar, id=idvar,data=simdatPerm, corstr="fixed", zcor=zcor)
summary(mod4) 
#OK men begge nedenstående forsøg på prediktion giver en fejl besked


mod4 <- lm(yvar~tvar, data=simdatPerm)

LSmeans(mod4) 
LSmeans(mod4,at=list(tvar=3))


