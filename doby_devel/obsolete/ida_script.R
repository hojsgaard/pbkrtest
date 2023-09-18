library(geepack)
library(doBy)


###########konstrueret eksempel
timeorder <- rep(1:5, 6)
tvar <- timeorder + rnorm(length(timeorder))
idvar <- rep(1:6, each=5)
uuu <- rep(rnorm(6), each=5)
yvar <- 1 + 2*tvar + uuu + rnorm(length(tvar))
simdat <- data.frame(idvar, timeorder, tvar, yvar)
simdatPerm <- simdat[sample(nrow(simdat)),]
simdatPerm <- orderBy(~idvar, simdatPerm)
simdatPerm$yvar[simdatPerm$yvar<0]<- rlnorm(1)
w <- rlnorm(length(simdatPerm$yvar))
cor.fixed <- matrix(c(1 , 0.5 , 0.25, 0.125, 0.125,
0.5 , 1 , 0.25, 0.125, 0.125,
0.25 , 0.25 , 1 , 0.5 , 0.125,
0.125, 0.125, 0.5 , 1 , 0.125,
0.125, 0.125, 0.125, 0.125, 1 ), 5, 5)
zcor <- fixed2Zcor(cor.fixed, id=simdatPerm$idvar, waves=simdatPerm$timeorder)

# gaussian distribution, fixed correlation
mod1 <- geeglm(yvar~tvar, id=idvar,data=simdatPerm, corstr="fixed", zcor=zcor)
summary(mod1)
LSmeans(mod1)
LSmeans(mod1,at=list(tvar=3))
predict(mod1,newdata=data.frame(tvar=3)) #estimatet stemmer overens


# gaussian distribution, ar1 correlation
mod2 <- geeglm(yvar~tvar, id=idvar,data=simdatPerm, corstr="ar1")
summary(mod2)
LSmeans(mod2)
LSmeans(mod2,at=list(tvar=3))
predict(mod2,newdata=data.frame(tvar=3)) #estimatet stemmer overens


# gamma distribution, log-link, ar1 correlation
mod3 <- geeglm(yvar~tvar, id=idvar,data=simdatPerm, corstr="ar1",family=Gamma(link='log'))
summary(mod3)
LSmeans(mod3,at=list(tvar=4),type='response')
predict(mod3, newdata=data.frame(tvar=4), type='response') #estimatet stemmer overens


# gamma distribution, inverse-link, ar1 correlation
mod4 <- geeglm(yvar~tvar, id=idvar,data=simdatPerm, corstr="ar1",family=Gamma(link='inverse'))
summary(mod4)
LSmeans(mod4,at=list(tvar=3), type='response')
predict(mod4,newdata=data.frame(tvar=3), type='response') #estimatet stemmer overens


# gamma distribution, intercept model
mod5 <- geeglm(yvar~1, id=idvar,data=simdatPerm, corstr="ar1",family=Gamma(link='inverse'))
summary(mod5)
LSmeans(mod5, type='response') # giver en fejl besked
predict(mod5, type='response')


# gamma distribution, inverse-link, ar1 correlation, and weigths
mod6 <- geeglm(yvar~tvar, id=idvar,data=simdatPerm, corstr="ar1",family=Gamma(link='inverse'),weights=w)
summary(mod6)
LSmeans(mod6,at=list(tvar=3), type='response')
predict(mod6,newdata=data.frame(tvar=3), type='response') #estimatet stemmer overens


#effekt af faktorer, geepack dataset
mod7 <- geeglm(breaks ~ tension, id=wool, family=gaussian, data=warpbreaks)
LSmeans(mod7)
LSmeans(mod7,effect='tension',level=0.95)
LSmeans(mod7,effect='tension',level=0.90) #ingen effect af at ændre level

# glm, intercept model
mod8 <- glm(yvar~1, data=simdatPerm,)
summary(mod8)
LSmeans(mod8) # giver en fejl besked
predict(mod8)


predict(mod8, interval="confidence")
