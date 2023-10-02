library('geepack')
data('ohio')
 fit.exch <- geeglm(resp~age+smoke, family=binomial(link="logit"),
                    data=ohio, id=id, corstr = "exchangeable", std.err="san.se")
 
anova.geeglm(fit.exch)














library(geepack)
library(dplyr)

x <- read.csv("GEE.csv")


load_all("geepack")
res<-geeglm(formula=SAR ~ year, id=Co_name, data=x, family = binomial(link = "logit"),  corstr="exchangeable")
res0<-geeglm(formula=SAR ~ 1, id=Co_name, data=x, family = binomial(link = "logit"),  corstr="exchangeable")
a<-anova(res, res0)
a


library(geeM)
res<-geem(formula=SAR ~ year, id=Co_name, data=x, family = binomial(link = "logit"),  corstr="exchangeable")
res0<-geem(formula=SAR ~ 1, id=Co_name, data=x, family = binomial(link = "logit"),  corstr="exchangeable")

a<-anova(res, res0)
a






bv
V <- bv$V0
b <- bv$b0


V <- bv$V0

V <- diag(diag(V)) + V 

ee <- eigen(V)
dd <- ee$values
rr <- sum(dd > 1e-12)

X2 <- as.numeric( t(b) %*% MASS::ginv(V) %*% b )
X2

as.numeric(t(b) %*% solve(V, b))

Vb <- t(ee$vec) %*% b
t(Vb) %*% diag(1 / dd) %*% Vb









anova(res)




data(dietox)


data(dietox)
dietox$Cu     <- as.factor(dietox$Cu)
mf <- formula(Weight~Cu*(Time+I(Time^2)+I(Time^3)))
gee1 <- geeglm(mf, data=dietox, id=Pig, family=poisson("identity"),corstr="ar1")
gee1
summary(gee1)
g <- glm(mf, data=dietox, family=poisson("identity"))








dietox$Cu     <- as.factor(dietox$Cu)

mf <- formula(Weight~Cu*(Time+I(Time^2)+I(Time^3)))

## run the model
gee1 <- geeglm(mf, data=dietox, id=Pig, family=poisson("identity"),corstr="ar1")

## Create a new dataframe to generate predictions
newdat <- dplyr::select(dietox, Time, Cu)

## Generate predictions 
pr <- predict(gee1, newdat, type="response", re.form=NA)


glm1 <- glm(mf, data=dietox, family=poisson("identity"))
pr <- predict(glm1, newdat, type="response", re.form=NA)


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


