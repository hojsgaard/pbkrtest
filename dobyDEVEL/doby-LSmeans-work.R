library(doBy)

srcit <- function(){
    flist <- list.files("linestPack/R",pattern=glob2rx("*.R"),full.names=TRUE);
    sapply(flist,source)
    NULL
}

library(linestPack)


## GEE
library(geepack)
warp.gee <- geeglm(breaks~tension, id=wool, data=warpbreaks)
Linest(warp.gee, effect="tension")

## GLM
warp.glm <- glm(breaks~tension+wool, family=Gamma, data=warpbreaks)
Linest(warp.glm, effect="tension")

## LMM
library(lme4)
warp.mm <- lmer(breaks~-1+tension+ (1|wool), data=warpbreaks)
Linest(warp.mm, effect="tension", adjust.df=F)

## GLMM
warp.mm <- glmer(breaks~-1+tension+ (1|wool), family=poisson, data=warpbreaks)
Linest(warp.mm, effect="tension")

library(ggplot2)

ChickWeight$Diet <- factor(ChickWeight$Diet)
qplot(Time, weight, data=ChickWeight, colour=Chick, facets=~Diet,
      geom=c("point","line"))

mm <- lm(weight~Time*Diet, data=ChickWeight)

ww <- lm(breaks~-1+wool:tension, data=warpbreaks)

linestMatrix(mm, "Diet", at=list(Time=0))
linestMatrix(mm, "Diet", at=list(Time=1))


K <- linestMatrix(mm, "Diet", at=list("Time"=1)) - linestMatrix(mm, "Diet", at=list("Time"=0))

LSmeans(mm, K=K)

.get_linest_list(mm, effect="wool")




library(doBy)
library(lme4)
library(pbkrtest)
library(lsmeans)


data(beets)

object <- lm(sugpct~harvest+sow+ yield, data=beets)

srcit(); (aa1 <- linestMatrix(object))

srcit(); (aa2 <- linestMatrix(object, effect="harvest"))

srcit(); (aa3 <- linestMatrix(object, at=list(sow="sow3")))

srcit(); (aa4 <- linestMatrix(object, effect="harvest", at=list(sow="sow3"))) #NO

srcit(); (aa4 <- le(object, effect="harvest", at=list(sow="sow3")))

effect = "sow"
at = NULL

object <- lm(sugpct~harvest+sow+ yield, data=beets)

srcit()
(m <- linest(object, effect=effect, at=at))

srcit()
pdiff(object, effect=effect, at=at)


object<-lmer(yield~block+sow+harvest+(1|block:harvest), data=beets, REML=FALSE)

srcit()
(m<-linest(object, effect="sow"))

lsmeans(object, ~sow)


















source("Russ.R"); russ(object, ~sow)


a <- factor(1:2)
b <- factor(1:3)
d <- expand.grid(a=a,b=b)
d <- rbind(d,d)
d$y <- rnorm(nrow(d))
d <- subset(d, !(b==3 & a==2))
xtabs(~a+b, data=d)

effect=c("a","b")
at=NULL
object <- lm(y~a*b, data=d)
srcit()
linest(object, effect=effect)

pdiff(object, effect=effect)








## Make balanced dataset
dat.bal <- expand.grid(list(AA=factor(1:2), BB=factor(1:3), CC=factor(1:3)))
dat.bal$y <- rnorm(nrow(dat.bal))

## Make unbalanced dataset
#   'BB' is nested within 'CC' so BB=1 is only found when CC=1
#   and BB=2,3 are found in each CC=2,3,4
dat.nst <- dat.bal
dat.nst$CC <-factor(c(1,1,2,2,2,2,1,1,3,3,3,3,1,1,4,4,4,4))
mod.bal  <- lm(y ~ AA + BB*CC,    data=dat.bal)
mod.nest <- lm(y ~ AA + BB%in%CC, data=dat.nst)

mod.bal <- lm(y ~ AA + BB*CC,    data=dat.bal)
mod.nst <- lm(y ~ AA + BB %in% CC, data=dat.nst)

mod.nst2 <- lm(y ~ AA + BB : CC, data=dat.nst)

mod.nst


pm<- linestMatrix(mod.nst2, effect=c("BB", "CC"))

linest(mod.nst2, effect=c("BB", "CC"))
lsmeans(mod.nst2, ~BB*CC)

ftable(xtabs(~AA+BB+CC, data=dat.nst))




mod.nst <- lm(y ~ AA + CC * BB , data=dat.nst)
mod.nst













