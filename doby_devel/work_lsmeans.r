library(doBy)
library(emmeans)

## GEE
library(geepack)
warp.gee <- geeglm(breaks~tension, id=wool, data=warpbreaks)
oo <- LSmeans(warp.gee, effect="tension")
LSmeans(warp.gee, effect=~tension)
emmeans(warp.gee, specs =~tension)


load_all("_doby")
library(lme4)
warp.mm <- lmer(breaks~-1+tension+ (1|wool), data=warpbreaks)
aa  <- LSmeans(warp.mm, effect="tension", adjust.df=F)
aa





oo

ee <-emmeans(warp.mm, specs="tension")


        <- cbind(oo$grid, oo$coef)
attr(oo2, "L") <- oo$L

cbind(oo2,
      cbind(lwr=oo2$estimate -1.96*oo2$std.error,
            upr=oo2$estimate +1.96*oo2$std.error))









## GLM
warp.glm <- glm(breaks~tension+wool, family=Gamma, data=warpbreaks)
LSmeans(warp.glm, effect="tension")

## LMM

## GLMM
warp.mm <- glmer(breaks~-1+tension+ (1|wool), family=poisson, data=warpbreaks)
LSmeans(warp.mm, effect="tension")

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















library(tidyverse)

ToothGrowth <- ToothGrowth %>% 
    mutate(dosef = factor(dose, 
                          levels = c(0.5, 1, 2), 
                          labels = c("LO", "ME", "HI")))



tooth2 <- lm(len ~ supp + dosef, data = ToothGrowth)
coef(tooth2)

new.data <- data.frame(supp = c("VC", "OJ"), dosef = "ME")
predict(tooth2, newdata = new.data, interval = "confidence")


load_all("doby")
at <- list(supp = c("VC" ,"OJ"), dosef = "ME")
at


K <- doBy::LE_matrix(tooth2, at = at)
K

load_all("doby")
at2 <- as.data.frame(at)
at2
K2 <- LE_matrix(tooth2, at = at2)
K2


at3 <- split(at2, 1:2)


do.call(rbind, lapply(at3, function(at) LE_matrix(tooth2, at = at)))












at <- list(supp = c("VC" ,"OJ"), dosef = "ME")
K <- linest_matrix(tooth2, at = at)

K
summary(K)






linest(tooth2, L = K)







lambda <- c(0, 0, -1, 1)
sum(coef(tooth2) * lambda)

library(doBy)
esticon(tooth2, cm = lambda)







































library(ggplot2)
library(dplyr)

bacteria <- read.table("./DATA/bacteria.txt", header=T)
bacteria <- bacteria  %>% mutate(day=factor(day), leucine=factor(leucine), 
                                 sucrose=factor(sucrose))
sapply(bacteria, class)

qplot(sucrose, logdens, group=day, colour=day, data=bacteria,
      geom=c("point", "line")) + facet_grid(~leucine) 

mod <- lm(logdens ~ day + leucine + sucrose, data = bacteria)


load_all("doBy")
l1 = c(1, 1/4, 1/4, 1/4, 0, 1, 0, 0, 1)
l2 = c(1, 1/4, 1/4, 1/4, 0, 1, 0, 0, 0)
K1 <- rbind(l1, l2)
ec1 <- esticon(mod, l1)
ec2 <- esticon(mod, K1)
class(ec1) 
attributes(ec1)
attributes(ec1)$L

load_all("doBy")
le1 <- linest(mod, K1)
le1
class(le1)
names(le1)
coef(le1)
confint(le1)
summary(le1)

load_all("doBy")
as(le1, "data.frame")

at1 <- list(day = c("1" ,"2"), leucine = "3", sucrose = "4")
at2 <- list(sucrose = "4")
at3 <- list(leucine = "3", sucrose = "4")

K1 <- linest_matrix(mod, at=at1)
K2 <- linest_matrix(mod, at=at2)
K3 <- linest_matrix(mod, at=at3)
K1; K2; K3

le1 <- get_linest_list(mod, effect="leucine", at=at1)
le1
class(le1)

L1 <- linest_matrix(mod, effect="leucine", at=at1)

linest(mod, L=L1) %>% summary

le2 <- get_linest_list(mod, effect="leucine", at=at2)
class(le2)
## burde være linest_list_class
## naturligt hvis effect også var attribute.


linest(mod, L=K1)

library(doBy)
popMeans(mod, at=at1) %>% summary 

library(doBy)
load_all("doBy")
lsm.tab <- popMeans(mod, effect="leucine", at=list(sucrose="4"))
lsm.tab

lsm.tab <- popMeans(mod, effect=c("leucine", "sucrose"))
lsm.tab
x <- lsm.tab
x$coef
x$K
x$grid

## linearEstimate -> linest _class
## linest_matrix -> linest_matrix_class



confint(le1)



confint(x)




confint(xx)
confint(xx, 1:4)







%%
%%<< >>= 
%%ggplot(lsm.tab, aes(x=interaction(leucine, sucrose), y=estimate, 
%%                    colour=sucrose)) + 
%%    geom_errorbar(aes(ymin=estimate-se, ymax=estimate+se), width=.1) +
%%    geom_line() +
%%    geom_point()
%%@
%%
