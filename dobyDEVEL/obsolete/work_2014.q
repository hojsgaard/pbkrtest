load_all("doBy")
data(hellung, package="ISwR")
head(hellung, 4)
hellung <- transform(hellung, lconc=log(conc),
                     gluc=factor(glucose, labels=c("yes","no")))
head(hellung, 4)
hmod2 <- lm( diameter ~ gluc + lconc, data=hellung)

load_all("doBy")
linest(hmod2, c(1,1,0))





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

mod4 <- lm(yvar~tvar, data=simdatPerm)


load_all("doBy")
a<-doBy:::.get_linest_list(mod4)
a

load_all("doBy")
K<-LSmatrix(mod4)
L <- linest(mod4, K)


LSmatrix(mod4, at=list(tvar=3))

LSmeans(mod4)
LSmeans(mod4,at=list(tvar=3))



cor.fixed <- matrix(c(1 , 0.5 , 0.25, 0.125, 0.125,
0.5 , 1 , 0.25, 0.125, 0.125,
0.25 , 0.25 , 1 , 0.5 , 0.125,
0.125, 0.125, 0.5 , 1 , 0.125,
0.125, 0.125, 0.125, 0.125, 1 ), 5, 5)


zcor <- fixed2Zcor(cor.fixed, id=simdatPerm$idvar, waves=simdatPerm$timeorder)

mod4 <- geeglm(yvar~tvar, id=idvar,data=simdatPerm, corstr="fixed", zcor=zcor)
summary(mod4)
#OK men begge nedenstående forsøg på prediktion giver en fejl besked

















warpbreaks

library(doBy)

load_all("doBy")
m <- lm(breaks~wool+tension, data=warpbreaks)
p <-linest(m)
p

names(p$coef)[5]<-

printCoefmat(p$coef, tst.ind=4, na.print='', P.values=T, has.Pvalue=T)



sd<-4
load_all("doBy")
summaryBy(rate+conc ~ state, data=Puromycin, FUN = c(mean, sd))

aggregate(conc ~ state, data=Puromycin, FUN = sd)



lapplyBy(~Plant+Type, data=CO2, FUN = function(x){summary(x$uptake)})

lapplyBy(~Plant+Type, data=CO2, FUN = function(x){summary(x$uptake)},keep.attr = T)

## sapplyBy <- function(formula, data=parent.frame(), FUN) {
##   sb <- splitBy(formula, data = data)
##   gr <- unique(attr(sb, "grps"))
##   ret <- attr(sb, "groupid")
##   if (is.function(FUN))
##     FUN <- list(FUN=FUN)
##   for (n in names(FUN)) {
##     ddd <- sapply(sb, FUN[[n]])
##     ret <- cbind(ret,
##                  renameCol(data.frame(result=ddd[gr]),
##                            "result", n))
##   }
##   ret
## }


sapplyBy(~Plant+Type, data=CO2,
         FUN = function(x){
             c(m=mean(x$uptake))
         })


ddd<-splitBy(~Type+Plant, data=CO2)
grpid <- attr(ddd, "groupid")

out<-sapply(dd, FUN=function(x)summary(x$uptake))
cbind(grpid,t(out))


out<-sapply(dd, FUN=function(x)summary(x))


answer<-lapply(dd, summary)

simplify=T
a<-simplify2array(answer, higher = (simplify == "array"))


X <- dd; FUN=summary

lapplyBy(~Type+Plant, data=CO2, FUN=c(mean,var))


x<-sapply(dd, summary, simplify=T)

lapply(dd, FUN=c(mean,var))


sapplyBy<-function (formula, data = parent.frame(), FUN, simplify = TRUE, USE.NAMES = TRUE){
    ddd <- splitBy(formula, data = data)
    grpid <- attr(ddd, "groupid")
    idx <- match(names(grpid),names(data))
    ddd <- lapply(ddd, function(x){x[,-idx]})
    out <- sapply(ddd, FUN=FUN, simplify=simplify, USE.NAMES = USE.NAMES)
    if (is.matrix(out)){
        cbind(grpid, t(out))
    } else {
        out
    }
}

sapplyBy(~Plant+Type, data=CO2, summary)

sapply(dd, summary)



lapplyBy <- function (formula, data = parent.frame(), FUN)
{
    ddd <- splitBy(formula, data = data)
    grp    <- unique(attr(ddd,"grps"))
    grpid  <- unique(attr(ddd,"groupid"))
    idx <- match(names(grpid),names(data))
    ddd <- lapply(ddd, function(x){x[,-idx]})
    ddd <- lapply(ddd, FUN)
    ddd <- ddd[grp] ## probably unnecessary
    attr(ddd, "groupid") <- grpid
    ddd
}

lapplyBy(~Plant+Type, data=CO2, c(mean,var))
