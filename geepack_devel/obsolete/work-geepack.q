library(geepack)
f <- list.files("geepack/R",pattern=glob2rx("*.R"),full.names=T)
sapply(f,source)


library("geepack")
geedata<-read.table("gee_ex.txt",header=T)

## Everything is numeric:
sapply(geedata,class)
## Now create factors (according to YOUR OWN description):
geedata$year <- factor(geedata$year)
geedata$soil <- factor(geedata$soil)
geedata$trt  <- factor(geedata$trt)

## Y ahora; todo me parece bien:
model <- geeglm(moss ~ year * soil * trt, family = gaussian, id = id, corstr = "ar1", data=geedata)
summary(model)

model2 <- geeglm(moss ~ year * trt, family = gaussian, id = id, corstr = "ar1", data=geedata)
summary(model2)
anova(model,model2)

## Pero porque no asi (supongo que es el mismo modelo - no?):
library(lme4)

model.a <- lmer(moss ~ year * soil * trt + (1|id), data=geedata)
summary(model.a)

model2.a <- lmer(moss ~ year * soil + (1|id), data=geedata)
summary(model2.a)

anova(model.a, model2.a)

library(pbkrtest)
KRmodcomp(model.a, model2.a)
PBmodcomp(model.a, model2.a)



















timeorder <- rep(1:5, 6)
tvar      <- timeorder + rnorm(length(timeorder))
idvar <- rep(1:6, each=5)
uuu   <- rep(rnorm(6), each=5)
yvar  <- 1 + 2*tvar + uuu + rnorm(length(tvar))
simdat <- data.frame(idvar, timeorder, tvar, yvar)
head(simdat,12)

library(doBy)
simdatPerm <- simdat[sample(nrow(simdat)),]


simdatPerm1 <- orderBy(~idvar, simdatPerm)

simdatPerm2 <- simdatPerm[order(simdatPerm$idvar),]




head(simdatPerm)

cor.fixed <- matrix(c(1    , 0.5  , 0.25,  0.125, 0.125,
                      0.5  , 1    , 0.25,  0.125, 0.125,
                      0.25 , 0.25 , 1   ,  0.5  , 0.125,
                      0.125, 0.125, 0.5  , 1    , 0.125,
                      0.125, 0.125, 0.125, 0.125, 1     ), 5, 5)
cor.fixed

zcor <- fixed2Zcor(cor.fixed, id=simdatPerm$idvar, waves=simdatPerm$timeorder)
zcor

mod4 <- geeglm(yvar~tvar, id=idvar, data=simdatPerm, corstr="fixed", zcor=zcor)
mod4







data<-dataset <- read.csv("example data.csv",header=T)

library(geepack)
f <- list.files("geepack/R",pattern=glob2rx("*.R"),full.names=T)
sapply(f,source)

cars$id <- 1:nrow(cars)
cars$x  <- cars$speed

sapply(f,source)
geeglm(dist~speed, id=id, data=cars)
geeglm(dist~speed + x, id=id, data=cars)


g<-glm(dist~speed + x, data=cars)


m1 <- geeglm(MarketShare ~ Policy + TimeTrend + TimePolicy + Post +
    PolicyLevel + PostTrend + PolicyTrend,
    family=gaussian,
    data=TXdata,
    id=Policy,
    wave=TimeTrend,
    corstr="ar1",
    std.err="san.se")

m1$geese

summary(TXgeeglm)


m1$geese

































data(koch)
write.table(koch,file='koch.txt')

data(respdis)
write.table(respdis,file='respdis.txt')

library(geepack)
f <- list.files("geepack/R",pattern="\\.R",full.names=T)
f <- f[(1:length(f))[-grep("~",f)]]
sapply(f,source)



sapply(f,source)
mf <- geeglm(y ~ x1 + x2, id = id, data = mdat, waves = visit,
                   corstr="ar1", std.err='jack')
summary(mf)

mf <- geeglm(y ~ x1 + x2, id = id, data = mdat, waves = visit,
                   corstr="ar1", std.err='jack')
summary(mf)





[1,]  0.060652700 -0.009156728 -0.072387719
[2,] -0.009156728  0.048542224  0.002519852
[3,] -0.072387719  0.002519852  0.165668861

            [,1]         [,2]         [,3]
[1,]  0.06108700 -0.010802054 -0.075147303
[2,] -0.01080205  0.050298940  0.001521835
[3,] -0.07514730  0.001521835  0.170315347




sapply(f,source)
mf <- geese(y ~ x1 + x2, id = id, data = mdat, waves = visit,
                   corstr="ar1",
                   jack = TRUE, j1s = TRUE, fij = TRUE)

summary(mf)

g <- geese.control()





data(spruce)


library(dataRep)
data(sitka89)

m<-geeglm(size ~ as.factor(time)*treat, Gamma, data=sitka89, id=tree, corst='ar1')


sitkamiss <- sitka89

navec<-rbinom(nrow(sitkamiss),1,0.9)
navec[navec==0]<- NA
sitkamiss$size <- sitkamiss$size * navec

sitkamiss$ordering <- sitkamiss$time

sapply(f,source)
m2<-geeglm(size ~ as.factor(time)*treat, Gamma, data=sitkamiss, id=tree,
  corst='ar1', waves=ordering)

m2<-geeglm(size ~ as.factor(time)*treat, Gamma, data=sitkamiss[complete.cases(sitkamiss),], id=tree,
  corst='ar1', waves=ordering)
  
sitkamiss[complete.cases(sitkamiss),]
  




sit2 <- sitka89
sit2$time <- as.factor(sit2$time)
m<-geeglm(size ~ time+treat+time:treat, Gamma, data=sit2, id=tree, corst='ar1')

m<-geeglm(size ~ treat+as.factor(time)+as.factor(time):treat, Gamma, data=sitka89, id=tree, corst='ar1')

m<-geeglm(size ~ as.factor(time)+treat+as.factor(time):treat, Gamma, data=sitka89, id=tree, corst='ar1')
sapply(f,source)
anova(m)

library(dataRep)
data(fatacid)
fatacid$sample<-factor(fatacid$sample)
library(geepack)

sapply(f,source)
g.b<-geeglm(x14~sample+dose+sample:dose,id=pigid,data=fatacid,
  corstr="exch")

g.a<-geeglm(x14~sample+dose+sample:dose,id=pigid,data=fatacid,
  control=geese.control(maxit=3),corstr="unstr")

g.a2<-geeglm(x14~sample+dose+sample:dose,id=pigid,data=fatacid,corstr="un")

g.b<-geese (x14~sample+dose+sample:dose,id=pigid,data=fatacid,control=geese.control(maxit=3),corstr="un")


data(seizure)
## Diggle, Liang, and Zeger (1994) pp166-168, compare Table 8.10
seiz.l <- reshape(seizure, varying=list(c("base","y1", "y2", "y3", "y4")),
                  v.names="y", times=0:4, direction="long")
seiz.l <- seiz.l[order(seiz.l$id, seiz.l$time),]
seiz.l$t <- ifelse(seiz.l$time == 0, 8, 2)
seiz.l$x <- ifelse(seiz.l$time == 0, 0, 1)
sapply(f,source)
f2 <- geeglm(y ~ offset(log(t)) + x + trt + x:trt, id = id,
              data = seiz.l, subset = id!=49, corstr = "exch", family=poisson)

f3 <- geeglm(y ~ offset(log(t)) + x + trt, id = id,
              data = seiz.l, subset = id!=49, corstr = "exch", family=poisson)

anova(f2)
anova(f2,f3)
summary(f2)

g2 <- glm(y ~ offset(log(t)) + x + trt + x:trt,
            data = seiz.l, subset = id!=49, family=poisson)
g2 <- glm(y ~  x + trt + x:trt,
            data = seiz.l, subset = id!=49, family=poisson)

g3 <- glm(y ~ offset(log(t)) + x + trt,
            data = seiz.l, subset = id!=49, family=poisson)


sapply(f,source)

library(geepack)
data(dietox)

dietox$Cu     <- as.factor(dietox$Cu)
gee01 <- geeglm (Weight ~ Time + Cu + Cu * Time, id =Pig, data = dietox,
         family=gaussian,corstr="ex")

mf <- formula(Weight~Cu*(Time+I(Time^2)+I(Time^3)))
gee1 <- geeglm(mf, data=dietox, id=Pig, family=poisson("identity"),corstr="ar1")
summary(gee)
anova(gee)



mf2 <- formula(Weight~Cu*Time+I(Time^2)+I(Time^3))
gee2 <- geeglm(mf2, data=dietox, id=Pig, family=poisson("identity"),corstr="ar1")
anova(gee2)


data(spruce)
spruce$contr <- ifelse(spruce$ozone=="enriched", 0, 1)
sitka88 <- spruce[spruce$wave <= 5,]
sitka89 <- spruce[spruce$wave > 5,]
sitka89$time <- as.factor(sitka89$time)

fit.88 <- geeglm(logsize ~ ozone * time ,
                id=id, data=sitka89, corstr="exch")

fit.88 <- geeglm(logsize ~ as.factor(wave) + contr +
                          I(time/100*contr) - 1,
                id=id, data=sitka88, corstr="ar1")

summary(fit.88)

sitka88$fit<-fitted(fit.88)

coplot(fit~time|contr, data=sitka88,type="b")


plot(logsize~wave, data=spruce)


sitka88$tid<-sitka88$time

sortBy<-function(x,b,along=NULL){
 co<-(gsub(" ","",unlist(strsplit(paste(b)[2],"\\+"))))
 id<-(match(co,names(x)))
 ord<-SHorder(as.list(x[,id]), na.last=NA)
 lord<-length(ord)
 x<-x[ord,]
 if(!is.null(along)){
   for (i in 1:length(along)){
       curr <- along[[i]]
       print(dim(curr))
       if (length(dim(curr))==2){
          if (nrow(curr)==lord)
             z <- curr[ord,]
          else
              stop("jjjjjjjjjj")
          } else {
          if (length(curr)==lord)
             z <- curr[ord]
          else
              stop("jjjjjjjjjj")

          }
       }
    }
       ,
                                2={print(2)},
                                "0"={print(0); print(curr)}
                          )
   }
 }
 #return(x)
}
sortBy(sitka88,~tid+time,along=list(sitka88,sitka88[,1]))


SHorder(as.list(sitka88[,c(1,4)]),na.last=NA)


SHorder<-
function (..., na.last = TRUE, decreasing = FALSE)
{
    if (!is.na(na.last))
        .Internal(order(na.last, decreasing, ...))
    else {
        #z <- list(...)
        z<-c(...)
#        print(z)
        if (any(diff(sapply(z, length)) != 0))
            stop("Argument lengths differ")
        ans <- sapply(z, is.na)
        ok <- if (is.matrix(ans))
            !apply(ans, 1, any)
        else !any(ans)
        if (all(!ok))
            return(integer(0))
        z[[1]][!ok] <- NA
        ans <- do.call("order", c(z, decreasing = decreasing))
        keep <- seq(along = ok)[ok]
        ans[ans %in% keep]
    }
}

SHorder(list(sitka88$id,sitka88$tid),na.last=NA)

order(sitka88[,c(9,4)])




data(Orthodont)
fm1 <- lme(distance ~ age, data = Orthodont) # random is ~ age
fm2 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)
summary(fm1)
summary(fm2)

f <- list.files("SHtools/R",pattern="\\.R",full.names=T)
f <- f[(1:length(f))[-grep("~",f)]]
sapply(f,source)

par(mfrow=c(1,3),omi=c(0,0,.5,0))
plotBy(Time,Weight,subject=Pig,group=Cu,title="Cu=", data=dietox,col=1:100,lines=T)
mtext("Plot of weight against Time",outer=TRUE)


