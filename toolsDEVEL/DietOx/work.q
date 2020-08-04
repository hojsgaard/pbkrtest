library(geepack)
library(doBy)

f <- list.files("../SHtools/R",pattern="\\.R",full.names=T)
f <- f[(1:length(f))[-grep("~",f)]]
a <-sapply(f,source)


data(dietox)
dietox[1:5,]
dietox$Cu     <- as.factor(dietox$Cu)

### Prøv at estimere varianser på estimaterne med bootstrap - Det virker fint!!!
### Kan ikke få boot() funktionen til at virke for sammenligning...
mf <- formula(Weight~Cu*(Time+I(Time^2)+I(Time^3)))
gee1 <- geeglm(mf, data=dietox, id=Pig, family=poisson("identity"))
result <- NULL
dsplit <- split(dietox, dietox$Pig)
ld <- length(dsplit)

for(k in 1:20){
idx <- sample(1:ld,ld,replace=T)
sub <- dsplit[idx]
for (i in 1:length(sub))
  sub[[i]]$Pig <- i
sub <- list2df(sub)
gee2 <- geeglm(mf, data=sub, id=Pig, family=poisson("identity"))
result <- rbind(result,gee2$coef)
}

seRob <- summary(gee1)$coef$Rob
seBoot <- sqrt(apply(result,2,var))
round(cbind(seRob,seBoot,seBoot/seRob),6)
### Slut - bootstrap






dnew <- data.frame(Time=c(7,7),Cu=as.factor(c(2,3)))
predict(gee2,dnew)

L.Cu2 <- c(1,1,0,7,7^2,7^3,7,0,7^2,0)
L.Cu3 <- c(1,0,1,7,7^2,7^3,0,7,0,7^2)

sum(gee2$coef * c(1,1,0,7,7^2,7^3,7,0,7^2,0))

esticon(gee2,L.Cu2)
esticon(gee2,L.Cu3)
esticon(gee2, L.Cu2-L.Cu3)

lm5 <- update(lm4,.~.-Cu:I(Time^3)) 

lsmean(gee2)

mT <- mean(dietox$Time)
mT2 <- mean(dietox$Time^2)
mT3 <- mean(dietox$Time^3)

L1 <- c(1,0,0,mT,mT2,mT3,0,0,0,0)
L2 <- c(1,1,0,mT,mT2,mT3,mT,0,mT2,0)
L3 <- c(1,0,1,mT,mT2,mT3,0,mT,0,mT2)
esticon(gee2,rbind(L1,L2,L3))


L <- c(1,0,0,mT,mT2,mT3,0,0,0,0)

esticon(gee2,L)

a <-sapply(f,source)
a2<-lsmean2.lm(lm5, factors="Cu")


dietox2 <- dietox
dietox2$Time2 <- dietox$Time^2
dietox2$Time3 <- dietox$Time^3

gee3 <- geeglm(Weight~Cu*Time+Cu*Time2+Time3, data=dietox2, id=Pig, family=poisson("identity"),corstr="ar1")


lsmean(gee2)



lsmean(gee2)
lsmean(gee3)


lm6 <- lm(Weight~Cu*Time+Cu*Time2+Time3, data=dietox2)
lsmean(lm6)



mf <- formula(Weight~Cu*(Time+I(Time^2)+I(Time^3)))
lm4 <- lm(mf, data=dietox)



                                      Least Squares Means

                                          Standard
              Effect    Cu    Estimate       Error      DF    t Value    Pr > |t|

              Cu        1      60.4448      0.4229     851     142.92      <.0001
              Cu        2      59.5772      0.4042     851     147.40      <.0001
              Cu        3      62.1968      0.4132     851     150.51      <.0001



write.table(dietox, "dietox.csv",sep=",",na="", row.names=F)








lm1 <- lm(mean.Weight~Cu*Time, data=m.dietox)
lm2 <- lm(mean.Weight~Cu*poly(Time,2), data=m.dietox)
lm3 <- lm(mean.Weight~Cu*poly(Time,3), data=m.dietox)

 par(mfrow=c(1,3))
 plotBy(resid(lm1)~Time, subject=Cu, data=m.dietox,lines=T,col=1:3)
 plotBy(resid(lm2)~Time, subject=Cu, data=m.dietox,lines=T,col=1:3)
 plotBy(resid(lm3)~Time, subject=Cu, data=m.dietox,lines=T,col=1:3)
shsavePlot("fig\\dietox-residplots01")


par(mfrow=c(1,1))
plot(log(fitted(lm4)),log(resid(lm4)^2))
l <- lm(log(resid(lm4)^2)~log(fitted(lm4)))
abline(l, col="red")
l
shsavePlot("fig\\dietox-varmeanplots01")



lme1 <- lme(Weight~Cu*(Time+I(Time^2)+I(Time^3)), data = dietox, random=~1|Pig)

plot(lme1)


lme2 <- lme(Weight~Cu*(Time+I(Time^2)+I(Time^3)), data = dietox, weights=varPower(fixed=1), random=~1|Pig)

lme2 <- lme(Weight~Cu*(Time+I(Time^2)+I(Time^3)), data = dietox,  random=~1+Time|Pig)

lme2 <- gls(Weight~Cu*(Time+I(Time^2)+I(Time^3)), data = dietox, weights=varPower(1),correlation=corAR1(form=~1|Pig))
lme2 <- lme(Weight~Cu*(Time+I(Time^2)+I(Time^3)), data = dietox, weights=varPower(1),correlation=corAR1(form=~1|Pig), 
                                                                                    random=~1|Pig)


f <- list.files("../SHtools/R",pattern="\\.R",full.names=T)
f <- f[(1:length(f))[-grep("~",f)]]
sapply(f,source)
mf <- formula(Weight~Cu*(Time+I(Time^2)+I(Time^3)))
gee1 <- geeglm(mf, data=dietox, id=Pig, family=poisson("identity"),corstr="ar1")


SElm <- slm4$coef[,2]
SEgee<- sgee1$coef[,2]
SElme <- summary(lme2)$tT[,2]
cbind(SElme,SEgee,SElme/SElm, SEgee/SElm,SElme/SEgee )


plot(lme1)
plot(lme2)
par(mfrow=c(2,3))
plotBy(fitted(lme2)~Time,subject=Pig,group=Cu, data=dietox, lines=T, col=1:100)
plotBy(resid(lme2)~Time,subject=Pig,group=Cu, data=dietox, lines=T, col=1:100)

plotBy(fitted(lme1)-fitted(lme2)~Time,subject=Pig,group=Cu, data=dietox, lines=T, col=1:100)



bootBy<-function(data,by){
  dietox <- data

  d <- unique(dietox[,by])
  id <- sample(d,length(d),replace=T)

  v <- NULL
  for (i in 1:length(id)){
   dsub <- subset(dietox, Pig==id[i])
   dsub[,by] <- i
   v <- rbind(v,dsub)
 }
 for (j in 1:ncol(dietox))
   class(v[,j]) <- class(dietox[,j])
 return(v) 
}


d <- unique(dietox$Pig)

v1 <- v2 <- v3 <- NULL
for (i in 1:length(d)){
 print(c(i,d[i]))
 dsub <- subset(dietox,Pig!=d[i])
 gee1 <- geeglm(mf, data=dsub, id=Pig, family=poisson("identity"),corstr="ar1")
 qpo1 <-glm(mf, data=dsub, family=quasipoisson("identity"))
 lm4 <- lm(mf, data=dsub)
 v1 <- rbind(v1,summary(gee1)$coef[,1])
 v2 <- rbind(v2,summary(qpo1)$coef[,1])
 v3 <- rbind(v3,summary(lm4)$coef[,1]) 
 }
 
SEgeej <- sqrt(apply(v1,2,var)*(nrow(v1)-1))
SEqpoj <- sqrt(apply(v2,2,var)*(nrow(v2)-1))
SElmj <- sqrt(apply(v3,2,var)*(nrow(v3)-1))

round(cbind(SEgeej,SEqpoj,SElmj,SEgee,SEqpo,SElm),3)
