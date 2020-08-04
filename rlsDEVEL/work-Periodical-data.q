## Find periods of reproduction data
#
#1) Fjern starten af alle perioder
#2) Fit spectrum og find de køer hvor cykellængden L er < 25
#3) Fit cos + sin til data og find estimerede tider for minimum (dvs. find faseskift f og
#   så beregn f + k * L for k=1,2,3,4... op til hvor f+k*L bliver større end tmax.
#4) Dette giver en ny variabel "tid i cycle" (tic) der løber fra 0 til L (tvar mod L).
#5) Sæt tic på dataframe
#6) Så er der problemet med at få støjen til sidst sorteret ud, men måske skal vi bare blæse
#   på det.
#

getSpec <- function(yvar,tvar){
  #plot(tvar,yvar)
  #lines(tvar,fitted(kk))
  t.new <- seq(min(tvar),max(tvar))
  y.new <- predict(smooth.spline(tvar,yvar),x=t.new)$y
  #lines(t.new,y.new)
  sp<-spectrum(y.new,plot=F)
  plot(sp)
  1/sp$freq[which.maxn(sp$spec,5)]
}

load("reproplus1.Rdata")
reproplus1 <- subset(reproplus1, dfo>=0)
library(lattice)
library(doBy)
reproplus1$cownum <- factor(reproplus1$cownum)
dim(reproplus1)
xyplot(rprog~tfrc2|cownum, data=reproplus1,col='black',type=c("p","smooth"),span=.1,cex=.5,scale=list(x="free"))


wd  <- subset(reproplus1, cownum=="73")
tvar  <- wd$tfrc2
yvar  <- scale(wd$rprog)
plot(tvar,cut(yvar,5))
sss <- smooth.spline(tvar,as.numeric(cut(yvar,5)))
tnew <- seq(min(tvar),max(tvar))
plot(tnew, predict(sss,tnew)$y)



plot()
plot(tvar,cut(smooth.spline(tvar,as.numeric(cut(yvar,5)))$y,5))

as.numeric(cut(smooth.spline(tvar,as.numeric(cut(yvar,5)))$y,5))







zzz<-lapplyBy(~cownum, data=reproplus1, function(wd){
  wd  <- orderBy(~tfrc2, data=wd)

  yvar  <- wd$rprog
  tvar  <- wd$tfrc2
  plot(tvar, yvar, type='p')

  spec <- getSpec(yvar,tvar)
  spec

  ddd <- as.data.frame(cbind(yvar=yvar, cs=cos(2*pi*tvar/spec[1]), ss=sin(2*pi*tvar/spec[1])))
  mmm <- lm(yvar~cs+ss, data=ddd)
  pshift <- atan(coef(mmm)[3]/coef(mmm)[2]) # Check dette
  aaa <- pshift + (0:10)*spec[1]

  tic <- round(tvar,2) %% spec[1] # time in cycle...
  cycnum <- colSums(1*(do.call(rbind, lapply(aaa, function(tt) tt<tvar))))
  xyplot(yvar~tic, groups=cycnum,type='l')
  wd$tic <- tic
  wd$cycnum <- cycnum
  wd$spec   <- spec[1]
  wd
})

zzz2 <- do.call(rbind, zzz)

uuu <- subset(zzz2, spec<25)

xyplot(rprog~tfrc2|cownum, data=uuu,col='black',type=c("p","smooth"),span=.1,cex=.5,scale=list(x="free"))

yvar  <- uuu$rprog
tvar  <- uuu$tfrc2
ticc <- cut(uuu$tic,breaks=20)
uuu$ticc  <- ticc

xyplot(yvar~ticc,groups=cbind(cownum,cycnum), data=uuu,type="'")
plot(tapply(yvar, ticc, median),type='l')


plot(tvar, yvar, type='p')
tnew <- seq(min(tvar), max(tvar),by=.1)
ddd2 <- as.data.frame(cbind(cs=cos(2*pi*tnew/spec[1]), ss=sin(2*pi*tnew/spec[1])))
lines(tnew,predict(mmm, newdata=ddd2))



abline(v=aaa)
ticc <- cut(tic,breaks=10)
plot(tic, yvar,type='l')















source("EsmoothFUN.R")
sss<-.ses(yvar,tvar,alpha=0.05)
par(mfrow=c(2,4))
plot(tvar,yvar,type="p", col=2,lwd=2,lty=2)
lines(sss)
rss <- residuals(sss)
plot(tvar,rss)
plot(tvar,rss^2)


nyvar <-  rev(yvar)
ntvar <- -rev(tvar)
nsss<-.ses(nyvar,ntvar,alpha=0.1)
plot(ntvar,nyvar,type="p", col=2,lwd=2,lty=2)
lines(nsss)
nrss <- residuals(nsss)
plot(ntvar,nrss)
plot(ntvar,nrss^2)

ll <- tvar[which((rss^2)>100)[1]]
uu <- -ntvar[which((nrss^2)>100)[1]]

plot(tvar,yvar,type="p", col=2,lwd=2,lty=2)
abline(v=c(ll,uu))


wd <- subset(reproplus1, cownum==67)
yvar <- wd$rprog[25:110]
tvar <- wd$itfc[25:110]
  plot(tvar,yvar)





par(mfrow=c(8,6),mar=c(0,1,1,1))

use <- c("17","51","55","61","67","65","6","37")
for (ii in seq_along(use)){
  wd <- subset(reproplus1, cownum==use[ii])

  yvar <- wd$rprog
  tvar <- wd$itfc

  plot(tvar,yvar)
  ssp <- smooth.spline(tvar,yvar)
  lines(ssp)

  xx  <- ssp$x
  yy  <- ssp$y

  sf  <- splinefun(xx, yy)
  D0 <- sf(xx, deriv=0)
  D1 <- sf(xx, deriv=1)
  D2 <- sf(xx, deriv=2)

  #plot(tvar, D0)
  plot(tvar, scale(D1))
  plot(tvar, scale(D1)^2)

  plot(tvar, scale(D2))
  plot(tvar, scale(D2)^2)
  hist(D2^2,prob=T)
}







