### R code from vignette source 'Exp-smooth-changedetection.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: Exp-smooth-changedetection.Rnw:37-38
###################################################
source('../../EsmoothFUN.R')


###################################################
### code chunk number 2: chunk0 (eval = FALSE)
###################################################
## plot(y,ylim=c(5,25),pch='.',cex=2)
## lines(mu, col='blue')
## lines(.ses(y,alpha=alpha),col=1)
## lines(.des(y,alpha=alpha),col=2)


###################################################
### code chunk number 3: Exp-smooth-changedetection.Rnw:195-236
###################################################
#set.seed(12345)
eps <- rnorm(60, sd=1.5)
vl <- function(){abline(v=ch,col='cyan')}
hl <- function(){abline(h=0,col='pink')}
alpha <- .15
par(mfcol=c(2,2), mar=c(1, 4, 0, 1) + 0.1)
###########################################

mu1 <- rep(10,60)
y1  <- mu1 + eps
ch1 <- NA
ch  <- ch1
mu <- mu1
y <- y1

plot(y,ylim=c(5,25),pch='.',cex=2)
lines(mu, col='blue')
lines(.ses(y,alpha=alpha),col=1)
lines(.des(y,alpha=alpha),col=2)

mu2 <- rep(10,60)
mu2[31] <- 20
ch2 <- 31
y2  <- mu2 + eps
mu <- mu2
y <- y2
ch <- ch2
plot(y,ylim=c(5,25),pch='.',cex=2)
lines(mu, col='blue')
lines(.ses(y,alpha=alpha),col=1)
lines(.des(y,alpha=alpha),col=2)

mu3 <- c(rep(10,30), rep(20,30))
ch3 <- 31
y3  <- mu3 + eps
mu <- mu3
y <- y3
ch <- ch3
plot(y,ylim=c(5,25),pch='.',cex=2)
lines(mu, col='blue')
lines(.ses(y,alpha=alpha),col=1)
lines(.des(y,alpha=alpha),col=2)
  
mu4 <- c(rep(10,20), 10+(1:20)/2, rep(20,20))
ch4 <- c(21,41)
y4  <- mu4 + eps
mu <- mu4
y <- y4
ch<-ch4
plot(y,ylim=c(5,25),pch='.',cex=2)
lines(mu, col='blue')
lines(.ses(y,alpha=alpha),col=1)
lines(.des(y,alpha=alpha),col=2)


###################################################
### code chunk number 4: forecerr (eval = FALSE)
###################################################
## s1 <- .des(y,alpha=alpha)
## 
## ferr<-as.data.frame(cbind(s1$y.obs-s1$y, predict(s1,hh=hh)$ferr))
## ferr$diff <- ferr[,1]-ferr[,2]
## se <- sd(ferr$diff[1:15],na.rm=T)
## dif <- ferr$diff
## plot(dif, pch='.',cex=3,ylim=c(-3,6),type='l'); vl();
## abline(h=c(-2*se,2*se))
## 
## #dif <- s1$y.obs-s1$y - predict(s1,hh=4)$ferr
## #lines(dif,col=2,pch='.',cex=3)


###################################################
### code chunk number 5: Exp-smooth-changedetection.Rnw:298-314
###################################################
hh    <- 1
par(mfcol=c(2,2), mar=c(1, 4, 0, 1) + 0.1)
###########################################

y<-y1
ch<-ch1
s1 <- .des(y,alpha=alpha)

ferr<-as.data.frame(cbind(s1$y.obs-s1$y, predict(s1,hh=hh)$ferr))
ferr$diff <- ferr[,1]-ferr[,2]
se <- sd(ferr$diff[1:15],na.rm=T)
dif <- ferr$diff
plot(dif, pch='.',cex=3,ylim=c(-3,6),type='l'); vl();
abline(h=c(-2*se,2*se))

#dif <- s1$y.obs-s1$y - predict(s1,hh=4)$ferr
#lines(dif,col=2,pch='.',cex=3)
y<-y2
ch<-ch2
s1 <- .des(y,alpha=alpha)

ferr<-as.data.frame(cbind(s1$y.obs-s1$y, predict(s1,hh=hh)$ferr))
ferr$diff <- ferr[,1]-ferr[,2]
se <- sd(ferr$diff[1:15],na.rm=T)
dif <- ferr$diff
plot(dif, pch='.',cex=3,ylim=c(-3,6),type='l'); vl();
abline(h=c(-2*se,2*se))

#dif <- s1$y.obs-s1$y - predict(s1,hh=4)$ferr
#lines(dif,col=2,pch='.',cex=3)
y<-y3
ch<-ch3
s1 <- .des(y,alpha=alpha)

ferr<-as.data.frame(cbind(s1$y.obs-s1$y, predict(s1,hh=hh)$ferr))
ferr$diff <- ferr[,1]-ferr[,2]
se <- sd(ferr$diff[1:15],na.rm=T)
dif <- ferr$diff
plot(dif, pch='.',cex=3,ylim=c(-3,6),type='l'); vl();
abline(h=c(-2*se,2*se))

#dif <- s1$y.obs-s1$y - predict(s1,hh=4)$ferr
#lines(dif,col=2,pch='.',cex=3)
y<-y4
ch<-ch4
s1 <- .des(y,alpha=alpha)

ferr<-as.data.frame(cbind(s1$y.obs-s1$y, predict(s1,hh=hh)$ferr))
ferr$diff <- ferr[,1]-ferr[,2]
se <- sd(ferr$diff[1:15],na.rm=T)
dif <- ferr$diff
plot(dif, pch='.',cex=3,ylim=c(-3,6),type='l'); vl();
abline(h=c(-2*se,2*se))

#dif <- s1$y.obs-s1$y - predict(s1,hh=4)$ferr
#lines(dif,col=2,pch='.',cex=3)


###################################################
### code chunk number 6: levelc (eval = FALSE)
###################################################
## s1 <- .des(y,alpha=alpha)
## 
## m.y <- mean(s1$y[1:20])
## v.y <- var(s1$y[1:20])
## 
## 
## w  <- (s1$y - m.y)/sqrt(v.y)
## plot(w,pch='.',cex=3,ylim=c(-3,30))
## abline(h=c(-2*sqrt(v.y),2*sqrt(v.y)));vl()


###################################################
### code chunk number 7: Exp-smooth-changedetection.Rnw:339-355
###################################################
hh    <- 1
par(mfcol=c(2,2), mar=c(1, 4, 0, 1) + 0.1)
###########################################

y<-y1
ch<-ch1
s1 <- .des(y,alpha=alpha)

m.y <- mean(s1$y[1:20])
v.y <- var(s1$y[1:20])


w  <- (s1$y - m.y)/sqrt(v.y)
plot(w,pch='.',cex=3,ylim=c(-3,30))
abline(h=c(-2*sqrt(v.y),2*sqrt(v.y)));vl()
y<-y2
ch<-ch2
s1 <- .des(y,alpha=alpha)

m.y <- mean(s1$y[1:20])
v.y <- var(s1$y[1:20])


w  <- (s1$y - m.y)/sqrt(v.y)
plot(w,pch='.',cex=3,ylim=c(-3,30))
abline(h=c(-2*sqrt(v.y),2*sqrt(v.y)));vl()
y<-y3
ch<-ch3
s1 <- .des(y,alpha=alpha)

m.y <- mean(s1$y[1:20])
v.y <- var(s1$y[1:20])


w  <- (s1$y - m.y)/sqrt(v.y)
plot(w,pch='.',cex=3,ylim=c(-3,30))
abline(h=c(-2*sqrt(v.y),2*sqrt(v.y)));vl()
y<-y4
ch<-ch4
s1 <- .des(y,alpha=alpha)

m.y <- mean(s1$y[1:20])
v.y <- var(s1$y[1:20])


w  <- (s1$y - m.y)/sqrt(v.y)
plot(w,pch='.',cex=3,ylim=c(-3,30))
abline(h=c(-2*sqrt(v.y),2*sqrt(v.y)));vl()


