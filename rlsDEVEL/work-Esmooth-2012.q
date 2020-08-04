## 
## Double ES
##
source("EsmoothFUN.R")

g0  = 1 
g1  = .3
g2  = 0.01
ss  = 5
tvar = 1:500
mu   <- g0 + g1*tvar + g2*tvar^2
mu <- c(rep(g0,100), mu)
yvar <- rnorm(length(mu), mean=mu, sd=ss)
tvar <- seq_along(yvar)

plot(yvar~tvar)

al <- .1
be <- 1-al

q1 <- be/(1-be)
q2 <- -be*(1+be)/(1-be)^2

S1 <- .SES(yvar, alpha=al)$y
S2 <- .SES(S1, alpha=al)$y
S3 <- .SES(S2, alpha=al)$y


par(mfrow=c(2,2))
plot(yvar~tvar)
plot(S1~tvar)
plot(S2~tvar)
plot(S3~tvar)

plot(yvar~tvar)
lines(S1~tvar)
lines(S2~tvar)
lines(S3~tvar)

plot(yvar~tvar)
lines((S1-S2)~tvar)
lines((S2-S3)~tvar)

lines((S1-2*S2+S3)~tvar)

plot(g2.hat)
plot(g1.hat)


g2.hat <- (S3-2*S2+S1)/(2*q1^2)
g1.hat <- -(S2 - S1 - g2.hat*q2 + 2*g2.hat*q1*tvar - 2*g2.hat*q1^2)/q1
g0.hat <- S1-(-g1.hat*q1+g2.hat*q2-2*g2.hat*q1*tvar)-g1.hat*tvar-g2.hat*tvar^2

plot(yvar~tvar)
mu.hat   <- g0.hat + g1.hat*tvar + g2.hat*tvar^2
lines(mu.hat~tvar)






yvar <- cumsum(rnorm(20))
sss<-.ses(yvar,alpha=0.5)

plot(yvar)
lines(sss)












library(zoo)

N     <- 60
tvar  <- 1:N
sd1    <- .1
ee    <- rnorm(N,sd=sd1)

cls1 <- mu1   <- rep(0,N)
cls1[] <- 1

cls2 <- mu2   <- rep(0,N)
cls2[] <- 1

mu2[c(21,31,32,33,41,42,43,44,45,46)] <- 5
cls2[c(21,31,34,41,47)] <- 2

cls3 <- mu3   <- rep(0,N)
cls3[] <- 1
mu3[c(21:30,41:50)] <- 5
cls3[c(21,31,41,51)] <- 2

cls4 <- mu4   <- rep(0,N)
cls4[] <- 1

mu4[11:20] <- 1:10/2
mu4[21:40] <- 5
mu4[41:50] <- (10:1)/2
cls4[c(11:20,41:50)] <- 3

ymat <- cbind(y1=mu1+ee, y2=mu2+ee, y3=mu3+ee, y4=mu4+ee)
matplot(ymat)

yall <- as.numeric(ymat)
cls  <- c(cls1,cls2,cls3,cls4)

plot(yall)

source("EsmoothFUN.q")

y <- c(rep(1,30), 1:20, rep(20,10)) + 10
y <- y + rnorm(length(y),sd=.5)
#y[15] <- 20
y
par(mfrow=c(3,1), mar=c(3, 4, 0, 2) + 0.1)
alpha <- 0.1
s0 <- .ses(y,alpha)
s1 <- .des(y,alpha)
plot(y, ylim=range(y,s0$y,s1$y))
lines(s0,col=1)
lines(s1,col=2)

alpha <- 0.2
s0 <- .ses(y,alpha)
s1 <- .des(y,alpha)
plot(y, ylim=range(y,s0$y,s1$y))
lines(s0,col=1)
lines(s1,col=2)

alpha <- 0.3
s0 <- .ses(y,alpha)
s1 <- .des(y,alpha)
plot(y, ylim=range(y,s0$y,s1$y))
lines(s0,col=1)
lines(s1,col=2)

acc <- c(0, diff(s1$b.t))
mm <- cbind(resid(s0),resid(s1),s1$b.t,acc)


mu <- c(rep(1,30), 1:20, rep(20,30)) + 10
y <- mu + rnorm(length(mu),sd=1)
y[15] <- 30
y

par(mfrow=c(3,1), mar=c(3, 4, 0, 2) + 0.1)
alpha <- 0.2
s0 <- .ses(y,alpha)
s1 <- .des(y,alpha)
plot(y, ylim=range(y,s0$y,s1$y))
lines(mu,col=1)
lines(s0,col=2)
lines(s1,col=3)


kk <- 5
matplot(scale(cbind(wsd(s1$y,kk),wsd(s1$b.t,kk))))
matplot((cbind(wsd(s1$y,kk),wsd(s1$b.t,kk))))
matplot(scale(cbind(s1$y,s1$b.t)))

kk <- 10

hh <- 3
ferr<-cbind(predict(s1,hh=hh)$ferr, predict(s0,hh=hh)$ferr)
matplot(ferr,type='l')



cbind(s1$y.obs-predict(s1)$y,s0$y.obs-predict(s0)$y)





#par(mfrow=c(2,1))
pmat <- cbind(p=s1$b.t, l=s1$b.t-2*wsd(s1$b,kk), u=s1$b.t+2*wsd(s1$b,kk))
matplot(pmat, type="l", lty=c(1,2,2))

wwald <- (s1$b.t/wsd(s1$b.t,kk))
plot(wwald,ylim=c(-10,10))
abline(h=c(-1,1)*qt(.975, kk))


alpha <- .1
kk    <- 10
s1 <- .des(y, alpha)

s2.y <- var(s1$y[1:kk])
s2.b <- var(s1$b[1:kk])
m.y  <- mean(s1$y[1:kk])

w.y <- (s1$y-m.y)^2 / s2.y
w.b <- s1$b^2 / s2.b

l.wald <- ((s1$y-m.y)/wsd(s1$y,kk))^2
b.wald <- (s1$b.t/wsd(s1$b.t,kk))^2
matplot(scale(cbind(l.wald, b.wald)),type='l')

rug((1:length(l.wald))[1-pchisq(l.wald,1) < .1])



w.b <- wmean(s1$b,kk)^2 / s2.b

plot((wmean(s1$b,kk))^2 / s2.b)



dof <- 5

w2.y <- wcumsum(w.y, dof)
w2.b <- wcumsum(w.b, dof)


wmat <- cbind(w2.y,w2.b)


matplot(1-pchisq(wmat, dof))





























