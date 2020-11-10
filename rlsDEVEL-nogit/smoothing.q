tn <- 40
tvar <- 1:tn 
xt <- .5*tvar + 0*tvar^2 + rnorm(tn,sd=1)


hh <- c(.5,.5)

k <- 0

ff <- function(xt,hh){
  l1 <- xt*hh[1]
  l2 <- c(NA,(xt*hh[2])[1:(tn-1)])
  l1+l2
}

s1 <-ff(xt,hh)
s1[1]<- xt[1]
s2 <- ff(s1,hh)
s2[1]<- xt[1]

plot(tvar,xt)
lines(tvar, s1,col=2)
lines(tvar, s2,col=3)
b.est <- -(s2-s1)/hh[2]

## Forecasts

f1 <- c(NA,s1[1:(tn-1)])
f2 <- c(NA,s2[1:(tn-1)])
f3 <- c(NA,s1[1:(tn-1)])+c(NA, b.est[1:(tn-1)])

plot(tvar,xt,ylim=c(-2,max(xt)))
abline(h=0)
lines(tvar, f1,col=2)
lines(tvar, f2,col=3)
lines(tvar, f3,col=5,lwd=2)

r1 <- xt-f1
r2 <- xt-f2
r3 <- xt-f3

lines(tvar, r1,col=2)
lines(tvar, r2,col=3)
lines(tvar, r3,col=5,lwd=2)



