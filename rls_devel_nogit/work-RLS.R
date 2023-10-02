library(RLS)
load_all("RLS")

tvar <- 1:20
b0 <- 0; b1 <- 1; b2 <- .1; s <- 1
yvar <- b0 + b1 * tvar + b2 * tvar^2 + rnorm(length(tvar), sd=s)

plot(tvar, yvar)

m <- rls(x=tvar, y=yvar, optim=F)
f <- fitted(m)
plot(tvar, yvar)
lines(tvar, f)

mm <- lm(yvar ~ tvar)
ff <- fitted(mm)
lines(tvar, ff)

coef(m)
coef(mm)








## Recursive least squares
##

nn <- 10000
lam <- 0.98
(1-lam^nn)/(1-lam)

sum(wgt <- rev(lam^((1:nn)-1))  )





tvar <- seq(-4,4,.1)
yvar <- tvar^2 + rnorm(length(tvar), sd=.1)
plot(tvar, yvar)
m1 <- lm(yvar~tvar)
lines(tvar,fitted(m1))
lam <- 0.5
wgt <- rev(lam^(seq_along(tvar)-1))  
m2 <- lm(yvar~tvar, weight=wgt)
lines(tvar,fitted(m2))
X <- model.matrix(m2)
solve(t(X) %*% diag(wgt) %*% X) %*% t(X) %*% diag(wgt) %*% yvar


ans <- list()
for (ii in 3:length(tvar)){
tvar2 <- tvar[1:ii]
X <- cbind(1,tvar2)
lam <- 1
wgt <- rev(lam^(seq_along(tvar2)-1))  
ans[[ii]] <- diag(solve(t(X) %*% diag(wgt) %*% X))
}


ans <- do.call(rbind,ans)
diff(ans)




## package.skeleton("RLS", code_files=c("RLS.R"), namespace=TRUE)
library(RLS)
load_all("RLS")


nn    <- 50
tvar  <- 1:nn
lam   <- .7
lamvec <- lam^(c(0,abs(diff(tvar))))
X  <- cbind(1,tvar)
J  <- diag(1,2)

tt <- 5
wgt <- rev(lam^(seq_along(1:tt)-1))  
Wn.bf <- t(X[1:tt,])%*%diag(wgt)%*%X[1:tt,]

tt <- 4
wgt <- rev(lam^(seq_along(1:tt)-1))  
Wn <- t(X[1:tt,]) %*% diag(wgt) %*% X[1:tt,]
Pn.start <- solve(Wn)

tt <- 5
Pn <- Pn.start
xt <- t(X[tt,,drop=FALSE])
Wn.new <- xt %*% t(xt) + Wn *lam
Pn.new.ck <- solve(Wn.new)

k       <- ( Pn %*% xt ) / as.numeric( lam + t(xt) %*% Pn %*% xt  ) 
Pn.new  <- (J - k %*% t(xt)) %*% (Pn / lam)

tt <- 5
ans1 <- list()

Pn <- Pn.start
for (tt in 5:nn){
  xt <- t(X[tt,,drop=FALSE])
  k      <- ( Pn %*% xt ) / as.numeric( lam + t(xt) %*% Pn %*% xt  ) 
  Pn  <- (J - k %*% t(xt)) %*% (Pn * lam)
  ans1[[tt]] <- Pn 
}
ans1 <- ans1[c(lapply(ans1, function(x) !is.null(x) ),recursive=T)]
ans1 <- do.call(rbind, lapply(ans1, diag))
matplot(ans1,type='l')

ans2 <- list()
for (tt in 5:nn){
  wgt <- rev(lam^(seq_along(1:tt)-1))  
  PP <- t(X[1:tt,])%*%diag(wgt)%*%X[1:tt,]
  ans2[[tt]] <- solve(PP)
}

ans2 <- ans2[c(lapply(ans2, function(x) !is.null(x) ),recursive=T)]
ans2 <- do.call(rbind, lapply(ans2, diag))

matplot(ans1,type='l', lty=1)
matlines(ans2, lty=4)









ee <- 3

ans <- NULL
for (ee in 3:length(yy)){  
  yyy <- yy[1:ee]
  xxx <- xx[1:length(yyy)]
  wgt <- rev(.2^seq_along(yyy))
  mm <- lm(yyy~xxx, weight=wgt)
  modmat <- model.matrix(mm)
  #ans <- c(ans, diag(vcov(mm)))
  ans <- c(ans, diag(solve(t(modmat) %*% diag(1/wgt) %*% modmat)))

}
ans <- matrix(ans, nc=2, byrow=T)
matlines(ans)

matplot(ans)


t(modmat) %*% diag(wgt) %*% modmat



load("Prog.RData")

tvar <- prog[,1]
yvar <- prog[,2]

plot(tvar,yvar)

rtvar <- rev(tvar)
ryvar <- rev(yvar)


yvar <- as.numeric(nhtemp)
tvar <- seq_along(yvar)

yvar <- vandrivers$y
tvar <- seq_along(yvar)

srcit()
plot(tvar,yvar)

r<-rls2(tvar, yvar, degree=1, eta=.98,optim=T)
lines(tvar, fitted(r))
r$ferr
r$lam



etvec <- seq(0.01, 0.99, .02)
ervec <- rep(NA, length(etvec))
for (ii in seq_along(etvec))
  ervec[ii] <- rls2(tvar, yvar, eta=etvec[ii], optim=F)$err
plot(etvec, ervec)


lines(tvar,fitted(r),type='b',pch='+')
plot(tvar, residuals(r))

srcit()
par(mfrow=c(2,1))
r<-rls(rtvar, ryvar, degree=0,lam=.9,optim=F)

plot(r)
lines(rtvar,fitted(r),type='b',pch='+')
plot(rtvar, residuals(r)^2)
























srcit()
yy <- as.numeric(Nile)
xx <- seq_along(yy)
lm(yy~xx)

srcit()
r <- rls(xx,yy,degree=2,lam=.96, optim=F,b=c(1000,0,0), Pi=100)
r1<-fitted(r)
srcit()
plot(r)
lines(fitted(r),col=2)
lines(fitted(r, type="static"),col=3)
m<-scale(t(apply(r$Pi,3,diag)))
matplot(diff(m)[-c(1:10),])

r <- rls(xx,yy,degree=2,lam=.9, optim=F)
r2<-fitted(r)

m<-t(apply(r$Pi,3,diag))







srcit()
plot(residuals(r),col=2)
lines(residuals(r, type="static"),col=3)


xxx <- 21:30

srcit()
predict(r, newdata=xxx)
predict(r, newdata=xxx, at=100)

srcit()
predict(r, newdata=xxx, tvar=21:30)
predict(r, newdata=xxx, at=21)





object <- r

forecastRLS(r, type="er")







par(mfrow=c(2,2))
plot(xx,yy)
lines(xx, fitted(r),type="b",col="red", pch='+')
plot(xx, residuals(r),type='b'); abline(h=0)


plot(xx, residuals(r)^2)
plot(xx, cumsum(residuals(r)))





yy2 <- yy
yy2[sample(length(yy2), 30)] <- NA


ans <- rlsfit(xx,yy, lam=.9, degree=2)
plot(xx,yy)

lines(xx, ans[,"yt.hat"])

plot(xx,ans[,"et"], type="l")

















parmt <- list(b=b, Pi=Pi)
for(tt in 1:lenx){
  xt <- X[tt,]
  yt <- y[tt]
  parmt <- upd(yt, xt, parmt, lam)
  cat("tt:", tt, "b:", parmt$b,"\n")
}


upd <- function(yt, xt, parmt, lam){
  k      <- (parmt$Pi %*% xt/lam)/as.numeric(1+t(xt)%*%parmt$Pi%*%xt/lam) 
  yt.hat <- as.numeric(t(xt)%*%parmt$b )
  et     <- yt-yt.hat
  
  b.new  <- parmt$b + k * et
  Pi.new <- (J - k%*%t(xt))%*%parmt$Pi/lam
  list(b=b.new, Pi=Pi.new)
}



lm(y~x)
