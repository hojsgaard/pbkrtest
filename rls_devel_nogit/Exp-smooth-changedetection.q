source('./EsmoothFUN.R')

.sd <- 15
yvar <- c(rnorm(100,sd=.sd),(1:100+rnorm(100,sd=.sd)))

tvar <- 1:100
yvar <- -.05*tvar + .9*log(tvar)
yvar <- yvar + rnorm(length(yvar),sd=.2)


tvar <- 1:100
yvar <- 0.01*tvar^2 + rnorm(length(tvar),sd=5)



source('./EsmoothFUN.R')
getalpha(yvar, FUN=.SES)
getalpha(yvar, FUN=.DES)


plot(tvar, yvar)

ss<-.SES(yvar,alpha=.5/2, fil=rep(1,3))
dd<-.DES(yvar,alpha=.2/2, fil=rep(1,3))

plot(ss)
lines(dd,col=2)

plot(tvar, residuals(ss))
plot(tvar, residuals(dd))


forecast(ss)
forecasterr(ss)

get.alpha(yvar)
get.alpha(yvar, FUN=.DES)

ss<-.SES(yvar,alpha=.45)
dd<-.DES(yvar,alpha=.20)

plot(ss)
lines(dd,col=2)







yvar <- c(NA,NA,NA,5,7,6,7,8)
yvar <- c(4,7,NA,5,7,6,7,8)
tvar <- c(1,3,5,6,9,10,11,12)
init <- 4

dd<-.DES(yvar,alpha=.1)
ss<-.SES(yvar,alpha=.1)


na.y  <- is.na(yvar)
tvar2 <- tvar[!na.y]
yvar2 <- yvar[!na.y]

dtvar2 <- diff(tvar2)
n2  <- length(dtvar2)
xx2 <- dd$xx[!na.y]
bb2 <- dd$bb[!na.y]

c(NA, xx2[1:n2] + bb2[1:n2]*dtvar2)
fe <- yvar2-c(NA, xx2[1:n2] + bb2[1:n2]*dtvar2)
sum(fe^2,na.rm=T)/sum(!is.na(fe))

forerr <- function(dd){
	yvar <- dd$y.obs
	tvar <- dd$x
	na.y  <- is.na(yvar)
	tvar2 <- tvar[!na.y]
	yvar2 <- yvar[!na.y]

	dtvar2 <- diff(tvar2)
	n2  <- length(dtvar2)
	xx2 <- dd$xx[!na.y]
	bb2 <- dd$bb[!na.y]

	fe <- yvar2-c(NA, xx2[1:n2] + bb2[1:n2]*dtvar2)
	sum(fe^2,na.rm=T)/sum(!is.na(fe))
}

forerr(dd)


fe <- ss$y.obs-c(NA,ss$y[1:(length(ss$x)-1)])
sum(fe^2,na.rm=T)/sum(!is.na(fe))



forcasterror <- function(object){
  if (inherits(object, "DES")){
		yvar <- object$y.obs
		tvar <- object$x
		na.y  <- is.na(yvar)
		tvar2 <- tvar[!na.y]
		yvar2 <- yvar[!na.y]

		dtvar2 <- diff(tvar2)
		n2  <- length(dtvar2)
		xx2 <- dd$xx[!na.y]
		bb2 <- dd$bb[!na.y]

		fe <- yvar2-c(NA, xx2[1:n2] + bb2[1:n2]*dtvar2)
		sum(fe^2,na.rm=T)/sum(!is.na(fe))
  
  
  } else {
	if (inherits(object, "SES")) {
		fe <- object$y.obs-c(NA,object$y[1:(length(object$x)-1)])
		sum(fe^2,na.rm=T)/sum(!is.na(fe))
	
	} else {
		stop()
	}
}





yvar <- as.numeric(Nile)
tvar <- seq_along(yvar)
init <- 1200

yvar <- c(4,7,NA,5,7,6,7,8)
yvar <- c(NA,NA,NA,5,7,6,7,8)
tvar <- c(1,3,5,6,9,10,11,12)
init <- 4

alpha=.2

na.y  <- is.na(yvar)
tvar2 <- tvar[!na.y]
yvar2 <- yvar[!na.y]
wgt2  <- .seswgt(tvar2, alpha)  
S1    <- .ses.core(yvar2, wgt2, init=init)

y.fit <- rep(NA,length(yvar))
y.fit[!na.y] <- S1

if(is.na(y.fit[1])){
	y.fit[1] <- init
}

## Forecast missing for ses
## Kræver y.fit
  for (ii in 2:length(y.fit)){
    if (is.na(y.fit[ii])){
		y.fit[ii] <- y.fit[ii-1]
	}
  }   

par(mfcol=c(2,2))
ans <- list(x=tvar, y=y.fit, y.obs=yvar, init=init)
plot(ans$x, ans$y.obs)
lines(ans, type='l')
#lines(ans$x, fitted(loess(yvar~tvar,span=.2, na.action=na.exclude)),col=2)
plot(ans$x, ans$y.obs-ans$y)


S2  <- .ses.core(S1, wgt2, init=S1[1])
xx2 <- 2*S1 - S2
bb2 <- (S1 - S2)*wgt2/(1-wgt2)

bb <- xx <- rep(NA, length(yvar))
xx[!is.na(yvar)] <- xx2
bb[!is.na(yvar)] <- bb2

## Forecast missing for des 
## Kræver xx2, bb2, tvar
y.forec <- rep(NA, length(yvar))
last    <- 1
if (is.na(xx[last])) {
	xx.last <- init
	bb.last <- 0
} else {
	xx.last <- xx[last]
	bb.last <- bb[last]
}
for (ii in 1:length(y.forec)){	
	if (is.na(xx[ii])){
	 y.forec[ii] <- xx.last+(tvar[ii]-tvar[last])*bb.last
	} else {
	 last <- ii
  	 xx.last <- xx[last]
	 bb.last <- bb[last]
	}
}
  
y.fit <- xx
y.fit[is.na(y.fit)] <- y.forec[!is.na(y.forec)]

ans <- list(x=tvar, y=y.fit, y.obs=yvar, init=init, xx=xx2, bb=bb2)
plot(ans$x, ans$y.obs)
lines(ans, type='l')
lines(ans$x, filter(ans$y,c(.5,.5),sides=1), type='l',col=3)
plot(ans$x, ans$y.obs-ans$y,ylim=c(-500,500))
  
lines(ans$x, fitted(loess(yvar~tvar,span=.2)),col=2)

  
bb <- ans$bb
cc <- bb
for (ii in 2:length(bb)){
  cc[ii] <- (bb[ii]+bb[ii-1])/2
}
cc

bb <- ans$bb
filter(bb,c(.5,.5),sides=1)













  
