.ses.core <- function(yvar2, wgt, init=0){
  S1 <- yvar2
  S1[1] <- init	
  for (ii in 2:length(yvar2)){
    S1[ii] <- S1[ii-1] + wgt[ii]*(yvar2[ii] - S1[ii-1])
  }
  S1
}

.seswgt <- function(tvar, alpha){
  dd <- c(0, abs(diff(tvar)))
  #print(dd)
  1-((1-alpha)^dd)
}

#lines(ans$x, filter(ans$y,c(.5,.5),sides=1), type='l',col=3)

.SES <- function(yvar, tvar=seq_along(yvar), alpha=.3, init=NULL, filter.=NULL){

  na.y  <- is.na(yvar)
  #cat(sprintf("Missing values=%i; first missing value=%i\n", sum(na.y), which(na.y)[1]))
  tvar2 <- tvar[!na.y]
  yvar2 <- yvar[!na.y]
  wgt2  <- .seswgt(tvar2, alpha)
  if (is.null(init))
    init <- yvar2[1]
  
  S1    <- .ses.core(yvar2, wgt2, init=init)
  y.fit <- rep(NA,length(yvar))
  y.fit[!na.y] <- S1
  
  if(is.na(y.fit[1])){
    y.fit[1] <- init
  }
  
  ## Forecast missing for ses; Kræver y.fit
  for (ii in 2:length(y.fit)){
    if (is.na(y.fit[ii])){
      y.fit[ii] <- y.fit[ii-1]
    }
  }   

  
  if (!is.null(filter.)){
    filter. <- filter./sum(filter.)
    y.fit <- filter(y.fit,filter.,sides=1)
  }

  ans <- list(x=tvar, y=y.fit, y.obs=yvar, init=init)
  class(ans) <- c("SES","ES")
  ans
}

.DES <- function(yvar, tvar=seq_along(yvar), alpha=.3, init=NULL,filter.=NULL){

  na.y  <- is.na(yvar)
  #cat(sprintf("Missing values=%i; first missing value=%i\n", sum(na.y), which(na.y)[1]))
  tvar2 <- tvar[!na.y]
  yvar2 <- yvar[!na.y]
  wgt2  <- .seswgt(tvar2, alpha)
  if (is.null(init))
    init <- yvar2[1]

  S1  <- .ses.core(yvar2, wgt2, init=init)
  S2  <- .ses.core(S1, wgt2, init=S1[1])

  xx2 <- 2*S1 - S2
  bb2 <- (S1 - S2)*wgt2/(1-wgt2)
  
  bb <- xx <- rep(NA, length(yvar))
  xx[!is.na(yvar)] <- xx2
  bb[!is.na(yvar)] <- bb2
  
  ## Forecast missing for des 
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

  if (!is.null(filter.)){
    filter. <- filter./sum(filter.)
    y.fit <- filter(y.fit,filter.,sides=1)
  }
  
  ans <- list(x=tvar, y=y.fit, y.obs=yvar, xx=xx, bb=bb, init=init)

  class(ans) <- c("DES","ES")
  ans
}

residuals.ES <- function(object,...){
  object$y.obs-object$y
}

fitted.ES <- function(object,...){
  object$y
}

plot.ES <- function(x,...){
  plot.default(x$x, x$y.obs)
  lines(x$x,x$y)
}

getalpha <- function(yvar, tvar=seq_along(yvar), FUN=.SES, n.ahead=1){
  ff <- function(alpha, yvar, tvar, n.ahead=1){
    ss <- FUN(yvar, tvar, alpha=alpha)
    forecasterror(ss)
  }
  aa <- optimize(ff, interval=c(0,1), yvar=yvar,tvar=tvar, n.ahead=n.ahead)$minimum
  aa
}

forecasterror <- function(object){
  if (inherits(object, "DES")){
    yvar <- object$y.obs
    tvar <- object$x
    na.y  <- is.na(yvar)
    tvar2 <- tvar[!na.y]
    yvar2 <- yvar[!na.y]
    
    dtvar2 <- diff(tvar2)
    n2  <- length(dtvar2)
    xx2 <- object$xx[!na.y]
    bb2 <- object$bb[!na.y]
    
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
}
  














## forecast <- function(ss, n.ahead=1){
##   list(x=ss$x,
##        y=c(rep(NA, n.ahead), ss$y[1:(length(ss$y)-n.ahead)]))
## }

## forecasterr <- function(ss,n.ahead=1){
##   fe <- ss$y.obs-forecast(ss,n.ahead=n.ahead)$y
##   sum(fe^2,na.rm=T)/sum(!is.na(fe))
## }

## get.alpha <- function(yvar, tvar=seq_along(yvar), FUN=.SES, n.ahead=1){
##   ff <- function(alpha, yvar, tvar, n.ahead=1){
##     ss <- FUN(yvar, tvar, alpha=alpha)
##     forecasterr(ss)
##   }
##   aa <- optimize(ff, interval=c(0,1), yvar=yvar,tvar=tvar, n.ahead=n.ahead)$minimum
##   aa
## }



## Exponential smoothing
##

## .ses <- function(yvar, tvar=seq_along(yvar), alpha=.3){
## #  print(yvar)
## #  print(tvar)
##   S1 <- yvar
##   S1[1] <- mean(yvar[1:30], na.rm=T)
##   wgt<- .seswgt(tvar, alpha)
##   #print(wgt)
##   for (ii in 2:length(yvar)){
##     S1[ii] <- S1[ii-1] + wgt[ii]*(yvar[ii] - S1[ii-1])
##   }
##   ans <- list(x=tvar, y=S1, y.obs=yvar)
##   class(ans) <- c("ses","mes")
##   ans
## }

## .des <- function(yvar, tvar=seq_along(yvar), alpha=.3){
##   S1 <- .ses(yvar, tvar=tvar, alpha=alpha)$y
##   S2 <- .ses(S1,tvar=tvar, alpha=alpha)$y

##   yvar.t  <- 2*S1-S2
##   b.t  <- (alpha/(1-alpha)) * (S1-S2)

##   ans <- list(x=tvar,y=yvar.t, b.t=b.t, y.obs=yvar)
##   class(ans) <- c("des","mes")
##   ans
## }

## residuals.mes <- function(object,...){
##   object$y.obs-object$y
## }

## fitted.mes <- function(object,...){
##   object$y
## }

## predict.ses <- function(object, hh=1){
##   fh  <- c(rep(NA,hh),object$y)[object$x]
##   ferr <- object$y-fh
##   fh  <- list(x=object$x, y=fh, ferr=ferr, h=hh)
##   fh
## }

## predict.des <- function(object, hh=1){
##   fh  <- c(rep(NA,hh),object$y+object$b.t*hh)[object$x]
##   ferr <- object$y-fh
##   fh  <- list(x=object$x, y=fh, ferr=ferr, h=hh)
##   fh
## }

## print.mes <- function(x, ...){
##   print(str(x))
##   return(invisible(x))
## }




## ###
## ### Some utilities seemingly used in connection with exponential smoothing (don't quite remember)
## ### 
## wcumsum <- function(x,k){
##   x1 <- x2 <- cumsum(x)
##   for (ii in (k+1):length(x)){
##     x2[ii] <- x1[ii]-x1[(ii-k)]
##   }
##   x2
## }

## wmean <- function(x,k){   
##   rollapply(zoo(x),k,mean,na.rm=T, na.pad=T,align="right")
## }

## wvar <- function(x,k){
##   rollapply(zoo(x^2),k,mean,na.rm=T, na.pad=T,align="right") - 
##   rollapply(zoo(x),  k,mean,na.rm=T, na.pad=T,align="right")^2
## }


## wsd <- function(x,k){
##   sqrt(wvar(x,k))
## }
