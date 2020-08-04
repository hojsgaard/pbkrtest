
rls <- function(x,y=NULL, degree=1, lambda=0.8, na.action, optim=TRUE){

  if (is.null(y)){
    y <- as.numeric(x)
    x <- seq_along(y)
  }
  
  if (optim){ ## Forgetting factor is estimated from data
    f <- function(lam){
      ans <- rlsfit(x,y, lam=lam, degree=degree)
      e <- ans[,"et1"]
      sum(e[!is.na(e)]^2)
    }
    optpar <- optim(lambda, f, method="L-BFGS-B", lower=0, upper=1)$par
    cat("optpar:", optpar, "\n")
    ans <- rlsfit(x, y, lam=optpar, degree=degree)
  } else {
    ans <- rlsfit(x, y, lam=lambda, degree=degree)
  }

  class(ans) <- "rls"
  return(ans)
}

rlsfit <- function(xx, yy, b, Pi, lam=0.5, degree=1){
  
  lenx <- length(xx)
  
  J <- diag(1,degree+1)
  
  if(degree==0){
    X <- matrix(1, nrow=lenx, ncol=1)
  } else {
    if (degree==1){
      X <- cbind(1,xx)
    } else {
      X <- cbind(1,xx,xx^2)
    }
  }
  
  if (missing(Pi))
    Pi <- diag(100,degree+1)
  if (missing(b))
    b <- rep(0, degree+1)
  
  ans1 <- matrix(NA, nrow=lenx, ncol=length(b))
  ans2 <- matrix(NA, nrow=lenx, ncol=4)
  colnames(ans2) <-c("yt1.hat", "yt.hat", "et1", "et")
    
  for(tt in 1:lenx){
    xt <- X[tt,]
    yt <- yy[tt]
    if (!is.na(yt)){
      k      <- (Pi %*% xt/lam)/as.numeric(1+t(xt)%*%Pi%*%xt/lam) 
      yt1.hat <- as.numeric(t(xt)%*%b )
      et1     <- yt-yt1.hat ## 1 step prediction error
      
      b  <- b + k * et1
      Pi <- (J - k%*%t(xt))%*%Pi/lam
      
      yt.hat <- as.numeric(t(xt)%*%b )
      et <- yt - yt.hat

      ans1[tt,] <- b
      ans2[tt,] <- c(yt1.hat, yt.hat, et1, et)
      ##cat("tt:", tt, "b:", b, "yt.hat", yt.hat,"yt1.hat", yt1.hat,"\n")
    }
  }
  ans <- cbind(ans1, ans2)
  ans
}


fitted.rls <- function(x,...){
  x[,"yt.hat"]
}

residuals.rls <- function(x,...){
  x[,"et"]
}
