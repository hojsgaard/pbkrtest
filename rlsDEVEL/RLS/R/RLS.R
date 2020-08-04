#' @title Recursive Least Squares
#' @description FIXME
#' @name rls
#' 


rls <- function(x, y=NULL, tvar=NULL, degree=1,  lambda=.99, b=NULL, Pi=NULL, na.action, optim=TRUE){

    cl <-  match.call()
    if (is.null(y)){
        y <- as.numeric(x)
        x <- seq_along(y)
    }
    if (is.null(tvar))
        tvar <- x
  
    if (optim){ ## Forgetting factor is estimated from data
        optfun <- function(lambda){
            ans <- rlsfit(x, y, tvar=tvar, degree=degree, lambda=lambda, b=b, Pi=Pi)
            e <- ans$fit[, "et1"]
            sum(e[!is.na(e)]^2)
        }
        
        optpar <- optimize(optfun, interval=c(0.95, 1))$minimum
        lambda <- optpar
    }

    ans <- rlsfit(x, y, degree=degree, lambda=lambda, b=b, Pi=Pi)
  
    ferr <- var(ans$fit[,"et1"])
    var  <- do.call(rbind, lapply(ans$Pi, diag))
    extra <- list(x=x,y=y,tvar=tvar, degree=degree, lambda=lambda, var=var, ferr=ferr, call=cl)
    ans <- c(extra, ans)
    class(ans) <- "rls"
    ans
}


rlsfit <- function(xx, yy, tvar=xx,  degree=1, lambda=0.99 , b=NULL, Pi=NULL){
  
  if (is.null(b))
    b <- rep(0, degree + 1)
  if (is.null(Pi)){
    Pi <- diag(50000, degree + 1)
  } else {
    if (is.numeric(Pi))
      Pi <- diag(Pi, degree + 1)
  }
  
    lenx <- length(xx)  
    J <- diag(1,degree + 1)
    X <- .makeBasis(xx, degree)
    
    coefmat <- matrix(NA, nrow=lenx, ncol=length(b))
    Piarray <- array(NA, c(degree + 1, degree + 1, lenx))
    Piarray <- vector("list", lenx)
    fitmat  <- matrix(NA, nrow=lenx, ncol=4)
    
###lam <- 1/(1+exp(-lambda))
    lam <- lambda
    lamvec <- lam^(c(0, abs(diff(tvar))))
    lamvec[1] <- lam

    for(tt in 1:lenx){
        xt <- X[tt, ]
        yt <- yy[tt]
        if (!is.na(yt)){
            k       <- (Pi %*% xt/lamvec[tt]) / as.numeric( 1 + t(xt) %*% Pi %*% xt /lamvec[tt]) 
            k       <- (Pi %*% xt) / as.numeric( lamvec[tt] + t(xt) %*% Pi %*% xt ) 
            ft      <- as.numeric(t(xt)%*%b )
            et1     <- yt-ft  ## 1 step prediction error
            
            b  <- b + k %*% et1
            
            Pi <- (J - k %*% t(xt)) %*% Pi/lamvec[tt]
            
            ##Piarray[,,tt] <- Pi
            Piarray[[tt]] <- Pi
            yt.hat <- as.numeric(t(xt) %*% b )
            et <- yt - yt.hat

            coefmat[tt, ] <- b
            fitmat[tt,] <- c(ft, et1, yt.hat, et)
        }
    }

    colnames(fitmat) <-c("ft", "et1", "yt.hat", "et")  
    ans <- list(coef=coefmat, fit=fitmat, X=X, Pi=Piarray, lambda=lambda)
    ans
}
