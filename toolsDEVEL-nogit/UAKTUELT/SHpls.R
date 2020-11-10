### My PLS stuff

sh.pls      <- function(y,X,K=1,data){
  #cat("sh.pls\n")
  X.data <- data[,X];
  y.data <- data[,y];
  MX          <- matrix(rep(mean(X.data),nrow(X.data)),ncol=ncol(X.data),byrow=T)
  My          <- rep(mean(y.data),length(y.data))
  beta.pls    <- pls1a(X.data-MX, y.data-My, K=K)
  #print(mean(X.data)); print(beta.pls);  print(mean(X.data)*beta.pls)
  correction <- sum(mean(X.data)*beta.pls)
  beta2.pls   <- c(mean(y.data)-correction, beta.pls)
  names(beta2.pls) <- c("(Intercept)", X)
  result <- list("coefficients"=beta2.pls,"components"=K, text=paste("PLS",K,sep=''),
                 "x"=X, "y"=y)
  class(result) <- "pls.display"
  return(result)
}

print.pls.display <- function(x, ...){
  cat("Info: ", x$text, "\n")
  cat("y:    ", x$y, "\n")
  cat("x:    ", x$x, "\n")
  cat("ncomp:", x$comp, "\n")
  cat("coeffcients:\n")
  print(round(x$coef,5))

}
predict.pls.display <- function(object, data=NULL){
  #cat("predict.pls.display\n")
  reg.coef  <- as.vector(unclass(object$coef[-1]))
  intercept <- as.vector(unclass(object$coef[1]))  
  sim2x <- data[,object$x]
  reg.coef <- matrix(reg.coef, ncol=1)
  y.hat <- (as.matrix(sim2x) %*% reg.coef) + rep(intercept, nrow(sim2x))
  value <- as.numeric(y.hat)
  return(value)
}

mspe <- function(object, data) UseMethod("mspe")

mspe.default <- function(object, data){
  cat("mspe.default\n")
  y.hat <- predict(object,data)
  switch(class(object),
         "lm"         = { resp  <- paste(attr(object$term,"predvars")[[2]]) },
         "pls.display"= { resp <- object$y })
  y      <- data[,match(resp, names(data))] 
  r      <- as.data.frame(cbind(y, y.hat, y-y.hat, (y-y.hat)^2))
  colnames(r) <- c("obs", "predicted", "residual", "sq.residual")
  MPE   <- mean(r$residual)
  MSPE  <- mean(r$sq.residual)
  RMSPE <- sqrt(mean(r$sq.residual))
  value <- as.data.frame(cbind(MPE, MSPE, RMSPE))
  names(value)<- colnames(value)
  attr(value, "predicted") <- r
  return(value)    
}

print.mspe <- function(x,...){
  print(x$summary)
}




plsHelland <- function(x,y,K=min(dx[1]-1,dx[2])){
  dx  <- dim(x)
  m.x <- apply(x,2,mean)
  m.y <- mean(y)
  ##Sxx <- cov(x)
  Sxx <- var(x)
  Sxy <- cov(x,y)
  ##Init
  x1 <- scale(x,scale=F); y1 <- scale(y,scale=F)
  Score <- NULL; Beta <- NULL; Load <-NULL

  for(k in 1:K){ ##Iteration                                        
    A1 <- cov(x1,y1)
    w1 <- x1 %*% A1   ## w1 is the scores
    Score <- cbind(Score,w1)
    Load  <- cbind(Load,A1) ## A is the loading
    X1 <- cbind(rep(1,length(y1)),w1)
    H  <- solve(t(X1)%*%X1)%*%t(X1)
    ##Regress x and y on w1
    by <- H%*%y1
    bX <- H%*%x1
    beta<-A1*by[2]
    ##Predict x and y from w1
    x.hat <- t(t(w1 %*% bX[2,])+bX[1,])
    y.hat <- w1  * by[2,]+by[1,]
    
    B1 <- Load%*%solve(t(Load)%*%Sxx%*%Load)%*%t(Load)%*%Sxy

    Beta <- cbind(Beta,B1)
    ##Calculate 'residuals'
    x1  <- x1 - x.hat
    y1  <- y1 - y.hat
  }
  
  intercept <- m.y - m.x %*% Beta 
  Beta<-rbind(intercept,Beta)
  rownames(Beta)[1] <- ".int"
  Score <- as.data.frame(Score)
  names(Score) <- paste("S",1:K,sep='')
  Load <- as.data.frame(Load)
  names(Load) <- paste("L",1:K,sep='')
  value <- list(Scores=Score, Loadings=Load, Beta=Beta)
  class(value) <- "plsHelland"
  return(value)
}

plotB.plsHelland <- function(x,d=ncol(x$Beta)){
  ymin <- min(x$Beta[,1:d])
  ymax <- max(x$Beta[,1:d])
  for (j in 1:d){
    plot(x$Beta[-1,j],ylim=c(ymin,ymax),type="l",ylab="Beta")
    title(paste("Factors:",j))
  }
}
plotB <- function(x,d=ncol(x$Beta)) UseMethod("plotB")

sh.pls      <- function(X,y,K,data=F,predict=F,y.var=F){
#############################################################################
#   X:      data frame of predictors, (or, if data is a dataframe, 
#           a vector of names of predictors)
#   y:      data frame of response, (or, if data is a dataframe, 
#           the name of the response)
#   K:      number of factors to be extracted
#   data:   An optional data frame containing predictors and response
#           (in this case, X and y must be names of the predictors/responses)
#   predict:An optional data frame consisting of columns containing X. 
#   y.val   The name of variable to be predicted in the predict data frame.
#           (If present, the SPE is returned together with the predicted values)
#Example:
#X   <- sel.col(expl.list,   small)
#y   <- sel.col("y",         small)
#sh.pls(X,y,K=2,predict=s2)

if (is.data.frame(data)){
    X   <- sel.col(X, data)
    y   <- sel.col(y, data)
}

MX          <- matrix(rep(mean(X),nrow(X)),ncol=ncol(X),byrow=T)
My          <- rep(mean(y),length(y))
beta.pls    <- pls1c(X-MX,y-My,K=K)

if (is.data.frame(predict)){
    X2          <- sel.col(names(X), predict)
    MX2         <- matrix(rep(mean(X),nrow(X2)),ncol=ncol(X),byrow=T)
    My2         <- rep(mean(y),nrow(X2))
    y.pls       <- My2+as.matrix((X2-MX2))%*%beta.pls
    result      <- y.pls
    if (y.var != F){
        y.var   <- sel.col(y.var,  predict)
        pe      <- y.var-y.pls
        spe     <- pe^2
        result  <- as.data.frame( cbind(y.pls, y.var, pe, spe) )
        names(result)   <-   c("y.pls", "y.var", "pe", "spe")

    }
    return(result)
}
else 
    return(beta.pls)
}






######## BELOW HERE IT IS MIKE DENHAMS PLS CODE ########
######## DO NOT CHANGE                          ########

"pls1a"<-
function(X, y, K=min(dx[1]-1,dx[2]))
{
# Copyright (c) October 1993, Mike Denham.
# Comments and Complaints to: snsdenhm@reading.ac.uk
#
# Orthogonal Scores Algorithm for PLS (Martens and Naes, pp. 121--123)
#
# X: A matrix which is assumed to have been centred so that columns
#    sum to zero.
#
# y: A vector assumed to sum to zero.
#
# K: The number of PLS factors in the model which must be less than or
#    equal to the  rank of X.
#
# Returned Value is the vector of PLS regression coefficients
#
        X <- as.matrix(X)
        dx <- dim(X)
        W <- matrix(0, dx[2], K)
        P <- matrix(0, dx[2], K)
        Q <- numeric(K)
        for(i in 1:K) {
                w <- crossprod(X, y)
                w <- w/sqrt(crossprod(w)[1])
                W[, i] <- w
                tee <- X %*% w
                cee <- crossprod(tee)[1]
                p <- crossprod(X, (tee/cee))
                P[, i] <- p
                q <- crossprod(y, tee)[1]/cee
                Q[i] <- q
                X <- X - tee %*% t(p)
                y <- y - q * tee
        }
        W %*% solve(crossprod(P, W), Q)
}
"pls1b"<-
function(X, y, K=min(dx[1]-1,dx[2]))
{
# Copyright Mike Denham, October 1993.
# Comments and Complaints to: snsdenhm@reading.ac.uk
#
# Orthogonal Loadings Algorithm for PLS (Martens and Naes, pp. 123--125)
#
# X: A matrix which is assumed to have been centred so that columns
#    sum to zero.
#
# y: A vector assumed to sum to zero.
#
# K: The number of PLS factors in the model which must be less than or
#    equal to the  rank of X.
#
# Returned Value is the vector of PLS regression coefficients
#
# tol is set as the tolerance for the QR decomposition in determining
# rank deficiency
#
        tol <- 1e-10
        X <- as.matrix(X)
        dx <- dim(X)
        W <- matrix(0, dx[2], K)
        Tee <- matrix(0, dx[1], K)
        y0 <- y
        for(i in 1:K) {
                w <- crossprod(X, y)
                w <- w/sqrt(crossprod(w)[1])
                W[, i] <- w
                tee <- X %*% w
                Tee[, i] <- tee
                Q <- qr.coef(qr(Tee[, 1:i], tol = tol), y0)
                X <- X - tee %*% t(w)
                y <- y0 - Tee[, 1:i, drop = F] %*% Q
        }
        W %*% Q
}

"pls1c"<-
function(X, y, K=min(dx[1]-1,dx[2]))
{
# Copyright Mike Denham, October 1994.
# Comments and Complaints to: snsdenhm@reading.ac.uk
#
# Modified Helland Algorithm (Helland 1988 + Denham 1994)
#
# X: A matrix which is assumed to have been centred so that columns
#    sum to zero.
#
# y: A vector assumed to sum to zero.
#
# K: The number of PLS factors in the model which must be less than or
#    equal to the  rank of X.
#
# Returned Value is the vector of PLS regression coefficients
#
# tol is set as the tolerance for the QR decomposition in determining
# rank deficiency
#
        tol <- 1e-10
        X <- as.matrix(X)
        dx <- dim(X)
        W <- matrix(0, dx[2], K)
        XW <- matrix(0, dx[1], K)
        s <- crossprod(X, y)
        W[, 1] <- s
        XW[, 1] <- X %*% s
        QR <- qr(XW[, 1], tol = tol)
        r <- qr.resid(QR, y)
        if(K > 1) {
                for(i in 2:K) {
                        w <- crossprod(X, r)
                        W[, i] <- w
                        XW[, i] <- X %*% w
                        QR <- qr(XW[, 1:i], tol = tol)
                        r <- qr.resid(QR, y)
                }
        }
        W %*% qr.coef(QR, y)
}
"svdpls1a"<-
function(X, y, K = r)
{
# Copyright Mike Denham, October 1993.
# Comments and Complaints to: snsdenhm@reading.ac.uk
#
# Orthogonal Scores Algorithm for PLS (Martens and Naes, pp. 121--123)
# using Singular Value Decomposition. (Uses a replacement version of svd
# which is more efficient when the number of columns is large relative to
# the number of rows.)
#
# X: A matrix which is assumed to have been centred so that columns
#    sum to zero.
#
# y: A vector assumed to sum to zero.
#
# K: The number of PLS factors in the model which must be less than or
#    equal to the  rank of X.
#
# Returned Value is the vector of PLS regression coefficients
#

    X <- as.matrix(X)
    r <- min(dim(X) - c(1, 0))
    X <- my.svd(X)
    X$v[, 1:r] %*% pls1a(diag(X$d[1:r]), crossprod(X$u[, 1:r], y), K)
}
"svdpls1b"<-
function(X, y, K = r)
{
# Copyright Mike Denham, October 1993.
# Comments and Complaints to: snsdenhm@reading.ac.uk
#
# Orthogonal Loadings Algorithm for PLS (Martens and Naes, pp. 123--125)
#
# X: A matrix which is assumed to have been centred so that columns
#    sum to zero.
#
# y: A vector assumed to sum to zero.
#
# K: The number of PLS factors in the model which must be less than or
#    equal to the  rank of X.
#
# Returned Value is the vector of PLS regression coefficients
#
# tol is set as the tolerance for the QR decomposition in determining
# rank deficiency
#

    X <- as.matrix(X)
    r <- min(dim(X) - c(1, 0))
    X <- svd(X)
    X$v[, 1:r] %*% pls1b(diag(X$d[1:r]), crossprod(X$u[, 1:r], y), K)
}
"svdpls1c"<-
function(X, y, K = r)
{
# Copyright Mike Denham, October 1994.
# Comments and Complaints to: snsdenhm@reading.ac.uk
#
# Modified Helland Algorithm (Helland 1988 + Denham 1994)
#
# X: A matrix which is assumed to have been centred so that columns
#    sum to zero.
#
# y: A vector assumed to sum to zero.
#
# K: The number of PLS factors in the model which must be less than or
#    equal to the  rank of X.
#
# Returned Value is the vector of PLS regression coefficients
#
# tol is set as the tolerance for the QR decomposition in determining
# rank deficiency
#

    X <- as.matrix(X)
    r <- min(dim(X) - c(1, 0))
    X <- svd(X)
    X$v[, 1:r] %*% pls1c(diag(X$d[1:r]), crossprod(X$u[, 1:r], y), K)
}
"my.svd"<-
function(x, nu = min(n, p), nv = min(n, p))
{
# Alternative to Singular Value Decomposition function svd
# Examines matrix n by p matrix x and if n < p obtains the svd 
# by applying svd the transpose of x.
    x <- as.matrix(x)
    dmx <- dim(x)
    n <- dmx[1]
    p <- dmx[2]
    transpose.x <- n < p
    if(transpose.x) {
        x <- t(x)
        hold <- nu
        nu <- nv
        nv <- hold
    }
    cmplx <- mode(x) == "complex"
    if(!(is.numeric(x) || cmplx))
        stop("x must be numeric or complex")
    if(!cmplx)
        storage.mode(x) <- "double"
    dmx <- dim(x)
    n <- dmx[1]
    p <- dmx[2]
    mm <- min(n + 1, p)
    mn <- min(dmx)
    job <- (if(nv) 1 else 0) + 10 * (if(nu == 0) 0 else if(nu == mn)
        2
    else if(nu == n)
        1
    else stop("Invalid value for nu (must be 0, number of rows, or number of cols)"
            ))
    z <- .Fortran(if(!cmplx) "dsvdcs" else "zsvdcs",
        x,
        as.integer(n),
        as.integer(n),
        as.integer(p),
        d = if(!cmplx) double(mm) else complex(mm),
        if(!cmplx) double(p) else complex(p),
        u = if(!cmplx) if(nu)
                matrix(0, n, nu)
            else 0 else if(nu)
            matrix(as.complex(0), n, nu)
        else as.complex(0),
        as.integer(n),
        v = if(!cmplx) if(nv)
                matrix(0, p, p)
            else 0 else if(nv)
            matrix(as.complex(0), p, p)
        else as.complex(0),
        as.integer(p),
        if(!cmplx) double(n) else complex(n),
        as.integer(job),
        info = integer(1))[c("d", "u", "v", "info")]
    if(z$info)
        stop(paste("Numerical error (code", z$info, ") in algorithm"))
    if(cmplx) {
        if(all(Im(z$d) == 0))
            z$d <- Re(z$d)
        else stop("a singular value has a nonzero imaginary part")
    }
    length(z$d) <- mn
    if(nv && nv < p)
        z$v <- z$v[, seq(nv)]
    if(transpose.x) {
        z <- z[c("d", if(nu) "u" else NULL, if(nv) "v" else NULL)]
        names(z) <- names(z)[c(1, 3, 2)]
        z
    }
    else {
        z[c("d", if(nv) "v" else NULL, if(nu) "u" else NULL)]
    }
}
