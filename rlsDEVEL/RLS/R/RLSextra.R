#' @title Recursive Least Squares - extra
#' @description FIXME
#' @name rls-extra
#' 

#' @rdname rls-extra
print.rls <- function(x, ...){
  cat("call: ")
  print(x$call)

  cat("lambda: ", x$eta, "\n")
                                        ##cat("slots:\n")
                                        ##print(names(x))
}


#' @rdname rls-extra
fitted.rls <- function(object, type="dynamic", ...){
  type <- match.arg(type, c("dynamic", "static"))
  if (type=="dynamic")
    object$fit[,"yt.hat"]
  else {
    b <- object$coef[nrow(object$coef),,drop=FALSE]
    as.numeric(object$X %*% t(b))
  }
}

#' @rdname rls-extra
residuals.rls <- function(object, type="dynamic", ...){
  type <- match.arg(type, c("dynamic", "static"))
  if (type=="dynamic")
    object$fit[,"et"]
  else {
    b <- object$coef[nrow(object$coef),,drop=FALSE]
    object$y - as.numeric(object$X %*% t(b))    
  }
}

#' @rdname rls-extra
forecastRLS <- function(object, nAhead=1, na.pad=TRUE, type="response"){
  type=match.arg(type, c("response","error"))
  b <- object$coef
  b <- object$coef[1:(nrow(b)-nAhead),,drop=FALSE]
  X <- object$X[(1+nAhead):nrow(object$X),,drop=FALSE]
  ans <- rowSums(X*b)

  if (type=="error")
    ans <- ans - object$y[(1+nAhead):nrow(object$X)]
  
  if (na.pad)
    ans <- c(rep(NA, nAhead), ans)
  
  return(ans)
}

## predict.rls
##
## newdata :
##   x variable for creating polynomials (via basis matrix X)
## at :
##   An index used to extract row with b in the coefficient matrix
## tvar :
##   time variable (of same length as x) relating x[i] to time t[i]
## value :
##   A vector

## If tvar is not NULL then for each tvar[i] find the largest t* in the
## object such that t*<=tvar[i]. Extract corresponding b from coefficient
## matrix and calculate the the predicted value. If tvar is NULL then use
## the at variable. If at is NULL then take b to be the last row in the
## coef matrix. Otherwise take b to be the entry in the coef matrix given
## by at. Thus when at is used, the same b is used for creating all
## predicted values.

#' @rdname rls-extra
predict.rls <- function(object, newdata, at=NULL, tvar=NULL, ...){

  if (missing(newdata)) {
    newdata <- object$x
  }

  X <- .makeBasis(newdata, degree=object$degree)
  
  if (!is.null(tvar)){
    objt <- object$tvar
    idx <- unlist(lapply(tvar, function(t2) max(which(objt<=t2))))
    b <- object$coef[idx,,drop=FALSE]
    ans <- rowSums(X*b)
  } else {
    if (is.null(at)){
      b <- object$coef[nrow(object$coef),,drop=FALSE]
    } else {
      b <- object$coef[at,,drop=FALSE]
    }
    ans <- as.numeric(X%*%t(b))
  }
  return(ans)
}



.makeBasis <- function(x, degree=1){

  if (degree>3)
    stop("Degree larger than 3 not allowed...\n")

  X <- switch(as.character(degree),
              "0"= {matrix(1, nrow=length(x), ncol=1) }, 
              "1"= {cbind(1,x)},
              "2"= {cbind(1,x,x^2)},
              "3"= {cbind(1,x,x^2,x^3)})
  return(X)
}

