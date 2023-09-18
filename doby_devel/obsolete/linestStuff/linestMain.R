## ####################################################################################################
##
## Banff, august 2013
## This is an experimental function for creating 'linear estimates'
##
## ####################################################################################################


linest <- function(object, effect=NULL, at=NULL, K=NULL, ...){
    UseMethod("linest")
}

linest.default <- function(object, effect=NULL, at=NULL, K=NULL, ...){
    if (is.null(K)){
        K <- linestMatrix(object, effect=effect, at=at)
    } else {
        if (!inherits(K, "matrix"))
            stop("'K' must be a matrix\n")
    }
    .coef <- .do.linest( object, K )
    res <- list(coef=.coef, grid=attr(K,"grid"), K=K)
    class(res) <- "linest"
    res
}

pdiff <- function(object, effect=NULL, at=NULL, ...){
    K   <- linestMatrix(object, effect=effect, at=at, ...)
    pm   <- .do_pairs( K )
    DK  <- pm$DD %*% K
    .coef <- .do.linest( object, DK)
    res  <- list(coef=.coef, grid=pm$grid, K=DK)
    class(res)<-"linest"
    res
}

print.linest <- function(x, ...){
    print(cbind(x$coef, x$grid))
    invisible(x)
}

.do.linest <- function(object,K) UseMethod(".do.linest")

.do.linest.lm <- function(object,K){
    bhat <- coef(object)
    VV0  <- vcov(object)
    ddf  <- object$df.residual ## Need function for this...
    ddf.vec <- rep(ddf, nrow( K ))
    is.est <- .is_estimable(K, .get_null_basis( object ))
    .doest(K, bhat, VV0, ddf.vec, is.est)
}

.do.linest.lmerMod <- function(object,K){ #need adjust.df argument
    bhat <- fixef(object)
    VV0  <- vcov(object)
    VVa  <- vcovAdj(object)
    ddf.vec <- unlist(lapply(1:nrow(K), function(ii) .get_ddf(VVa, VVa, K[ii,], 0))) ##DIRTY
    is.est <- .is_estimable(K, .get_null_basis( object ))
    .doest(K, bhat, VVa, ddf.vec, is.est)
}

.doest <- function(K, bhat, VV, ddf.vec, is.est){
    used       <- which(!is.na(bhat))
    bhat.used  <- bhat[used]
    K  <- K[, used, drop=FALSE]
    res <- matrix(NA, nrow=nrow(K), ncol=3)
    for (ii in 1:nrow(res)){
        kk <- K[ii,]
        if (is.est[ii]){
            est  <- sum(kk * bhat.used)
            se   <- sqrt(sum(kk * (VV %*% kk)))
            df2  <- ddf.vec[ii]
            res[ii,] <- c(est, se, df2)
        }
    }
    colnames(res) <- c("estimate","SE","df")
    tstat <- res[,"estimate"]/res[,"SE"]
    pvalue <- pt(tstat, df=res[,"df"], lower.tail=FALSE)
    cbind(res, tstat, pvalue)
}


















    #printCoefmat(x$LE, tst.ind=4, na.print='', has.Pvalue=TRUE)
    #print( cbind(x$LE, attr(x$KK, "grid")) )
    #printCoefmat(x$coef, tst.ind=4, na.print='', has.Pvalue=TRUE)
    #print(x$coef)
    #print(x$grid)
