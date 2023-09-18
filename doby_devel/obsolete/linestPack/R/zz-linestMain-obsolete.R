## ####################################################################################################
##
## Banff, august 2013
## This is an experimental function for creating 'linear estimates'
##
## ####################################################################################################

linest <- function(object, effect=NULL, at=NULL, K=NULL, level=0.95, ...){
    UseMethod("linest")
}

linest.glm <- function(object, effect=NULL, at=NULL, K=NULL, level=0.95,
                       type=c("link","response"), ...){
    type <- match.arg(type)

    cl <- match.call()
    cl[[1]]<-as.name("linest.default")
    le <- eval( cl )
    if (type=="response"){

        fit    <- le$coef[,1]
        se.fit <- le$coef[,2]
        df     <- le$coef[,3]

        se.fit <- se.fit * abs(family(object)$mu.eta(fit))
        fit <- family(object)$linkinv(fit)

        le$coef[,1] <- fit
        le$coef[,2] <- se.fit

        ## tstat  <- fit / se.fit
        ## pvalue <- pt(tstat, df=df, lower.tail=FALSE)
        ## le$coef[,4] <- tstat
        ## le$coef[,5] <- pvalue

        le$coef[,6] <- family(object)$linkinv(le$coef[,6])
        le$coef[,7] <- family(object)$linkinv(le$coef[,7])
    }
    le
}

linest.default <- function(object, effect=NULL, at=NULL, K=NULL, level=0.95,
                           ...){
    if (is.null(K)){
        K <- linestMatrix(object, effect=effect, at=at)
    } else {
        if (!inherits(K, "matrix")) stop("'K' must be a matrix\n")
    }
    .coef <- .do.linest( object, K )
    res   <- list(coef=.coef, grid=attr(K,"grid"), K=K)
    class(res) <- "linest"
    res
}






.do.linest <- function(object,K,level=0.95) UseMethod(".do.linest")

.do.linest.lm <- function(object,K,level=0.95){
    bhat <- coef(object)
    VV0  <- vcov(object)
    ddf  <- object$df.residual ## Need function for this...
    ddf.vec <- rep(ddf, nrow( K ))
    is.est <- .is_estimable(K, .get_null_basis( object ))

    .doest(K, bhat, VV0, ddf.vec, is.est, level)
}

.do.linest.lmerMod <- function(object,K,level=0.95){ #need adjust.df argument
    bhat <- fixef(object)
    VV0  <- vcov(object)
    VVa  <- vcovAdj(object)
    is.est <- .is_estimable(K, .get_null_basis( object ))

##    ddf.vec <- unlist(lapply(1:nrow(K), function(ii) .get_ddf(VVa, VVa, K[ii,], 0)))
    ddf.vec <- unlist(lapply(1:nrow(K), function(ii) ddf_Lb(VVa, K[ii,])))
    .doest(K, bhat, VVa, ddf.vec, is.est, level)
}

.doest <- function(K, bhat, VV, ddf.vec, is.est, level=0.95){
    used       <- which(!is.na(bhat))
    bhat.used  <- bhat[used]
    K   <- K[, used, drop=FALSE]
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
    qq<-qt(1-(1-level)/2, df=res[,"df"])
    lwr <- res[,"estimate"] - qq * res[,"SE"]
    upr <- res[,"estimate"] + qq * res[,"SE"]
    cbind(res, tstat, pvalue, lwr, upr)
}

print.linest <- function(x, ...){
    print(cbind(x$coef, x$grid))
    invisible(x)
}




