linest <- function(object, L=NULL, level=0.95, confint=FALSE, ...){
    UseMethod("linest")
}

linest.lm <- function(object, L=NULL, level=0.95, confint=FALSE, ...){
    bhat <- coef(object)
    L  <- .constructL(L, bhat)    
    is.est <- is_estimable(L, null_basis( object ))

    ##VV0  <- vcov(object)
    VV0  <- vcov(object, complete=FALSE)
    ddf  <- object$df.residual
    ddf.vec <- rep(ddf, nrow( L ))
    res    <- .getLb( L, bhat, VV0, ddf.vec, is.est)


    
    p.value <- 2*pt(abs(res[,"t.stat"]), df=res[,"df"], lower.tail=FALSE)

    print(1-(1-level)/2)
    
    qq  <- qt(1-(1-level)/2, df=res[,"df"])

    a <- (1 - level)/2
    a <- c(a, 1 - a)

    print(a)
    
    DF <- object$df
    fac <- qt(a, DF)

    print(fac)
    ses <- res[, "se"]

    ci <- res[,"estimate"] + ses %o% fac
    print(ci)
    
    
    lwr <- res[,"estimate"] - qq * res[,"se"]
    upr <- res[,"estimate"] + qq * res[,"se"]
    #res <- cbind(res, p.value, lwr, upr)
    res <- cbind(res, p.value)

    .finalize_linest(res, L, lwr, upr, confint)
}




linest.glm <- function(object, L=NULL, level=0.95, type=c("link", "response"), confint=FALSE, ...){
    type <- match.arg(type)

    bhat <- coef(object)
    L  <- .constructL(L, bhat)        
    is.est <- is_estimable(L, null_basis( object ))

    VV0  <- vcov(object, complete=FALSE)
    
    ddf.vec <- rep(object$df.residual, nrow( L ))
    res     <- .getLb( L, bhat, VV0, ddf.vec, is.est)

    if(family(object)[1] %in% c("poisson","binomial","quasipoisson","quasibinomial")){
        p.value <- 2*pnorm(abs(res[,"t.stat"]), lower.tail=FALSE)
        qq <- qnorm(1-(1-level)/2)
        colnames(res)[4] <- "z.stat"
        res <- res[,-3] # NO df's
    } else {
        p.value <- 2*pt(abs(res[,"t.stat"]), df=res[,"df"], lower.tail=FALSE)
        qq <- qt(1-(1-level)/2, df=res[,"df"])
    }

    lwr <- res[,"estimate"] - qq * res[,"se"]
    upr <- res[,"estimate"] + qq * res[,"se"]

    if (type=="response"){
        fit    <- family(object)$linkinv(res[,"estimate"])
        se.fit <- res[,"se"] * abs(family(object)$mu.eta(res[,"estimate"]))
        res[,"estimate"]  <- fit
        res[,"se"] <- se.fit
        lwr <- family(object)$linkinv(lwr)
        upr <- family(object)$linkinv(upr)
    }

    ##res <- cbind(res, p.value, lwr, upr)
    res <- cbind( res, p.value )
    .finalize_linest(res, L, lwr, upr, confint)

}


linest.geeglm <- function(object, L=NULL, level=0.95, type=c("link","response"), confint=FALSE, ...){
    type <- match.arg(type)
    bhat <- coef(object)
    L  <- .constructL(L, bhat)
    
    is.est <- is_estimable(L, null_basis( object ))

    VV0  <- summary(object)$cov.scaled
    ddf.vec <- rep(1, nrow(L))
    res     <- .getLb( L, bhat, VV0, ddf.vec, is.est)

    p.value <- 2*pnorm(abs(res[,"t.stat"]), lower.tail=FALSE)

    colnames(res)[4] <- "z.stat"
    res <- res[,-3, drop=FALSE] # NO df's

    qq <- qnorm(1 - (1 - level) / 2)
    lwr <- res[,"estimate"] - qq * res[,"se"]
    upr <- res[,"estimate"] + qq * res[,"se"]


    if (type=="response"){
        fit    <- family(object)$linkinv(res[,"estimate"])
        se.fit <- res[,"se"] * abs(family(object)$mu.eta(res[,"estimate"]))
        res[,"estimate"]  <- fit
        res[,"se"] <- se.fit
        lwr <- family(object)$linkinv(lwr)
        upr <- family(object)$linkinv(upr)
    }

    res <- cbind(res, p.value)
    .finalize_linest(res, L, lwr, upr, confint)
}

linest.lmerMod <- function(object, L=NULL, level=0.95, adjust.df=TRUE, confint=FALSE, ...){

    bhat <- lme4::fixef(object)
    L  <- .constructL(L, bhat)    
    is.est <- is_estimable(L, null_basis( object ))

    if (adjust.df){
        if (requireNamespace("pbkrtest", quietly=TRUE)){
            ##VVu  <- vcov(object)
            VVu  <- vcov(object, complete=FALSE)
            VV   <- pbkrtest::vcovAdj(object)
            ddf.vec <- unlist(lapply(1:nrow(L),
                                     function(ii) pbkrtest::ddf_Lb(VV , L[ii,], VVu)))
        } else {
            stop("adjustment of degrees of freedom requires that 'pbkrtest' is installed")
        }
    } else {
        a   <- logLik(object)
        ddf <- attributes(a)$nall - attributes(a)$df
        ddf.vec <- rep(ddf, length(bhat))
        ##VV  <- vcov(object)
        VV  <- vcov(object, complete=FALSE)
    }


    res     <- .getLb( L, bhat, VV, ddf.vec, is.est)
    p.value <- 2*pt(abs(res[,"t.stat"]), df=res[,"df"], lower.tail=FALSE)

    qq  <- qt(1 - (1 - level)/2, df=res[,"df"])
    lwr <- res[,"estimate"] - qq * res[,"se"]
    upr <- res[,"estimate"] + qq * res[,"se"]

    res <- cbind(res, p.value)
    .finalize_linest(res, L, lwr, upr, confint)
}

linest.merMod <- function(object, L=NULL, level=0.95, ...){
    cl <- match.call()
    cl[[1]] <- as.name("linest.lmerMod")
    cl$adjust.df <- FALSE
    eval(cl)
}




## tidy.linest_class <- function(x, conf.int = FALSE, conf.level = 0.95, ...){
##     co <- stats::coef(x)
##     rownames(co) <- NULL

##     if (ncol(co)==5){ ## There are degreeso of freedom in the output.
##         co <- co[,c(1,2,4,5,3)]
##         nn <- c("estimate", "std.error", "statistic", "p.value", "df")
##     } else
##         nn <- c("estimate", "std.error", "statistic", "p.value")
##     names(co) <- nn

##     if (conf.int){
##         ci <- confint(x, level=conf.level)
##         colnames(ci) <- c("conf.low", "conf.high")    
##         co <- cbind(co, ci)
##     }
##     as_tibble(co)
## }


.getLb <- function(L, bhat, VV, ddf.vec, is.est, level=0.95){
    #' cat(".getLb")
    #' print(attributes(L))
    off <- attr(L, "offset")
    #' print(off)

    used       <- which(!is.na(bhat))
    bhat.used  <- bhat[used]
    L   <- L[, used, drop=FALSE]
    res <- matrix(NA, nrow=nrow(L), ncol=3)
    for (ii in 1:nrow(res)){
        kk <- L[ii,]
        if (is.est[ii]){
            est  <- sum(kk * bhat.used)
            se   <- sqrt(sum(kk * (VV %*% kk)))
            df2  <- ddf.vec[ii]
            res[ii,] <- c(est, se, df2)
        }
    }

    if (!is.null(off))
        res[,1] <- res[,1] + off[[1]]

    colnames(res) <- c("estimate","se","df") ## FIXME : Change se to std.error
    t.stat        <- res[,"estimate"] / res[,"se"]
    cbind(res, t.stat)
}























