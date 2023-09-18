## linest<- function(object, effect=NULL, at=NULL, only.at=TRUE, grid=TRUE, df.adjust=FALSE, ...){
##   UseMethod("linest")
## }

## linest.lmerMod <- function(object, effect=NULL, at=NULL, only.at=TRUE, grid=TRUE, df.adjust=FALSE, ...){

##   bhat <- fixef( object )
##   VV   <- vcov( object )

##   if (df.adjust && require("pbkrtest")) {
##     VVadj<- vcovAdj( object, 0 )
##     ddfm <- function(kk, se) .get_ddf(VVadj, VV, kk, se * se)
##     VVuse <- VVadj
##   } else {
##     ddfm <- function(kk, se) 1
##     VVuse <- VV
##   }
##   KK  <- popMatrix(object, effect=effect, at=at, only.at=only.at)
##   used      <- which(!is.na(bhat))
##   not.used  <- which(is.na(bhat))
##   bhat.used <- bhat[used]

##   if (length(not.used) > 0) {
##     ## cat("some are 'not.used'\n")
##     null.basis <- .get_null_basis ( object ) ## Do all objects have qr slot ??
##     estimable  <- .is.estimable(KK, null.basis)
##   } else {
##     null.basis <- NULL
##     estimable  <- rep(TRUE, length(bhat.used))
##   }

##   res <- .do.estimation_lmerMod( KK, bhat.used, VVuse, ddfm, used, estimable, null.basis )
##   cbind(res, attributes(KK)$grid)
## }

## linest.lm <- function(object, effect=NULL, at=NULL, only.at=TRUE, grid=TRUE, ...){

##   bhat <- coef(object)
##   VV   <- vcov(object)
##   df   <- object$df.residual ## Need function for this...

##   KK  <- popMatrix(object, effect=effect, at=at, only.at=only.at)
##   used     <- which(!is.na(bhat))
##   not.used <- which(is.na(bhat))
##   bhat.used     <- bhat[used]

##   if (length(not.used) > 0) {
##     ##cat("some are 'not.used'\n")
##     null.basis <- .get_null_basis ( object ) ## Do all objects have qr slot ??
##     estimable  <- .is.estimable(KK, null.basis)
##   } else {
##     null.basis <- NULL
##     estimable  <- rep(TRUE, length(bhat.used))
##   }

##   res <- .do.estimation_default( KK, bhat.used, VV, df, used, estimable, null.basis )
##   cbind(res, attributes(KK)$grid)
## }



## .do.estimation_lmerMod <- function( KK, bhat, VV, ddfm, used, estimable, null.basis ){
##   res <- matrix(NA, nrow=nrow(KK), ncol=3)
##   for (ii in 1:nrow(res)){
##     kk <- KK[ii,]
##     if (estimable[ii]){
##       kk   <- kk[used]
##       est  <- sum(kk * bhat)
##       se   <- sqrt(sum(kk * (VV %*% kk)))
## ##      print(kk); print(se)
##       df2  <- ddfm(kk, se)
##       res[ii,] <- c(est, se, df2)
##     }
##   }
##   colnames(res) <- c("estimate","SE","df")
##   res
## }


## .do.estimation_default <- function( KK, bhat, VV, df, used, estimable, null.basis ){
##   res <- matrix(NA, nrow=nrow(KK), ncol=3)
##   for (ii in 1:nrow(res)){
##     kk <- KK[ii,]
##     if (estimable[ii]){
##       kk  <- kk[used]
##       est <- sum(kk * bhat)
##       se  <- sqrt(sum(kk * (VV %*% kk)))
##       df2 <- df
##       res[ii,] <- c(est, se, df2)
##     }
##   }
##   colnames(res) <- c("estimate","SE","df")
##   res
## }


