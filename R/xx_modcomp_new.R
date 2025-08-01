## #' @title Compare two models
## #' @name modcomp
## #' 
## #' @param largeModel,smallModel Two models
## #' @param control A list

## #' @export
## #' @rdname any_modcomp
## pb_modcomp <- function(largeModel, smallModel, control=list()){
##     out <- PBmodcomp(largeModel, smallModel, nsim=control$nsim, cl=control$cl)
##     ## return(out)
##     out2 <- handle_old_output(out)
##     out2 <- out2[2,,drop=FALSE] ## QUICK and dirty?    
##     return(out2)
## }

## #' @export
## #' @rdname any_modcomp
## kr_modcomp <- function(largeModel, smallModel, control=list()){
##     out <- KRmodcomp(largeModel, smallModel, betaH=control$betaH, details=control$details)
##     out2 <- handle_old_output(out)
##     out2$F.scaling <- NULL
##     out2 <- out2[1,,drop=FALSE] ## QUICK and dirty?
##     return(out2)
## }

## #' @export
## #' @rdname any_modcomp
## sat_modcomp <- function(largeModel, smallModel, control=list()){
##     out <- SATmodcomp(largeModel, smallModel, betaH=control$betaH, details=control$details)
##     out2 <- handle_old_output(out)
##     return(out2)
## }

## #' @export
## #' @rdname any_modcomp
## x2_modcomp <- function(largeModel, smallModel, control=list()){
    
##     out <- X2modcomp(largeModel, smallModel, betaH=control$betaH, details=control$details)
##     return(out)
## }

## handle_old_output <- function(out){
##     out2 <- out$test
## }

