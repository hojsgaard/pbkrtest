## FIXME: comodex er et dumt navn

##' @title Model comparison
##' 
##' @description Wrapper for functions KRmodcomp, SATmodcomp, PBmodcomp, X2modcomp
##' @name comodex
##' @param largeModel A model object
##' @param smallModel A model object, a formula or a restriction matrix
##' @param test A character string
##' @param control A list controlling the model comparions.
##' @param ... Additional arguments to be passed on to other methods
##' @param details should details be printed
##' @author Søren Højsgaard
##'
##' @examples
#' (lmm0 <- lmer(Reaction ~ (Days|Subject), sleepstudy))
#' (lmm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
#' (lmm2 <- lmer(Reaction ~ Days + I(Days^2) + (Days|Subject), sleepstudy))
#'
#' lm1 <- lm(dist ~ speed + I(speed^2), data=cars)
#' lm0 <- lm(dist ~ speed, data=cars)
#' 
#' comodex(lmm2, lmm1, test="x2")
#' comodex(lmm2, lmm1, test="kr")
#' comodex(lmm2, lmm1, test="sat")
#' comodex(lmm2, lmm1, test="PB", control=list(nsim=50, cl=1))
#' comodex(lmm2, .~. - I(Days^2))
#'
#' comodex(lm1, lm0)
#' comodex(lm1, lm0, test="pb", control=list(nsim=50, cl=1))
#' 

#' @export
#' @rdname comodex
comodex <- function(largeModel, smallModel, test="x2", control=list(), details=0, ...){
    UseMethod("comodex")
}

#' @rdname comodex
#' @export
comodex.lmerMod <- function(largeModel, smallModel, test="x2", control=list(), details=0, ...){

    test <- match.arg(tolower(test), c("kr", "sat", "pb", "x2"))
    modcomp_fun <- switch(test,                          
                          "x2" =x2_modcomp,
                          "kr" =kr_modcomp,
                          "sat"=sat_modcomp,
                          "pb" =pb_modcomp)
    out <- suppressWarnings(modcomp_fun(largeModel, smallModel,
                                        control=control, ...))
    return(out)
}

#' @rdname comodex
#' @export
comodex.default <- function(largeModel, smallModel, test="x2", control=list(), details=0, ...){
    
    test <- match.arg(tolower(test), c("pb", "x2"))
    modcomp_fun <- switch(test,
                          "x2" = x2_modcomp,                          
                          "pb" = pb_modcomp)
    out <- suppressWarnings(modcomp_fun(largeModel, smallModel,
                                        control=control, ...))
    return(out)
}


#' @title Compare two models
#' @name modcomp
#' 
#' @param largeModel,smallModel Two models
#' @param control A list

#' @export
#' @rdname any_modcomp
pb_modcomp <- function(largeModel, smallModel, control=list()){
    out <- PBmodcomp(largeModel, smallModel, nsim=control$nsim, cl=control$cl)
    ## return(out)
    out2 <- handle_old_output(out)
    out2 <- out2[2,,drop=FALSE] ## QUICK and dirty?    
    return(out2)
}

#' @export
#' @rdname any_modcomp
kr_modcomp <- function(largeModel, smallModel, control=list()){
    out <- KRmodcomp(largeModel, smallModel, betaH=control$betaH, details=control$details)
    out2 <- handle_old_output(out)
    out2$F.scaling <- NULL
    out2 <- out2[1,,drop=FALSE] ## QUICK and dirty?
    return(out2)
}

#' @export
#' @rdname any_modcomp
sat_modcomp <- function(largeModel, smallModel, control=list()){
    out <- SATmodcomp(largeModel, smallModel, betaH=control$betaH, details=control$details)
    out2 <- handle_old_output(out)
    return(out2)
}

#' @export
#' @rdname any_modcomp
x2_modcomp <- function(largeModel, smallModel, control=list()){
    
    out <- X2modcomp(largeModel, smallModel, betaH=control$betaH, details=control$details)
    return(out)
}

handle_old_output <- function(out){
    out2 <- out$test
}

