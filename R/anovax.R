##' @title anova like function
##' @name anovax
##' @param object A model object object
##' @param ... further arguments
##' @param control A list controling simulations, only relevant for
##'     parametric bootstrapping.
##' @param test A character string
##' @author Søren Højsgaard
##' 
##' @examples 
##' lmm1 <- lmer(sugpct ~ block + sow + harvest + (1|block:harvest), data=beets)
##' lmm0 <- update(lmm1, .~. - sow)
##' anovax(lmm1, .~. - harvest, test="X2")
##' anovax(lmm1, .~. - harvest, test="KR")
##' anovax(lmm1, .~. - harvest, test="SAT")
##' anovax(lmm1, .~. - harvest, test="PB", control=list(nsim=50, cl=1))
##'
##' anovax(lmm1, test="X2")
##' anovax(lmm1, test="KR")
##' anovax(lmm1, test="SAT")
##' anovax(lmm1, test="PB", control=list(nsim=50, cl=1))
##' 
##' @export
##' @rdname anovax
anovax <- function(object, ..., test="x2", control=list(nsim=1000, cl=NULL)){
    UseMethod("anovax")
}

#' @rdname anovax
#' @export
anovax.lmerMod <- function(object, ..., test="x2", control=list(nsim=1000, cl=NULL)){
    test <- match.arg(tolower(test), c("kr", "sat", "pb", "x2"))    
    anovax_worker(object, ..., test=test, control=control)
}

#' @rdname anovax
#' @export
anovax.default <- function(object, ..., test="x2", control=list(nsim=1000, cl=NULL)){
    test <- match.arg(tolower(test), c("pb", "x2"))    
    anovax_worker(object, ..., test=test, control=control)
}

anovax_worker <- function(object, ..., test="x2", control=list(nsim=1000, cl=NULL)){

    ## print(test)
    dots <- list(...)
    if (is.null(control$nsim)) control$nsim <- 1000
    ## print(control)
    ## cat("anovax_worker dots:\n"); print(dots)

    test <- match.arg(tolower(test), c("kr", "sat", "pb", "x2"))    
    if (length(dots) == 0){
        an  <- anova(object)
        nms <- rownames(an)
        nms <- setdiff(nms, "Residuals")
        lg  <- object
        lgf <- formula(lg)

        nms <- rev(nms)
        ttt <- vector("list", length(nms))
        for (i in seq_along(nms)){
            term <- nms[i]
            smf <- doBy::formula_add_str(lgf, terms=term, op="-")
            sm <- update(lg, smf, control=lmerControl(check.conv.singular = "ignore"))
            kk <- comodex(lg, sm, test=test, control=control)
            out <- as.data.frame(kk)
            ttt[[i]] <- out
            lg  <- sm
            lgf <- formula(lg)
        }
        ttt <- rev(ttt)
        ## print(ttt)
        ttt <- do.call(rbind, ttt)
        rownames(ttt) <- rev(nms)
    } else {
        if (length(dots)==1){
            mod <- dots[[1]]
            ## if (!inherits(mod, "lmerMod"))
                ## stop("Second argument is not lmerMod\n")
            ttt <- comodex(object, mod, test=test, control=control) 
        }            
    }
    class(ttt) <- c("anovax", "data.frame")
    return(ttt)
}

##' @title print anovax object
##' @param x anovax object
##' 
#' @rdname anovax
#' @export
print.anovax <- function(x, ...){
    ## printCoefmat(x, digits=5, zap.ind =c(3,4))
    printCoefmat(x, digits=5)
    ## old <- options("digits")$digits
    ## options("digits"=5)
    ## print.data.frame(x)
    ## options("digits"=old)
    return(invisible(x))
}

## lmerControl(check.conv.singular = "ignore")

##' @title Various different tests for model comparison
##' @param object Model object 
##' @param object2 Model object or equivalent way of specifying a submodel of lmm1
##' @param test A vector with the various test types.
##' @param control A list controlling the model comparions.
##' @return Dataframe with results of the various tests
##' @author Søren Højsgaard
##' @export
anovax_list <- function(object, object2, test=c("x2", "kr", "sat", "pb"), control=list(nsim=1000)){
    if (is.null(control$nsim)) control$nsim <- 1000
    lapply(test,
         function(.test){
            anovax(object, object2, test=.test, control=control)
         }) |> do.call(rbind, args=_)
}

#' @noRd
comodex <- function(fit1, fit0, test="x2", control=list(), details=0, ...){
    UseMethod("comodex")
}


#' @noRd
comodex.lmerMod <- function(fit1, fit0, test="x2", control=list(), details=0, ...){

    test <- match.arg(tolower(test), c("kr", "sat", "pb", "x2"))
    modcomp_fun <- switch(test,                          
                          "x2" =x2_modcomp,
                          "kr" =kr_modcomp,
                          "sat"=sat_modcomp,
                          "pb" =pb_modcomp)
    out <- suppressWarnings(modcomp_fun(fit1, fit0,
                                        control=control, ...))
    return(out)
}


#' @noRd
comodex.default <- function(fit1, fit0, test="x2", control=list(), details=0, ...){
    
    test <- match.arg(tolower(test), c("pb", "x2"))
    modcomp_fun <- switch(test,
                          "x2" = x2_modcomp,                          
                          "pb" = pb_modcomp)
    out <- suppressWarnings(modcomp_fun(fit1, fit0,
                                        control=control, ...))
    return(out)
}

## ' @title Compare two models
## ' @name modcomp
## ' 
## ' @param fit1,fit0 Two models
## ' @param control A list

## ' @export
## ' @rdname any_modcomp

pb_modcomp <- function(fit1, fit0, control=list()){
    out <- PBmodcomp(fit1, fit0, nsim=control$nsim, cl=control$cl)
    ## return(out)
    out2 <- handle_old_output(out)
    out2 <- out2[2,, drop=FALSE] ## QUICK and dirty?    
    return(out2)
}

# #' @export
# #' @rdname any_modcomp
kr_modcomp <- function(fit1, fit0, control=list()){
    out <- KRmodcomp(fit1, fit0, betaH=control$betaH, details=control$details)
    out2 <- handle_old_output(out)
    out2$F.scaling <- NULL
    out2 <- out2[1,, drop=FALSE] ## QUICK and dirty?
    return(out2)
}

# #' @export
# #' @rdname any_modcomp
sat_modcomp <- function(fit1, fit0, control=list()){
    ## cat("sat_modcomp\n")
    out <- SATmodcomp(fit1, fit0, betaH=control$betaH, details=control$details)
    out2 <- handle_old_output(out)
    return(out2)
}

# #' @export
# #' @rdname any_modcomp
x2_modcomp <- function(fit1, fit0, control=list()){
    out <- X2modcomp(fit1, fit0, betaH=control$betaH, details=control$details)
    return(out)
}

handle_old_output <- function(out){
    out2 <- out$test
}

