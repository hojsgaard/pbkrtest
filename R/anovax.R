##' @title Model comparison
##' 
##' @description Wrapper for functions KRmodcomp, SATmodcomp, PBmodcomp, PBFmodcomp
##' @name comodex
##' @param largeModel An \code{lmer} model
##' @param smallModel An \code{lmer} model or a restriction matrix
##' @param test A character string
##' @param control A list controlling the model comparions.
##' @param ... Additional arguments to be passed on to other methods
##' @param details should details be printed
##' @author Søren Højsgaard
##'
##' @examples
#' (lmer0 <- lmer(Reaction ~ (Days|Subject), sleepstudy))
#' (lmer1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
#' (lmer2 <- lmer(Reaction ~ Days + I(Days^2) + (Days|Subject), sleepstudy))
#'
#' anovax(lmer2, lmer1)
#' anovax(lmer2, lmer1, test="sat")
#' anovax(lmer2, .~. - I(Days^2))
#' anovax(lmer2, lmer1, test="PB", control=list(nsim=50, cl=1))
#' anovax(lmer2, lmer1, test="PBF", control=list(nsim=50, cl=1))


#' @export
#' @rdname comodex
comodex <- function(largeModel, smallModel, test="x2", control=list(), details=0, ...){
    UseMethod("comodex")
}

#' @rdname comodex
#' @export
comodex.lmerMod <- function(largeModel, smallModel, test="x2", control=list(), details=0, ...){

    ## cat("comodex.lmerMod, test=", test, "\n")

    test <- match.arg(tolower(test), c("kr", "sat", "pb", "pbf", "x2"))
    modcomp_fun <- switch(test,
                          
                          "x2" =X2modcomp,
                          "kr" =KRmodcomp,
                          "sat"=SATmodcomp,
                          "pb" =PBmodcomp,
                          "pbf"=PBFmodcomp)
    out <- suppressWarnings(modcomp_fun(largeModel, smallModel,
                                        control=control, ...))
    out
}

#' @rdname comodex
#' @export
comodex.gls <- function(largeModel, smallModel, test="x2", control=list(), details=0, ...){

    ## cat("....\n test\n", test)
    test <- match.arg(tolower(test), c("pb", "pbf", "x2"))
    modcomp_fun <- switch(test,
                  "x2" = X2modcomp,                          
                  "pb" = PBmodcomp,
                  "pbf"= PBFmodcomp)
    out <- modcomp_fun(largeModel, smallModel, ...)
    out
}

#' @rdname comodex
#' @export
comodex.glmerMod <- function(largeModel, smallModel, test="x2", control=list(), details=0, ...){

    ## cat("....\n test\n", test)
    test <- match.arg(tolower(test), c("pb", "pbf", "x2"))
    modcomp_fun <- switch(test,
                  "x2" = X2modcomp,                          
                  "pb" = PBmodcomp,
                  "pbf"= PBFmodcomp)
    out <- modcomp_fun(largeModel, smallModel, ...)
    out
}


#' @rdname comodex
#' @export
comodex.lm <- comodex.gls







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
##' fm1 <- lmer(sugpct ~ block + sow + harvest + (1|block:harvest), data=beets)
##' fm0 <- update(fm1, .~. - sow)
##' anovax(fm1, .~. - harvest, test="KR")
##' anovax(fm1, .~. - harvest, test="SAT")
##' anovax(fm1, .~. - harvest, test="PB", control=list(nsim=50, cl=1))
##'
##' anovax(fm1, test="KR")
##' anovax(fm1, test="SAT")
##' anovax(fm1, test="PB", control=list(nsim=50, cl=1))
##' ## anovax(fm1, test="PBF", control=list(nsim=50, cl=1))
##' @export
##' @rdname anovax
anovax <- function(object, ..., test="x2", control=list(nsim=1000, cl=NULL)){
    UseMethod("anovax")
}

#' @rdname anovax
#' @export
anovax.lmerMod <- function(object, ..., test="x2", control=list(nsim=1000, cl=NULL)){
    test <- match.arg(tolower(test), c("kr", "sat", "pb", "pbf", "x2"))    
    anovax_worker(object, ..., test=test, control=control)
}

#' @rdname anovax
#' @export
anovax.glmerMod <- function(object, ..., test="x2", control=list(nsim=1000, cl=NULL)){
    test <- match.arg(tolower(test), c("pb", "pbf", "x2"))    
    anovax_worker(object, ..., test=test, control=control)
}

#' @rdname anovax
#' @export
anovax.gls  <- function(object, ..., test="x2", control=list(nsim=1000, cl=NULL)){
    test <- match.arg(tolower(test), c("pb", "pbf", "x2"))    
    anovax_worker(object, ..., test=test, control=control)
}

#' @rdname anovax
#' @export
anovax.lm  <- function(object, ..., test="x2", control=list(nsim=1000, cl=NULL)){
    test <- match.arg(tolower(test), c("pb", "pbf", "x2"))    
    anovax_worker(object, ..., test=test, control=control)
}

##' @title print anovax object
##' @param x anovax object
##' 
#' @rdname anovax
#' @export
print.anovax <- function(x, ...){
    printCoefmat(x, digits=5, zap.ind =c(3,4))

    ## old <- options("digits")$digits
    ## options("digits"=5)
    ## print.data.frame(x)
    ## options("digits"=old)
    return(invisible(x))
}

anovax_worker <- function(object, ..., test="x2", control=list(nsim=1000, cl=NULL)){

    dots <- list(...)
    if (is.null(control$nsim)) control$nsim <- 1000
    ## cat("anovax_worker dots:\n"); print(dots)
## dd <<- dots
    test <- match.arg(tolower(test), c("kr", "sat", "pb", "pbf", "x2"))    
    if (length(dots) == 0){
        an  <- anova(object)
        nms <- rownames(an)
        nms <- setdiff(nms, "Residuals")
        lg <- object
        lgf <- formula(lg)

        nms <- rev(nms)
        ttt <- vector("list", length(nms))
        for (i in seq_along(nms)){
            term <- nms[i]
            smf <- doBy::formula_add_str(lgf, terms=term, op="-")
            sm <- update(lg, smf)
            kk <- comodex(lg, sm, test=test, control=control)
            out <- as.data.frame(kk)
            ttt[[i]] <- out
            lg <- sm
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


##' @title Various different tests for model comparison
##' @param object Model object 
##' @param object2 Model object or equivalent way of specifying a submodel of lmm1
##' @param test A vector with the various test types.
##' @param control A list controlling the model comparions.
##' @return Dataframe with results of the various tests
##' @author Søren Højsgaard
##' @export
anovax_list <- function(object, object2, test=c("x2", "kr", "sat", "pb"), control=list(nsim=1000)){
    lapply(test,
         function(.test){
            anovax(object, object2, test=.test, control=control)
         }) |> do.call(rbind, args=_)
}
