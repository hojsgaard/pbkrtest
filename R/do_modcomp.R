##' @title Model comparison
##' 
##' @description Wrapper for functions KRmodcomp, SATmodcomp, PBmodcomp, PBFmodcomp
##' @name fmodcomp
##' @param largeModel An \code{lmer} model
##' @param smallModel An \code{lmer} model or a restriction matrix
##' @param test A character string
##' @param ... Additional arguments to be passed on to other methods
##' @param details should details be printed
##' @param cl number of clusters
##' @author Søren Højsgaard
##'
##' @examples
#' (fm0 <- lmer(Reaction ~ (Days|Subject), sleepstudy))
#' (fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
#' (fm2 <- lmer(Reaction ~ Days + I(Days^2) + (Days|Subject), sleepstudy))
#'
#' fmodcomp(fm2, fm1)
#' fmodcomp(fm2, fm1, "sat")
#' fmodcomp(fm2, .~. - I(Days^2))
#' fmodcomp(fm2, fm1, "PB", nsim=1000, cl=1)
#' fmodcomp(fm2, fm1, "PBF", nsim=1000, cl=1)


#' @export
#' @rdname fmodcomp
fmodcomp <- function(largeModel, smallModel, test="kr", details=0, ...){
    UseMethod("fmodcomp")
}

#' @rdname fmodcomp
#' @export
fmodcomp.lmerMod <- function(largeModel, smallModel, test="kr", details=0, ...){

    ## cat("fmodcomp.lmerMod, test=", test, "\n")
    test <- match.arg(tolower(test), c("kr", "sat", "pb", "pbf", "x2"))
    modcomp_fun <- switch(test,
                          
                          "x2"=X2modcomp,
                          "kr" =KRmodcomp,
                          "sat"=SATmodcomp,
                          "pb" =PBmodcomp,
                          "pbf"=PBFmodcomp)
    out <- suppressWarnings(modcomp_fun(largeModel, smallModel, ...))
    out
}

#' @rdname fmodcomp
#' @export
fmodcomp.gls <- function(largeModel, smallModel, test="pb", details=0, ...){

    ## cat("....\n test\n", test)
    test <- match.arg(tolower(test), c("pb", "pbf", "x2"))
    modcomp_fun <- switch(test,
                  "x2" =X2modcomp,                          
                  "pb" =PBmodcomp,
                  "pbf"=PBFmodcomp)
    out <- modcomp_fun(largeModel, smallModel, ...)
    out
}


#' @rdname fmodcomp
#' @export
fmodcomp.lm <- fmodcomp.gls







##' @title anova like function
##' @name fanova
##' @param object A model object object
##' @param ... further arguments
##' @param cl number of cluster (for test="pf" and "pbf")
##' @param test A character string
##' @author Søren Højsgaard
##' 
##' @examples 
##' fm1 <- lmer(sugpct ~ block + sow + harvest + (1|block:harvest), data=beets)
##' fm0 <- update(fm1, .~. - sow)
##' fanova(fm1, ~.-harvest, test="KR")
##' fanova(fm1, ~.-harvest, test="SAT")
##' fanova(fm1, ~.-harvest, test="PB", cl=1)
##'
##' fanova(fm1, test="KR")
##' fanova(fm1, test="SAT")
##' fanova(fm1, test="PB", cl=1)
##' fanova(fm1, test="PBF", cl=1)
##' @export
##' @rdname fanova
fanova <- function(object, ..., test="kr", cl=NULL){
    UseMethod("fanova")
}


#' @rdname fanova
#' @export
fanova.lmerMod <- function(object, ..., test="kr", cl=NULL){
    test <- match.arg(tolower(test), c("kr", "sat", "pb", "pbf", "x2"))    
    fanova_worker(object, ..., test=test, cl=cl)
}

#' @rdname fanova
#' @export
fanova.gls  <- function(object, ..., test="pb", cl=NULL){
    test <- match.arg(tolower(test), c("pb", "pbf", "x2"))    
    fanova_worker(object, ..., test=test, cl=cl)
}

#' @rdname fanova
#' @export
fanova.lm  <- function(object, ..., test="pb", cl=NULL){
    test <- match.arg(tolower(test), c("pb", "pbf", "x2"))    
    fanova_worker(object, ..., test=test, cl=cl)
}

## FIXME 
#' @rdname fanova
#' @export
print.fanova <- function(x, ...){
    printCoefmat(x, digits=5, zap.ind =c(3,4))

    ## old <- options("digits")$digits
    ## options("digits"=5)
    ## print.data.frame(x)
    ## options("digits"=old)
    return(invisible(x))
}

fanova_worker <- function(object, ..., test="kr", cl=NULL){

    dots <- list(...)
    ## cat("dots:\n"); print(dots)

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
            ## print(term)
            ## print(lgf)
            ## print(smf)
            sm <- update(lg, smf)

            kk <- fmodcomp(lg, sm, test=test, cl=cl)
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
            ttt <- fmodcomp(object, mod, test=test, cl=cl) 
        }            
    }
    class(ttt) <- c("fanova", "data.frame")
    return(ttt)
}



