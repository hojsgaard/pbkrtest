
##########################################################
###
### Likelihood ratio statistic
###
##########################################################

#' @export
getLRT <- function(largeModel, smallModel){
  UseMethod("getLRT")
}

#' @export
getLRT.lmerMod <- function(largeModel, smallModel){

    logL_small <- logLik(suppressWarnings(update(smallModel, REML=FALSE)))
    logL_large <- logLik(update(largeModel, REML=FALSE))

    tobs     <- 2 * (logL_large - logL_small)
    df11     <- attr(logL_large, "df") - attr(logL_small, "df")
    p.X2     <- 1 - pchisq(tobs, df11)
    c(tobs=tobs, df=df11, p.value=p.X2)
}

#' @export
getLRT.gls <- function(largeModel, smallModel){

    logL_small <- logLik(suppressWarnings(update(smallModel, method="ML")))
    logL_large <- logLik(update(largeModel, method="ML"))

    tobs     <- 2 * (logL_large - logL_small)
    df11     <- attr(logL_large, "df") - attr(logL_small, "df")
    p.X2     <- 1 - pchisq(tobs, df11)
    c(tobs=tobs, df=df11, p.value=p.X2)
}

#' @export
getLRT.glmerMod <-
        function(largeModel, smallModel){
    logL_small <- logLik(update(smallModel))
    logL_large <- logLik(update(largeModel))
    tobs     <- 2 * (logL_large - logL_small)
    df11     <- attr(logL_large, "df") - attr(logL_small, "df")
    p.X2     <- 1 - pchisq(tobs, df11)
    c(tobs=tobs, df=df11, p.value=p.X2)
}

#' @export
getLRT.lm <- function(largeModel, smallModel){
  logL_small <- logLik(smallModel)
  logL_large <- logLik(largeModel)
  tobs     <- 2 * (logL_large - logL_small)
  df11     <- attr(logL_large, "df") - attr(logL_small, "df")
  p.X2     <- 1 - pchisq(tobs, df11)
  c(tobs=tobs, df=df11, p.value=p.X2)
}


## getLRT.merMod <-
    ## getLRT.mer <-
        ## function(largeModel, smallModel){
    ## logL_small <- logLik(update(smallModel, REML=FALSE))
    ## logL_large <- logLik(update(largeModel, REML=FALSE))
    ## tobs     <- 2 * (logL_large - logL_small)
    ## df11     <- attr(logL_large, "df") - attr(logL_small, "df")
    ## p.X2     <- 1 - pchisq(tobs, df11)
    ## c(tobs=tobs, df=df11, p.value=p.X2)
## }






