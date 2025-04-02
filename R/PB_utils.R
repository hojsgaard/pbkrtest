
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

    ## logL_small <- logLik(suppressWarnings(update(smallModel, REML=FALSE)))
    ## logL_large <- logLik(update(largeModel, REML=FALSE))

    logL_small <- logLik(smallModel, REML=FALSE)
    logL_large <- logLik(largeModel, REML=FALSE)

    tobs     <- 2 * (logL_large - logL_small)
    df11     <- attr(logL_large, "df") - attr(logL_small, "df")
    p.X2     <- 1 - pchisq(tobs, df11)
    c(tobs=tobs, df=df11, p.value=p.X2)
}

#' @export
getLRT.glmerMod <- function(largeModel, smallModel){
            ## cat("getLRT.glmerMod\n")
            logL_small <- logLik(smallModel)
            logL_large <- logLik(largeModel)
            tobs     <- 2 * (logL_large - logL_small)
            ## str(list(logL_small=logL_small, logL_large=logL_large, tobs=tobs))
            df11     <- attr(logL_large, "df") - attr(logL_small, "df")
            p.X2     <- 1 - pchisq(tobs, df11)
            c(tobs=tobs, df=df11, p.value=p.X2)
        }


#' @export
getLRT.gls <- function(largeModel, smallModel){

    logL_small <- logLik(update(smallModel, method="ML"))
    logL_large <- logLik(update(largeModel, method="ML"))

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









get_gls_data <- function(object){
    eval(object$call$data)
}

get_response_name <- function(object){
    all.vars(formula(object))[[1]]
}

gls_refit <- function(object, newresp){
    dat <- get_gls_data(object)
    y_var <- get_response_name(object)
    dat[[y_var]] <- newresp
    cl <- object$call
    cl$data <- dat
    eval(cl)
}

gls_refitML <- function(object){
    cl <- object$call
    cl$method <- "ML"
    eval(cl)
}
