## ##' @title Compute RMSEP for mvr object
## ##'
## ##' @param object An mvr object.
## ##' @param ... Additional arguments, not used
## ##'
## ##' @importFrom pls RMSEP
## ##' @export
## RMSEP2 <- function(object, ...){
##     if (!is(object, "mvr")){
##         stop("Argument 'object' must be 'mvr' object\n")
##     }
    
##     o <- pls::RMSEP(object, ...)
##     o$validation <- object$call$validation
##     class(o) <- c("mvrVal2", class(o))
##     o
## }

##' @export
as.data.frame.mvrVal <- function(x, row.names = NULL, optional = FALSE, ...){
    out <- x$val |>
        ftable() |>
        as.data.frame(row.names=row.names, optional=optional, ...)
    out$comps <- as.integer(x$comps)
    out$val   <- out$Freq
    out$Freq  <- NULL
    out$validation <- x$validation
    out$model <- NULL
    out
}

##' @export
as.double.mvrVal <- function(x, ...){
    out <- c(x$val)
    names(out) <- paste0("comps",x$comps)
    out
}

## ##' @export
## print.mvrVal2 <- function(x, ...){
##     print(as.numeric(x))
## }
