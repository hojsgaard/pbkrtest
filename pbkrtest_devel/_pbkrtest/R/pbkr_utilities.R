

as.data.frame.PBmodcomp <- function(x, ...){
    out <- x$test
    attributes(out) <- c(attributes(out), x[-1])
    out
}

as.data.frame.summary_PBmodcomp <- function(x, ...){
    out <- x$test
    attributes(out) <- c(attributes(out), x[-1])
    out
}








#' @export 
tidy.PBmodcomp <- function(x, ...){
    ret <- x$test    
    as_tibble(cbind(type=rownames(ret), ret))
}

#' @export 
tidy.summary_PBmodcomp <- function(x, ...){
    ret <- x$test    
    as_tibble(cbind(type=rownames(ret), ret))
}

#' @export 
tidy.summary_KRmodcomp <- function(x, ...){
    ret <- x##$test    
    as_tibble(cbind(type=rownames(ret), ret))
}

#' @export 
tidy.KRmodcomp <- function(x, ...){
    F.scale <- x$aux['F.scaling']
    tab <- x$test

    FF.thresh <- 0.2
## ttt <<- tab
    
    if (max(F.scale) > FF.thresh)
        i <- 1
    else
        i <- 2


    ret <- x$test[i,,drop=FALSE]
    ret$F.scaling <- NULL
    as_tibble(cbind(type=rownames(ret), ret))
}

#' @export 
tidy.SATmodcomp <- function(x, ...){
    ret <- x$test
    as_tibble(cbind(type="Ftest", ret))    
}

#' @export 
as.data.frame.PBmodcomp <- function(x, ...){
    x$test
}

#' @export 
as.data.frame.KRmodcomp <- function(x, ...){
    x$test
}

#' @export 
as.data.frame.SATmodcomp <- function(x, ...){
    x$test
}
