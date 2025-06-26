

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






