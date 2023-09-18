





pdiff <- function(object, effect=NULL, at=NULL, ...){
    K     <- linestMatrix(object, effect=effect, at=at, ...)
    pm    <- .do_pairs( K )
    DK    <- pm$DD %*% K
    .coef <- .do.linest( object, DK)
    res   <- list(coef=.coef, grid=pm$grid, K=DK)
    class(res)<-"linest"
    res
}
