#' @title Coerce PBmodcomp objects to data frames
#'
#' @description
#' Convert the test result table of a \code{PBmodcomp} or \code{summary.PBmodcomp} object
#' into a standard data frame for easy inspection or export.
#'
#' @param x An object of class \code{PBmodcomp} or \code{summary_PBmodcomp}.
#' @param ... Further arguments (currently ignored).
#'
#' @return
#' A \code{data.frame} containing the test results with attributes preserved.
#'
#' @details
#' These methods extract the internal \code{test} component (a data frame of test statistics and p-values)
#' from the object and return it as a standard data frame. The original object's attributes are preserved
#' as attributes on the returned data frame.
#'
#' This is useful for exporting or manipulating bootstrap test results in a tidy form.
#'
#' @examples
#' \dontrun{
#'   sleepstudy$Days2 <- sleepstudy$Days^2
#'   lmer_fit1 <- lme4::lmer(Reaction ~ Days + Days2 + (1 | Subject), data = sleepstudy, REML=FALSE)
#'   lmer_fit0 <- update(lmer_fit1, . ~ . - Days2)
#'
#'   res <- pb2_modcomp(lmer_fit1, lmer_fit0, nsim = 200)
#'   df  <- as.data.frame(res)
#'   head(df)
#'
#'   s <- summary(res)
#'   as.data.frame(s)
#' }
#'
#' ## @seealso \code{\link{pb2_modcomp}}, \code{\link{summary.PBmodcomp}}
#'
#' @export
as.data.frame.PBmodcomp <- function(x, ...) {
  out <- x$test
  attributes(out) <- c(attributes(out), x[-1])
  out
}

#' @rdname as.data.frame.PBmodcomp
#' @export
as.data.frame.summary_PBmodcomp <- function(x, ...) {
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
