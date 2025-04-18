## #' @title Extract (or "get") components from a \code{KRmodcomp} object.
## #' 
## #' @description Extract (or "get") components from a \code{KRmodcomp} object,
## #'     which is the result of the \code{KRmodcomp} function.
## #'
## #' @name get_modcomp
## #' 
## #' @param object A \code{KRmodcomp} object, which is the result of the
## #'     \code{KRmodcomp} function
## #' @param name The available slots. If \code{name} is missing or \code{NULL}
## #'     then everything is returned.
## #' @author Søren Højsgaard \email{sorenh@@math.aau.dk}
## #' @seealso \code{\link{KRmodcomp}}, \code{\link{PBmodcomp}},
## #'     \code{\link{vcovAdj}}
## #' @references Ulrich Halekoh, Søren Højsgaard (2014)., A Kenward-Roger
## #'     Approximation and Parametric Bootstrap Methods for Tests in Linear Mixed
## #'     Models - The R Package pbkrtest., Journal of Statistical Software,
## #'     58(10), 1-30., \url{https://www.jstatsoft.org/v59/i09/}
## #' @keywords utilities
## #' @examples
## #' 
## #' data(beets, package='pbkrtest')
## #' lg <- lmer(sugpct ~ block + sow + harvest + (1|block:harvest), 
## #'               data=beets, REML=FALSE)
## #' sm <- update(lg, .~. - harvest)
## #' modcomp <- KRmodcomp(lg, sm)
## #' getKR(modcomp, "ddf") # get denominator degrees of freedom.
## #' 
## #' 

## #' @export
## #' @rdname get_modcomp
## getKR <- function (object, name = c("ndf", "ddf", "Fstat", "p.value", "F.scaling", "FstatU", "p.valueU", "aux")) 
## {	
##   stopifnot(is(object, "KRmodcomp"))
##   if (missing(name) || is.null(name)){
##     return(stats)
##   } else {
##     stopifnot(length(name <- as.character(name)) == 1)
##     name <- match.arg(name)
##     object[[name]]
##   }
## }


## #' @export
## #' @rdname get_modcomp
## getSAT <- function (object, name = c("ndf", "ddf", "Fstat", "p.value")) 
## {	
##   stopifnot(is(object, "SATmodcomp"))
##   if (missing(name) || is.null(name)){
##     return(test) ## FIXME Should be stats
##   } else {
##     stopifnot(length(name <- as.character(name)) == 1)
##     name <- match.arg(name)
##     object[[name]] ## FIXME Should be stats
##   }
## }


## #' @export
## #' @rdname get_modcomp
## getPB <- function (object, name = c("ndf", "ddf", "Fstat", "p.value")) 
## {	
##   stopifnot(is(object, "PBmodcomp"))
##   if (missing(name) || is.null(name)){
##     return(test) ## FIXME Should be stats
##   } else {
##     stopifnot(length(name <- as.character(name)) == 1)
##     name <- match.arg(name)
##     object[[name]] ## FIXME Should be stats
##   }
## }
