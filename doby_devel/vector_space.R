#' @title Finds the basis of the (right) null space.
#' 
#' @description Finds the basis of the (right) null space of a matrix, a vector
#'     (a 1-column matrix) or a model object for which a model matrix can be
#'     extracted. I.e. finds basis for the (right) null space x : Mx = 0.
#'
#' @name linear_algebra
#' 
#' @param M A matrix or a vector (a 1-column matrix).
#' @return A matrix (possibly with zero columns if the null space consists only
#'     of the zero vector).
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link[MASS]{Null}}
#' @keywords utilities
#' @examples
#' 
#' M <- matrix(c(1,1,1,1,1,1,0,0,0,0,1,1), nrow=4)
#' null_basis(M)
#' MASS::Null(t(M))
#' 
#' M <- c(1,1,1,1)
#' null_basis(M)
#' MASS::Null(t(M))
#' 
#' m0 <- lm(breaks ~ wool + tension, data=warpbreaks)
#' null_basis(model.matrix(m0))
#' MASS::Null(t(model.matrix(m0)))
#' 
#' ## Make balanced dataset
#' dat.bal   <- expand.grid(list(A=factor(1:2), B=factor(1:3), C=factor(1:3)))
#' dat.bal$y <- rnorm(nrow(dat.bal))
#' 
#' ## Make unbalanced dataset: 'B' is nested within 'C' so B=1 is only
#' ## found when C=1 and B=2,3 are found in each C=2,3,4
#' dat.nst <- dat.bal
#' dat.nst$C <-factor(c(1,1,2,2,2,2,1,1,3,3,3,3,1,1,4,4,4,4))
#' xtabs(y ~ C+B+A , data=dat.nst)
#' 
#' mod.bal  <- lm(y ~ A + B*C, data=dat.bal)
#' mod.nst  <- lm(y ~ A + B*C, data=dat.nst)
#' 
#' null_basis( model.matrix(mod.bal) )
#' null_basis( model.matrix(mod.nst) )
#' 
#' MASS::Null( t(model.matrix(mod.bal)) )
#' MASS::Null( t(model.matrix(mod.nst)) )
#' 
#' 
#' 
#' @export null_basis

MASS_Null <- 
function (M) 
{
  tmp <- qr(M)
  set <- if (tmp$rank == 0L) 
    seq_len(ncol(M))
  else -seq_len(tmp$rank)
  qr.Q(tmp, complete = TRUE)[, set, drop = FALSE]
}

#' @rdname linear_algebra
#' @export
orth_comp_basis <- function(M) {
    ##if(!(is.matrix(M) && is.numeric(M))) stop("M must be a numeric matrix")
    ##rM <- tmp$rank
    ##M_perp <- qr.Q(tmp, complete=TRUE)[ , -c(1:rM), drop=FALSE]
    tmp <- qr(M)
    seq <- -seq_len(tmp$rank) ## Notice -sign
    M_perp <- qr.Q(tmp, complete=TRUE)[ , seq, drop=FALSE]
    return(M_perp)  
}


#' @rdname linear_algebra
#' @export
col_basis <- function(M) {
    ##if(!(is.matrix(M) && is.numeric(M))) stop("M must be a numeric matrix")
    ## rM <- tmp$rank
    ##M_col <- qr.Q(tmp, complete=TRUE)[ , seq1:rM, drop=FALSE]

    tmp <- qr(M)
    seq <- seq_len(tmp$rank)
    M_col <- qr.Q(tmp, complete=TRUE)[ , seq, drop=FALSE]
  return(M_col)
}

#' @rdname linear_algebra
#' @export
row_basis <- function(M) {
  col_basis(t(M))
}


#' @rdname linear_algebra
#' @export
null_basis <- function(M) {
#  if(!(is.matrix(M) && is.numeric(M))) stop("M must be a numeric matrix")
#  orth_comp_basis(col_basis(t(M)))
  MASS_Null(t(M))
}

#' @rdname linear_algebra
#' @export
left_null_basis <- function(M) {
##    orth_comp_basis(col_basis(A))
    MASS_Null(M)
}






#' @export
## null_basis <- function(object){

##     .null_basis <- function(object){
##         S <- svd( object )
##         id <- S$d<1e-15
##         if (any(id)){
##             null.basis <- S$v[,id, drop=FALSE]
##             null.basis
##         }
##         else {
##             matrix(nrow=ncol(object), ncol=0)
##         }
##     }
    
##     if (is.character(object)){
##         stop("'object' of type 'character' not valid")
##     }
    
##     if (is.vector(object)){
##         object <- matrix(object, ncol=1)
##     }


##     ##if (class(object) %in% c("matrix","Matrix")){
##     ##    .null_basis(object)

##     if (inherits(object, c("matrix", "Matrix"))){
##         .null_basis(object)
##     } else {
##         m <- try(model.matrix(object), silent=TRUE)
##         ##if (class(m) != "try-error")
##         if (!inherits(m, "try-error"))
##             .null_basis(m)
##         else
##             stop("Can not find null basis for 'object'")
##     }
## }
