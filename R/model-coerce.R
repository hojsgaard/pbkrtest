################################################################################
#' @title Conversion between a model object and a restriction matrix
#' 
#' @description Testing a small model under a large model corresponds
#'     imposing restrictions on the model matrix of the larger model
#'     and these restrictions come in the form of a restriction
#'     matrix. These functions converts a model to a restriction
#'     matrix and vice versa.
#' 
#' @name model-coerce
################################################################################
## #' @aliases model2remat model2remat.lm
## #'     model2remat.mer model2remat.merMod
## #'     remat2model remat2model.lm
## #'     remat2model.mer remat2model.merMod
#'
#' @param largeModel,smallModel Model objects of the same "type". Possible types
#'     are linear mixed effects models and linear models (including generalized
#'     linear models)
#' @param LL A restriction matrix.
#' @param sparse Should the restriction matrix be sparse or dense?
#' 
#' @return \code{model2remat}: A restriction matrix.
#'     \code{remat2model}: A model object.
#'
#' @note That these functions are visible is a recent addition; minor changes
#'     may occur.
#'
#' @author Ulrich Halekoh \email{uhalekoh@@health.sdu.dk}, Søren Højsgaard
#'     \email{sorenh@@math.aau.dk}
#'
#' @seealso \code{\link{PBmodcomp}}, \code{\link{PBrefdist}},
#'     \code{\link{KRmodcomp}}
#'
#' @references Ulrich Halekoh, Søren Højsgaard (2014)., A Kenward-Roger
#'     Approximation and Parametric Bootstrap Methods for Tests in Linear Mixed
#'     Models - The R Package pbkrtest., Journal of Statistical Software,
#'     58(10), 1-30., \url{http://www.jstatsoft.org/v59/i09/}
#' @keywords utilities
#' 
#' @examples
#' library(pbkrtest)
#' data("beets", package = "pbkrtest")
#' sug <- lm(sugpct ~ block + sow + harvest, data=beets)
#' sug.h <- update(sug, .~. - harvest)
#' sug.s <- update(sug, .~. - sow)
#' 
#' ## Construct restriction matrices from models
#' L.h <- model2remat(sug, sug.h); L.h
#' L.s <- model2remat(sug, sug.s); L.s
#' 
#' ## Construct submodels from restriction matrices
#' mod.h <- remat2model(sug, L.h); mod.h
#' mod.s <- remat2model(sug, L.s); mod.s
#' 
#' ## The models have the same fitted values
#' plot(fitted(mod.h), fitted(sug.h))
#' plot(fitted(mod.s), fitted(sug.s))
#' ## and the same log likelihood
#' logLik(mod.h)
#' logLik(sug.h)
#' logLik(mod.s)
#' logLik(sug.s)

#' @export model2remat
#' @rdname model-coerce
model2remat <- function (largeModel, smallModel, sparse=FALSE) {
    UseMethod("model2remat")
}

##    model2remat.mer <-
#' @method model2remat merMod
#' @export
model2remat.merMod <- function (largeModel, smallModel, sparse=FALSE) {
    L <- if (is.matrix(smallModel)) {
             force_full_rank(smallModel)
         } else  { #smallModel is mer model
             .rematBA(getME(largeModel,'X'),  getME(smallModel,'X'))
         }
    if (sparse) .makeSparse(L) else L
}

#' @method model2remat lm
#' @export
model2remat.lm <- function (largeModel, smallModel, sparse=FALSE) {
    L <- if(is.matrix(smallModel)) {
             force_full_rank(smallModel)
         } else  { #smallModel is mer model
             .rematBA(model.matrix(largeModel), model.matrix(smallModel))
         }
    ## print(L)
    if (sparse) .makeSparse(L) else L
}

#' @export
model2remat.default <- function (largeModel, smallModel, sparse=FALSE) {
    stop("No default method for 'model2remat'")
}


#' @rdname model-coerce
#' @export
remat2model <- function(largeModel, LL){
  UseMethod("remat2model")
}

##    remat2model.mer <-

## #' @rdname model-coerce
#' @export
remat2model.merMod <- function(largeModel, LL){

    XX.lg 	 <- getME(largeModel, "X")

    ## common
    form <- as.formula(formula(largeModel))    
    attributes(XX.lg)[-1] <- NULL
    XX.sm <- .restrictedModelMatrix(XX.lg, LL)
    
    ncX.sm  <- ncol(XX.sm)
    colnames(XX.sm) <- paste(".X", 1:ncX.sm, sep='')
    
    rhs.fix2 <- paste(".X", 1:ncX.sm, sep='', collapse="+")
    fff  <- .formula2list(form)
    ## !common
    
    new.formula <- as.formula(paste(fff$lhs, "~ -1+", rhs.fix2, "+", fff$rhs.ran))
    new.data    <- cbind(XX.sm, eval(largeModel@call$data))
    
    ans <- update(largeModel, eval(new.formula), data=new.data)
    ans
}

## #' @rdname model-coerce
#' @export
remat2model.lm <- function(largeModel, LL){
    
    XX.lg 	 <- model.matrix(largeModel)

    ## common
    form <- as.formula(formula(largeModel))
    attributes(XX.lg)[-1] <- NULL
    XX.sm <- zapsmall(.restrictedModelMatrix(XX.lg, LL))
    
    ncX.sm  <- ncol(XX.sm)
    colnames(XX.sm) <- paste(".X", 1:ncX.sm, sep='')
    
    rhs.fix2 <- paste(".X", 1:ncX.sm, sep='', collapse="+")
    fff  <- .formula2list(form)
    ### !common

    new.formula <- as.formula(paste(fff$lhs, "~ -1+", rhs.fix2))
    new.data    <- as.data.frame(cbind(XX.sm, eval(largeModel$model)))
    ans <- update(largeModel, eval(new.formula), data=new.data)
    ans
}

#' @export
remat2model.default <- function(largeModel, LL){
    stop("No default method for 'remat2model'")
}





## B is model matrix for large model; L is a restriction matrix;
## output is the corresponding model matrix for the corresponding
## smaller model.

## FIXME: .restrictedModelMatrix should be exported?

.restrictedModelMatrix <- function(B, L) {
    ##cat("B:\n"); print(B); cat("L:\n"); print(L)
    ## find A such that <A>={Bb| b in Lb=0}

    ## if (!is.matrix(L))
    ##     L <- matrix(L, nrow=1)

    if ( !inherits(L, c("matrix", "Matrix")) )
        L <- matrix(L, nrow=1)
    L <- as(L, "matrix")
    if ( ncol(B) != ncol(L) ) {
        print(c( ncol(B), ncol(L) ))
        stop('Number of columns of B and L not equal \n')
    }
    A <- B %*% .orthComplement(t(L))
    A
}


.rematBA <- function(B,A) {
  ## <A> in <B>
  ## determine L such that  <A>={Bb| b in Lb=0}
  d <- rankMatrix(cbind(A,B)) - rankMatrix(B)
  if (d > 0) {
    stop('Error:  <A> not subspace of <B> \n')
  }
  Q  <- qr.Q(qr(cbind(A, B)))
  Q2 <- Q[, (rankMatrix(A) + 1) : rankMatrix(B)]
  L  <- t(Q2) %*% B
  ##make rows of L2 orthogonal
  L <- t(qr.Q(qr(t(L))))
  L
}



## ## ensures  that L is of full row rank:
## LL <- smallModel
## q  <- rankMatrix(LL)
## if (q < nrow(LL)){
##     t(qr.Q(qr(t(LL)))[,1:qr(LL)$rank])
## } else {
##     smallModel
## }

## ## ensures  that L is of full row rank:
## LL <- smallModel
## q  <- rankMatrix(LL)
## if (q < nrow(LL) ){
##   t(qr.Q(qr(t(LL)))[,1:qr(LL)$rank])
## } else {
##   smallModel
## }


force_full_rank <- function(L){
    ## ensures that restriction matrix L is of full row rank:
    q  <- rankMatrix(L)
    if (q < nrow(L)){
        t(qr.Q(qr(t(L)))[ ,1:qr(L)$rank])
    } else {
        L
    }
}


.formula2list <- function(form){
  lhs <- form[[2]]
  tt  <- terms(form)
  tl  <- attr(tt, "term.labels")
  r.idx <- grep("\\|", tl)

  if (length(r.idx)){
    rane  <- paste("(", tl[r.idx], ")")
    f.idx <- (1:length(tl))[-r.idx]

    if (length(f.idx))
        fixe  <- tl[f.idx]
    else
        fixe  <- NULL
  } else {
    rane <- NULL
    fixe <- tl
  }

  ans <- list(lhs=deparse(lhs),
              rhs.fix=fixe,
              rhs.ran=rane)
  ans
}
