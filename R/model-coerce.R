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
#'
#' @param largeModel,smallModel Model objects of the same "type". Possible types
#'     are linear mixed effects models and linear models (including generalized
#'     linear models)
#' @param LL A restriction matrix.
#' @param sparse Should the restriction matrix be sparse or dense?
#' @param REML Controls if new model object should be fitted with REML or ML.
#' @param ... Additional arguments; not used.
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

#' @export
model2remat.default <- function (largeModel, smallModel, sparse=FALSE) {
    stop("No useful default method for 'model2remat'")
}

#' @method model2remat merMod
#' @export
model2remat.merMod <- function (largeModel, smallModel, sparse=FALSE) {
    ## cat("model2remat.merMod\n")
    ## print(smallModel)
    L <- if (is.numeric(smallModel)) {
             force_full_rank(smallModel)
         } else  { #smallModel is lmerMod
             make_remat(getME(largeModel, 'X'),  getME(smallModel, 'X'))
         }
    if (sparse) .makeSparse(L) else L
}

#' @method model2remat lm
#' @export
model2remat.lm <- function (largeModel, smallModel, sparse=FALSE) {
    L <- if (is.numeric(smallModel)) {
             force_full_rank(smallModel)
         } else  { 
             make_remat(model.matrix(largeModel), model.matrix(smallModel))
         }
    if (sparse) .makeSparse(L) else L
}






#' @rdname model-coerce
#' @export
remat2model <- function(largeModel, LL, REML=TRUE, ...){
  UseMethod("remat2model")
}

#' @export
remat2model.default <- function(largeModel, LL, REML=TRUE, ...){
    stop("No useful default method for 'remat2model'")
}

remat2model_internal <- function(largeModel, LL, XX.lg){
    form <- as.formula(formula(largeModel))    
    attributes(XX.lg)[-1] <- NULL
    XX.sm <- make_modelmat(XX.lg, LL)
    
    ncX.sm  <- ncol(XX.sm)
    colnames(XX.sm) <- paste(".X", 1:ncX.sm, sep='')
    
    rhs.fix2 <- paste(".X", 1:ncX.sm, sep='', collapse="+")
    new_form  <- .formula2list(form)
    
    zzz <- list(new_form=new_form, rhs.fix2=rhs.fix2, XX.sm=XX.sm)
    zzz
}


## #' @rdname model-coerce
#' @export
remat2model.merMod <- function(largeModel, LL, REML=TRUE, ...){

    zzz  <- remat2model_internal(largeModel, LL, getME(largeModel, "X"))
    
    new.formula <- as.formula(paste(zzz$new_form$lhs, "~ -1+", zzz$rhs.fix2,
                                    "+", zzz$new_form$rhs.ran))
    new.data    <- cbind(zzz$XX.sm, eval(largeModel@call$data))
    ans <- update(largeModel, eval(new.formula), data=new.data)
    if (!REML) ans <- update(ans, REML=FALSE)
    ans
}

## #' @rdname model-coerce
#' @export
remat2model.lm <- function(largeModel, LL, ...){
    
    zzz  <- remat2model_internal(largeModel, LL, model.matrix(largeModel))
    
    new.formula <- as.formula(paste(zzz$new_form$lhs, "~ -1+", zzz$rhs.fix2))
    new.data    <- as.data.frame(cbind(zzz$XX.sm, eval(largeModel$model)))
    ans <- update(largeModel, eval(new.formula), data=new.data)
    ans
}




## ##############################################################

## X is model matrix for large model; L is a restriction matrix;
## Output X2 is the corresponding model matrix for the corresponding
## smaller model.

## FIXME: make_modelmat should be exported?

make_modelmat <- function(X, L) {
    ##cat("X:\n"); print(X); cat("L:\n"); print(L)
    ## find A such that <A>={X b| b in Lb=0}

    if (!inherits(L, c("matrix", "Matrix")) )
        L <- matrix(L, nrow=1)
    L <- as(L, "matrix")
    if (ncol(X) != ncol(L)) {
        print(c( ncol(X), ncol(L) ))
        stop('Number of columns of X and L not equal \n')
    }
    X2 <- X %*% .orthComplement(t(L))
    X2
}

## ##############################################################

## X is model matrix for large model; X2 is model matrix for small
## model. Output is restriction matrix L

## FIXME: make_remat should be exported?

make_remat <- function(X, X2) {
  ## <X2> in <X>
  ## determine L such that  <X2>={Xb| b in Lb=0}
  d <- rankMatrix(cbind(X2, X)) - rankMatrix(X)
  if (d > 0) {
    stop('Error: <X2> not subspace of <X> \n')
  }
  Q  <- qr.Q(qr(cbind(X2, X)))
  Q2 <- Q[, (rankMatrix(X2) + 1) : rankMatrix(X)]
  L  <- t(Q2) %*% X
  ## Make rows of L2 orthogonal
  L <- t(qr.Q(qr(t(L))))
  L
}






force_full_rank <- function(L){
    ## ensures that restriction matrix L is of full row rank:
    if (is.numeric(L) && !is.matrix(L))
        L <- matrix(L, nrow=1)
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




## .rematBA <- function(B,A) {
##   ## <A> in <B>
##   ## determine L such that  <A>={Bb| b in Lb=0}
##   d <- rankMatrix(cbind(A,B)) - rankMatrix(B)
##   if (d > 0) {
##     stop('Error:  <A> not subspace of <B> \n')
##   }
##   Q  <- qr.Q(qr(cbind(A, B)))
##   Q2 <- Q[, (rankMatrix(A) + 1) : rankMatrix(B)]
##   L  <- t(Q2) %*% B
##   ##make rows of L2 orthogonal
##   L <- t(qr.Q(qr(t(L))))
##   L
## }



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



    ## ## common
    ## form <- as.formula(formula(largeModel))
    ## attributes(XX.lg)[-1] <- NULL
    ## XX.sm <- zapsmall(make_modelmat(XX.lg, LL))
    
    ## ncX.sm  <- ncol(XX.sm)
    ## colnames(XX.sm) <- paste(".X", 1:ncX.sm, sep='')
    
    ## rhs.fix2 <- paste(".X", 1:ncX.sm, sep='', collapse="+")
    ## new_form  <- .formula2list(form)
    ## ### !common
