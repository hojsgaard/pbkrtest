## #######################################################################################
##
## Banff, August 2013
##
## Eventually popMeans will be replaced by the more general linest function
##
## #######################################################################################


popMeans <- function(object, effect=NULL, at=NULL, only.at=TRUE, engine="esticon", grid=TRUE, ...){
  UseMethod("popMeans")
}

popMeans.default <- function(object, effect=NULL, at=NULL, only.at=TRUE, engine="esticon", grid=TRUE, ...){
  cl  <- match.call()
  mm  <- popMatrix(object, effect=effect, at=at, only.at=only.at)

  ans <- do.call(engine, list(object, mm,...))
  xtra <- attributes(mm)[[c("grid")]]

  if (engine=="esticon"){
    attr(ans,"X")    <- mm
    attr(ans,"call") <- cl
    if (grid && !is.null(xtra))
      ans <- cbind(ans, xtra)
    class(ans) <- c("popMeans", "conMeans", "data.frame")
  }
  attributes(ans)[c("grid","at")] <- attributes(mm)[c("grid","at")]
  ans
}

popMeans.lme <- function(object, effect=NULL, at=NULL, only.at=TRUE, engine="esticon", grid=TRUE, ...){
  cl <- match.call()
  lm.pseudo <- lm(object,data=object$data)
  cl[[2]]   <- lm.pseudo
  cl[[1]]   <- as.name("popMeans.default")
  eval(cl)
}

summary.conMeans <- function(object,...){
  print(object)
  cat("Call:\n")
  print(attr(object,"call"))
  cat("Contrast matrix:\n")
  summary(attr(object,"X"))
}

LSMEANS         <- popMeans
LSMEANS.default <- popMeans.default
LSMEANS.lme     <- popMeans.lme


