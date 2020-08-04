## ######################################################################
##
## Auxillary functions for calculating contrasts, lsmeans etc
##
## Banff, August 2013, Søren Højsgaard
##
## ######################################################################

.get_xlevels <- function(obj){
  UseMethod(".get_xlevels")
}

.get_xlevels.default <- function(obj){
  obj$xlevels
}

.get_xlevels.mer <- function(obj){
  ans <- lapply(obj@frame,levels)
  ans <- ans[unlist(lapply(ans, length))>0]

  nn <- names(attr(terms(obj), "dataClasses"))
  ii <- match(names(ans), nn)
  ii <- ii[!is.na(ii)]
  ans <- ans[nn[ii]]
  ans
}

.get_xlevels.lmerMod <- function(obj){
  ##cat(".get_xlevels.lmerMod\n")
  ans <- lapply(obj@frame,levels)
  ans <- ans[unlist(lapply(ans, length))>0]

  nn <-attr(terms(obj), "term.labels")
  ii <- match(names(ans), nn)
  ii <- ii[!is.na(ii)]
  ans <- ans[nn[ii]]
  ##print(ans)
  ans
}

.get_contrasts <- function(obj){
  UseMethod(".get_contrasts")
}

.get_contrasts.default <- function(obj){
  obj$contrasts ## FIXME: check RL code
}

.get_contrasts.lmerMod <- function(obj){
  attr(model.matrix(obj), "contrasts")
}

.set_xlevels <- function(xlev, at){
  nam     <- names(xlev)
  nn      <- match(nam, names(at))
  nn      <- nn[!is.na(nn)]
  at.fact <- at[nn]
  xlev[names(at[nn])]   <- at.fact
  attr(xlev, "at.fact") <- at.fact
  xlev
}

.set_convals <- function(xlev, covariateVal){
  nam     <- names(xlev)
  nn      <- match(nam, names(covariateVal))
  nn      <- nn[!is.na(nn)]
  con.con <- covariateVal[nn]
  xlev[names(covariateVal[nn])] <- con.con
  xlev
}

.get_vartypes <- function(object){
  ## Common!!
  trms <- terms( model.frame( object ))
  trms <- delete.response( trms )
  att  <- attributes( trms )
  rhs.terms <- rownames(att$factors)[rowSums(att$factors)>0]
  rhs.class <- att$dataClass[match(rhs.terms, names(att$dataClass))]
  nums      <- rhs.terms[rhs.class=="numeric"]
  ## END

  fact      <- rhs.terms[rhs.class=="factor"]
  list(numeric=nums, factor=fact)
}


.get_covariate_ave <- function(object, at=NULL, tt=terms(object)){
  ## Common!!
  trms <- terms( model.frame( object ))
  trms <- delete.response( trms )
  att  <- attributes( trms )
  rhs.terms <- rownames(att$factors)[rowSums(att$factors)>0]
  rhs.class <- att$dataClass[match(rhs.terms, names(att$dataClass))]
  nums      <- rhs.terms[rhs.class=="numeric"]
  ## END

  ans  <- lapply(model.frame(object)[,nums, drop=FALSE], mean)
  nn   <- match(names(ans), names(at))
  nn   <- nn[!is.na(nn)]
  at.num <- at[nn]
  ans[names(at[nn])]  <- at.num
  attr(ans, "at.num") <- at.num
  ans
}



.getX <- function(object, newdata){
  UseMethod(".getX")
}

.getX.default <- function(object, newdata){
  tt <- terms(object)
  Terms  <- delete.response(tt)
  mf  <- model.frame(Terms, newdata, xlev = .get_xlevels(object))
  X   <- model.matrix(Terms, mf, contrasts.arg = .get_contrasts(object))
  attr(X,"assign")<-NULL
  attr(X, "contrasts") <- NULL
  X
}


.getX.merMod <- function(object, newdata){
   tt <- terms(object)
   Terms  <- delete.response(tt)
   mf  <- model.frame(Terms, newdata, xlev = .get_xlevels(object))
   X   <- model.matrix(Terms, mf, contrasts.arg = .get_contrasts(object))
#  X <- getME(object, "X")
   attr(X,"assign")<-NULL
   attr(X, "contrasts") <- NULL
   X
}







