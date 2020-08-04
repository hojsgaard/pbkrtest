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

.get_xlevels.merMod <- function(obj){
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

.get_contrasts.merMod <- function(obj){
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


.get_null_basis <- function(object){
    S <- svd(model.matrix(object))
    null.basis <- S$v[,S$d<1e-6, drop=FALSE]
    null.basis
}

.is_estimable <- function(KK, null.basis){
    is.est <-
        unlist(lapply(1:nrow(KK),
                      function(ii){
                          kk <- KK[ii,]
                          all(abs(apply(null.basis, 2, function(x) sum(kk * x))) < 1e-04)
                      }))
    is.est
}

.do_contrast <- function(KK, bhat, V0, ddf, is.est){
    used       <- which(!is.na(bhat))
    not.used   <- which(is.na(bhat))
    bhat.used  <- bhat[used]
    VV <- V0
    ddfm <- function(kk, se) ddf
    res <- matrix(NA, nrow=nrow(KK), ncol=3)
    for (ii in 1:nrow(res)){
        if (is.est[ii]){
            kk   <- KK[ii,used]
            est  <- sum(kk*bhat.used)
            se   <- sqrt(sum(kk * (VV %*% kk)))
            df2  <- ddfm(kk, se)
            res[ii,] <- c(est, se, df2)
        }
    }
    colnames(res) <- c("estimate","SE","df")
    res
}

.do_pairs <- function(KK){ ## pairwise differences of lsmeans
    NN <- nrow(KK)
    MM <- ncol(KK)
    SS <- as.matrix(attr(KK,"grid")) ## todo: check if null
    EE <- vector("list", NN*(NN-1)/2)
    DD <- matrix(0, nrow=NN*(NN-1)/2, ncol=NN)
    kk <- 1
    for (ii in 1:(NN-1)){
        for (jj in (ii+1):NN){
            DD[kk, ii] <- 1
            DD[kk, jj] <- -1
            EE[[kk]] <- list(ff=SS[ii,], .ff=SS[jj,])
            kk <- kk + 1
        }
    }
    ff <- do.call(rbind, lapply(EE, function(x) x$ff))
    .ff <- do.call(rbind, lapply(EE, function(x) x$.ff))
    colnames(.ff) <- paste(".", colnames(.ff),sep='')
    pair <- as.data.frame(cbind(ff, .ff))
    list(DD=DD, grid=pair)
}





