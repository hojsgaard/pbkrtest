

lsm <- function(object, eff=NULL, at=NULL){
  UseMethod("lsm")
}

lsm.geeglm <- lsm.lm <- function(object, eff=NULL, at=NULL){

  ## Data frame 
  mmm <- object$model

  ## Attributes for variables
  att <- attributes(object$terms)
  
  rhs.terms <- rownames(att$factors)[rowSums(att$factors)>0]
  rhs.class <- att$dataClass[match(rhs.terms, names(att$dataClass))]
  
  ## get names of factors in model
  facts <- rhs.terms[rhs.class=="factor"]

  ## get those factors NOT given in 'eff' 
  cfact <- setdiff(facts, eff)

  ## get factor levels for factors given in 'eff'
  v.lev <- lapply(mmm[,eff,drop=FALSE], levels)

  ## get levels of those factors not in 'eff'
  cfact.lev <- lapply(mmm[,cfact, drop=FALSE], levels)

  ## get numeric variables and their values
  nums     <- rhs.terms[rhs.class=="numeric"]
  cnum.val <- lapply(mmm[,nums, drop=FALSE], mean)

  cov.val <- c(cnum.val, cfact.lev)
  
  if ((!is.null(at))){
    nnn <- names(at)
    ##print(nnn)
    for (ii in 1:length(at)){
      jjj <- match(nnn[ii], names(cov.val))
      if (!is.na(jjj)){
        cov.val[[jjj]] <- at[[ii]]
      }
    }
  }

  ## combinations of v.lev
  v.comb  <- expand.grid(v.lev)
  v.list  <- lapply(v.comb, as.character)

  if (length(v.list)>0){
    ## create list of these combinations (can be done easier).
    v.list  <- do.call(cbind, v.list)
    v.list2 <- vector("list", nrow(v.list))
    for (ii in 1:nrow(v.list)){
      v.list2[[ii]] <- as.list(v.list[ii,])
    }
    res0 <- lapply(v.list2, function(zz){
      a  <- c(zz, cov.val)
      atat <<- a
      cc <- contrast(object, a=a, type="average", X=TRUE)
      cc
    })
  } else {
    a    <- c(cov.val)
    cc   <- contrast(object, a=a, type="average", X=TRUE)
    res0 <- list(cc)
  }

  X <- do.call(rbind, lapply(res0, "[[", "X"))

  ## select relevant part
  sel     <- lapply(res0, function(zzz){ unlist(zzz[1:7], use.names=TRUE) })
  lsm.tab <- as.data.frame(do.call(rbind, sel))
  names(lsm.tab) <- names(res0[[1]][1:7]) ## Shouldn't be necessary...

  ## FIXME: Add variables fixed in the at= argument to the output
  if (length(v.list)>0){
    aux <- v.comb
    ans <- cbind(lsm.tab, aux)
  } else {
    ans <- lsm.tab
  }
  
  
  attr(ans, "effects") <- eff
  attr(ans, "at")      <- at
  attr(ans, "X")       <- X
  attr(ans, "call")    <- object$call
  class(ans) <- c("lsmeansTable", "data.frame")
  return(ans)
}


print.lsmeansTable <- function(x, ...){

  cat(sprintf("LSMEANS table\n"))
  sss <- sprintf(" effect: %s", paste(attr(x,"effects"),collapse="*"))

  at <- unlist(attr(x,"at"))
  if (length(at)>0){
    qqq <- sprintf("at: %s\n", paste(paste(names(at), at, sep="="), collapse=" "))
    sss <- paste(sss,qqq, sep=" ")
  }

  cat(sss, "\n")
  print.data.frame(x)
  invisible()
}



lsm.mer <- function(object, eff=NULL, at=NULL){

  mmm <- object@frame
  att   <- attributes(object@frame)$terms
  attat <- attributes(att)

  rhs.terms <- rownames(attat$factors)[rowSums(attat$factors)>0]
  rhs.class <- att$dataClass[match(rhs.terms, names(attat$dataClass))]
  
  ## get names of factors in model
  facts <- rhs.terms[rhs.class=="factor"]

  ## get those factors NOT given in 'eff' 
  cfact <- setdiff(facts, eff)

  ## get factor levels for factors given in 'eff'
  v.lev <- lapply(mmm[,eff,drop=FALSE], levels)

  ## get levels of those factors not in 'eff'
  cfact.lev <- lapply(mmm[,cfact, drop=FALSE], levels)

  ## get numeric variables and their values
  nums     <- rhs.terms[rhs.class=="numeric"]
  cnum.val <- lapply(mmm[,nums, drop=FALSE], mean)

  cov.val <- c(cnum.val, cfact.lev)
  
  if ((!is.null(at))){
    nnn <- names(at)
    ##print(nnn)
    for (ii in 1:length(at)){
      jjj <- match(nnn[ii], names(cov.val))
      if (!is.na(jjj)){
        cov.val[[jjj]] <- at[[ii]]
      }
    }
  }
  
  ## combinations of v.lev
  v.comb  <- expand.grid(v.lev)
  v.list  <- lapply(v.comb, as.character)

  if (length(v.list)>0){
    ## create list of these combinations (can be done easier).
    v.list  <- do.call(cbind, v.list)
    v.list2 <- vector("list", nrow(v.list))
    for (ii in 1:nrow(v.list)){
      v.list2[[ii]] <- as.list(v.list[ii,])
    }
    res0 <- lapply(v.list2, function(zz){
      a  <- c(zz, cov.val)
      cc <- contrast(object, a=a, type="average", X=TRUE)
      cc
    })
  } else {
    a    <- c(cov.val)
    cc   <- contrast(object, a=a, type="average", X=TRUE)
    res0 <- list(cc)
  }

  X <- do.call(rbind, lapply(res0, "[[", "X"))

  ## select relevant part
  sel     <- lapply(res0, function(zzz){ unlist(zzz[1:7], use.names=TRUE) })
  lsm.tab <- as.data.frame(do.call(rbind, sel))
  names(lsm.tab) <- names(res0[[1]][1:7]) 

  ## FIXME: Add variables fixed in the at= argument to the output
  if (length(v.list)>0){
    aux <- v.comb
    ans <- cbind(lsm.tab, aux)
  } else {
    ans <- lsm.tab
  }
  
  attr(ans, "effects") <- eff
  attr(ans, "at")      <- at
  attr(ans, "X")       <- X
  attr(ans, "call")    <- object@call
  class(ans) <- c("lsmeansTable", "data.frame")
  return(ans)
}

