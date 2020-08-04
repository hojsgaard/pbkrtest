diffBy <- function(formula, data= parent.frame(), lag=1, differences=1, na.pad=TRUE)
{
  mf <- match.call(expand.dots = FALSE)
  form <- eval.parent(mf[[2]])

  lhs <- .lhsVar(formula)
  rhs <- .rhsVar(formula)

  print(rhs)
  if (identical(rhs, "1")){
    facstr <- rep.int(1,nrow(data))
    ooostr <- 1
  } else {
    xt <- xtabs(~., data[,rhs,drop=FALSE])
    ooo <- as.data.frame.table(xt, usNA="always")[,1:length(rhs),drop=FALSE] 
    ooostr <- apply(ooo,1,paste,collapse="|")
    facstr <- apply(data[,rhs,drop=FALSE],1,paste,collapse="|")
  }

  
  ans <- vector("list", length(ooostr))
  names(ans) <- ooostr

  if (na.pad){
    pad <- matrix(NA, nc=length(lhs), nr=differences)
  } else {
    pad <- NULL
  }

  namestr <- paste("diff",differences, lhs, sep='.')
  
  for (ii in 1:length(ooostr)){
    zz  <-
      rbind(pad,
            diff(as.matrix(data[ooostr[ii]==facstr,lhs,drop=FALSE]),
                 lag=lag, differences=differences))
    colnames(zz) <- namestr
    rownames(zz) <- NULL
    ans[[ii]] <- zz
  }
  
  ans
}


# d.var: a character vector
# d: dataframe
# drop: should unused levels be dropped?
splitBy2 <- function(d.var, data, drop=FALSE){

  xt <- xtabs(~., data[,d.var,drop=FALSE])
  ooo <- as.data.frame.table(xt, usNA="always")[,1:length(d.var),drop=FALSE]
  ooostr <- apply(ooo,1,paste,collapse="|")
  facstr <- apply(data[,d.var,drop=FALSE],1,paste,collapse="|")
  idx <- match(facstr, ooostr)
  
  ans <- vector("list", length(ooostr))
  names(ans) <- ooostr
  for (ii in 1:length(ooostr)){
    ans[[ii]] <- data[ooostr[ii]==facstr,,drop=FALSE]
  }
  
  if (drop==TRUE)
    ans <- ans[lapply(ans,nrow)>0]

  attr(ans,"dn") <- dimnames(xt) #FIXME: Not needed
  attr(ans,"di") <- dim(xt)      #FIXME: Not needed
  attr(ans,"xt") <- xt
  attr(ans,"idx") <- ooostr
  return(ans)
}

