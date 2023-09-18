imputeBLP <- function(x, parms=cov.wt(na.omit(x), method="ML"), details=0){

  x.new <- x

  if(sum(is.na(x))>0){ # Yes, there are missing values
  
    ## Find pattern of missing data
    pat <- rep(NA,nrow(x))
    for (kk in 1:nrow(x)){
      sss <- x[kk,]
      obs <- which(!is.na(sss))
      mis <- which(is.na(sss))
      pat[kk] <- paste(paste(mis,collapse=","), paste(obs,collapse=","),sep="|")
    }
    
    unpat <- unique(pat)
    if (details>0)
      cat(sprintf("mo-patterns = %s\n", toString(unpat)))
    
    if (inherits(parms, "matrix")){
      if (nrow(parms)!=ncol(parms) || !isSymmetric(parms)){
        stop("'parms' appears not to be a covariance matrix")
      } else {
        parms <- list(cov=parms, center=rep(0,ncol(parms)))
      }
    } else {
      if (inherits(parms,"list")) {
        if (is.null(parms$center))
          parms$center <- rep(0,ncol(parms))
      } else {
        stop("'parms' must be a list (with components 'cov' and 'center') or a covariance matrix")
      }
    }

    if(ncol(x) != ncol(parms$cov) || ncol(x) != length(parms$center) ){
      stop("dimensions of data 'x' and parms do not match\n")
    }
    
    for (kk in 1:length(unpat)){
      idx <- which(unpat[kk]==pat)
      if (details>0)
        cat(sprintf("kk = %3i mo-pattern = %10s \n", kk, unpat[kk]))
      sss <- x[idx[1],]
      obs <- which(!is.na(sss))
      mis <- which(is.na(sss))
      
      if (length(mis)>0){
        if (length(obs)>0){
          ddd <- t( x[idx,obs,drop=FALSE] )-parms$center[obs]
          sss <- {parms$cov[mis, obs,drop=FALSE] %*% solve(parms$cov[obs, obs, drop=FALSE])}
          zzz <- t(parms$center[mis] +   sss %*%  ddd )
        } else {
          zzz<-matrix(rep(parms$center[mis], each=length(idx)), nr=length(idx))
        }
        
        x.new[idx,mis] <- zzz
      }
    }
  }
  x.new
}
