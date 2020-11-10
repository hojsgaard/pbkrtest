
recon <- function(pc1, useidx=1){
  ## Reconstruct (approximation to) original data in PCA
  ##
  if (length(useidx)==1)
    useidx <- 1:useidx

  x   <- pc1$x
  rot <- pc1$rot
  if (pc1$scale[1]){
    rot <- diag(pc1$scale)%*%rot
  }
  
  pred <- x[,useidx,drop=F] %*% t(rot[,useidx,drop=F])

  if (pc1$center[1]){
    pred <- t(t(pred) + pc1$center)
  }
  
  colnames(pred) <- row.names(pc1$rot)
  return(pred)
}


# prcomp2 <- 
# function(x, retx = TRUE, center = TRUE, scale. = FALSE, tol = NULL, ...){
#   mcall <- match.call(expand.dots=TRUE)
#   mcall[[1]] <- as.name("prcomp")
#   print(mcall)
#   print(mcall$na.action)
#   return(mcall)
# }
