
extInterval <- function(x, item=1, w=0){

  len.x <- length(x)
  ext <- mid <- rep(0, len.x)
  
  zz  <- subseq(x,item)
  fl <- cbind(zz[,"first"] - w, zz[,"last"]  + w)
  fl[fl[,2]>len.x,2]  <- len.x
  fl[fl[,1]<1,1]      <- 1
  
  mid[zz$midpoint]<- 1
  
  for (ii in 1:nrow(fl)){
    ext[fl[ii,1]:fl[ii,2]] <- 1
  }

  list(ext, mid)
}

  
  
