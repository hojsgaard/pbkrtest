
addwindow <- function(x, p=1){
  ist <- as.logical(subseq(x)$value)==TRUE
  
  ss <- subseq(x)$first + (subseq(x)$last-subseq(x)$first)/2
  ss <- ss[ist]
  
  
  b <- floor(ss-p)
  b[b<1] <- 1
  e <- round(ss+p)
  e[e>length(x)] <- length(x)
  
  m <- cbind(b,e)
  
  ans <- rep(0,length(x))
  for (ii in 1:nrow(m)){
    ans[m[ii,1]:m[ii,2]] <- 1
  }
  as.logical(ans)
}
