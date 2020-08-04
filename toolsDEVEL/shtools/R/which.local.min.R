## which.local.min <- function(x){
##   subseq(diff(x)<0,TRUE)$last[1]+1
## }


## Find indices of local minima of x. A value is a local minima if the
## function is decreasing 'nb' steps from the left and increasing 'nb'
## steps to the right.

which.local.min <- function(x, nb=1){
  rr <- rle(diff(x)<0)
  v <- rr$values
  l <- rr$lengths
  ## patch (if necessary) endpoints of v so that it is always T,F,T,F,...,T,F 
  ## patch (correspondingly) endpoints of l to have value 0 
  if (!v[1]){
    v <- c(TRUE, v)
    l <- c(0, l)
  }
  if (v[length(v)]){
    v <- c(v,FALSE)
    l <- c(l,0)
  } 
  
  m <- matrix(l, nc=2, byrow=T)
  ans <- rep(FALSE, nrow(m))
  for (ii in 1:nrow(m)){
    ans[ii] <- all(m[ii,][m[ii,]!=0]>=nb)
  }
  
  m2 <- matrix(cumsum(l), nc=2, byrow=T)
  res <- m2[ans,1]+1
  res
}
