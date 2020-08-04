## Consider:
## x  = (x1, ... ,xp), numeric
## bx = (b1, ... ,bp), logical
## y  = (y1, ... ,yq), numeric
##
## Each subsequence of T's in bx defines a corresponding subsequence
## of x given by intervals I1=(low1, up1), I2=(low2, up2), ....
## Return vector my=(m1, ..., mq) of logicals where each subsequene of
## T's are the indices of elements of y matching the intervals


changeResolution <- function(x,b,y,index=FALSE){
  sseq     <- subseq(b,item=TRUE)[,c("first","last")]
  lim     <- structure(x[as.matrix(sseq)],dim=dim(sseq))
  
  if (inherits(lim,"numeric"))
    lim <- matrix(lim, nr=1)

  lim <- split(lim, row(lim))
  
  ans <- lapply(lim,
                function(a) which((y>= a[1] & y<=a[2])))

  if (!index)
    ans <- lapply(ans, function(a) y[a])

  if (length(ans)==1)
    ans <- unlist(ans)
  
  return(ans)
}


