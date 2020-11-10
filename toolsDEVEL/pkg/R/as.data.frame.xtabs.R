

as.data.frame.xtabs <- function(tt){
  if (length(dim(tt))!=2)
    stop("Only 2-dimensional tables can be converted to dataframe\n") 
  dn <- attr(tt,"dimnames")
  tt <- ftable(tt)
  dimnames(tt) <- dn
  class(tt) <- 'matrix'
  as.data.frame(tt)
}


