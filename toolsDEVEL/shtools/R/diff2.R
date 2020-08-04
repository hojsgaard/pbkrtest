diff2 <- function(x, lag = 1, differences = 1, pad=FALSE, val=NA, ...){

  ans <- diff(x,lag=lag, differences=differences)
  if (pad){
    if (class(x)=="matrix"){
      ans <- rbind(matrix(val, nr=nrow(x)-nrow(ans), nc=ncol(x)), ans)
    } else {
      ans <- c(rep(val, length(x)-length(ans)), ans)
    }
  }
  ans
}
