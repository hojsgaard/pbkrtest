
corr.matrix <- function(S){
  temp <- diag(1/sqrt(diag(S)))
  temp <- zapsmall(temp%*%S%*%temp)
  dimnames(temp) <- dimnames(S)
  return(temp)
}


