orthogonal_complement<-function(W) {
    ##orthogonal complement of <W>: <W>orth=<Worth>
    rW    <- rankMatrix_(W)
    Worth <- qr.Q(qr(cbind(W)),
                  complete=TRUE)[,-c(1:rW), drop=FALSE]
    Worth
}

.spur<-function(U){
    sum(diag(U))
}

make_sparse<-function(X) {
  X <- as.matrix( X )
  w <- cbind( c(row(X)), c(col(X)), c(X))
  w <- w[ abs( w[,3] ) > 1e-16, , drop = FALSE]
  Y <- sparseMatrix( w[,1], w[,2], x=w[,3], dims=dim(X))
}



##if A is a N x N  matrix A[i,j]
## and R=c(A[1,1],A[1,2]...A[1,n],A[2,1]..A[2,n],, A[n,n]
## A[i,j]=R[r]
## .ij2r<-function(i,j,N)
##   (i-1)*N+j

.indexSymmat2vec <- function(i,j,N) {
  ## S[i,j] symetric N times N matrix
  ## r the vector of upper triangular element  in row major order:
  ## r= c(S[1,1],S[1,2]...,S[1,j], S[1,N], S[2,2],...S[N,N]
  ##Result: k: index of k-th element of r
  k <-if (i <= j) {
    (i - 1) * (N - i / 2) + j
  } else {
    (j - 1) * (N - j / 2) + i
  }
}

## FIXME indexVec2Symmat looks suspicious...
.indexVec2Symmat<-function(k,N) {
  ## inverse of indexSymmat2vec
  ## result: index pair (i,j) with i>=j
  ## k: element in the vector of upper triangular elements
  ## example: N=3: k=1 -> (1,1), k=2 -> (1,2), k=3 -> (1,3), k=4 -> (2,2)
  aa    <- cumsum(N:1)
  aaLow <- c(0,aa[-length(aa)])
  i     <- which(aaLow < k & k <= aa)
  j     <- k - N * i + N - i * (3 - i) / 2 + i
  return(c(i, j))
}

.index2UpperTriEntry <- .indexVec2Symmat

.divZero <- function(x, y, tol=1e-14){
    ## ratio x/y is set to 1 if both |x| and |y| are below tol

    if (abs(x) < tol & abs(y) < tol) 1
    else x / y
}

## FIXME Get rid of these
.is.lmm <- function(object) {
    inherits(object, "lmerMod")
}

.is.mm <- function(object) {
    inherits(object, "merMod")
}







































