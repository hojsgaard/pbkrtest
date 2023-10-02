\name{utils}
\alias{.makeSparse}
\alias{.ij2r}
\alias{.indexSymmat2vec}
\alias{.indexVec2Symmat}
\title{Utility functions}
\description{
  Utility functions not te be used directly by the user.
}
\usage{
.makeSparse(X)
.ij2r(i,j,N)
.indexSymmat2vec(i,j,N)
.indexVec2Symmat(k,N) 
	 }
\arguments{
  \item{X}{A matrix to be turned into a sparse matrix.}
  \item{N}{number of columns of a matrix}
  \item{i}{row index of a matrix}
  \item{j}{column index of a matrix}
  \item{k}{vector index}
}
\details{
  \code{.makeSparse} turns a matrix into a sparse matrix using
  \code{\link{sparseMatrix}} of the \code{\link{Matrix}} package.
  \code{.ij2r}, \code{.indexSymmat2vec} and
  \code{.indexVec2Symmat}  are index functions.
  Let \code{Q[i,j]} be a quadratic matrix of dimesnion N and
  \code{q=Q[1,1],Q[1,2],...,Q[1,N],Q[2,1],..Q[N,N]} the row major
  vectorization of the matrix.
  Then \code{Q[i,j]<-q[.ij2r(r)]}.
  Let \code{S[i,j]} be a symmetric matrix of dimension N and
  \code{s=S[1,1],S[1,2],...,S[1,N],S[2,3],..,S[N-1,N],S[N,N]} the row major
  vecotorization of the upper triangular part of S.
  Then    \code{s[indexSymmat2vec(i,j,N)]<-S[i,j]} and
  \code{S[indexVec2Symmat(r,N)]<-s[r]}. 
}
\seealso{
  \code{\link{KRmodcomp},\link{lmer}}.
}

\keyword{function}
