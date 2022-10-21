##' # Change of coordinate and basis
##'
##' 

##' Define
X1 <- matrix(c(1,1,1,1,0,0,1,1), nc=2)
X2 <- matrix(c(1,1,0,0,0,0,1,1), nc=2)
X1;X2

##' Then L=C(X1) = C(X2) where C(A) is the column space of A where A
##' has linearly independent columns. Write v in L as
##'
##' v = X1 v1 = X2 v2
##'
##' where vi are the coordinates to v in the basis Xi.
##' 
##' The matrix C12 that transforms v1 to v2 is:
##'
##' X1 v1 = X2 v2 =>
##' t(X2) X1 v1 = t(X2) X2 v2 =>
##' v2 = (t(X2) X2)^{-1} t(X2) X1 v1
##'
C12 <- solve(t(X2) %*% X2, t(X2) %*% X1)
C12

##' The matrix that transforms v2 to v1 is 
C21 <- solve(C12)
C21

##' Example
v1 <- c(1,2)
v = X1 %*% v1
v 

v2 <- C12 %*% v1
v2
v <- X2 %*% v2
v

##' Moreover, since v2 = C12 v1 we have can write
##'
##' X1 v1 = X2 v2 = X2 C12 v1
##'
##' This holds for any v1, which gives us
##'
##' X1 = X2 C12
##'
##' Hence X1[,j] = X2 C12[,j] so the j'th column of C12 are the
##' coordinates of the j'th column of X1 expressed in the basis X2
##' 

X1
X2 %*% C12

##' To summarize:
##'
##' 1. v = (1,1,3,3) has coordinates v1=(1,2) in the basis X1 and
##'    coordinates v2=(1,3) in the basis X2.
##'
##' 2. C12 transforms coordinates relative to X1 into coordinates
##'    relative to X2
##'
##'    v2 = C12 v1
##'
##' 3. C12 transforms then basis X2 into the basis X1:
##'
##'    X1 = X2 C12
##'



