library(caracas)
load_all("caracas")
options(caracas.print.method = "compactascii")

n <- 3
y <- vector_sym(n, "y")
X <- as_sym(rep(1, n))
H <- X %*% solve(t(X) %*% X, t(X))
R <- diag_(1, n) - H
p <- H %*% y
r <- R %*% y
scale_matrix(p, 1 / as_sym(n))
scale_matrix(r, 1 / as_sym(n))

rankMatrix_(H)
rankMatrix_(R)

## Orthogonal basis for C(R)
B <- QRdecomposition(R)$Q
B

## BB' is the orthogonal projection onto C(R)
B %*% t(B) - R

## Any vector in C(R) has the form Bb. In particular, the residuals
## r=Ry has this form: Bb=Ry. Since B is orthonormal, we have

b <- t(B) %*% r  |> simplify()
b



## ns0 <- function(x){
##     ## nullspace
##     rr <- rref(x)
##     b <- rr$mat[, -rr$pivot_vars, drop=FALSE]
##     if (length(rr$pivot_vars))
##         b <- rbind(b[rr$pivot_vars, ,drop=FALSE], diag_(1, length(rr$pivot_vars)))
##     b
## }

## cs0 <- function(x){
##     ## columnspace
##     rr <- rref(x)
##     b <- x[, rr$pivot_vars, drop=FALSE]
##     b
## }

## rs0 <- function(x){
##     ## rowspace
##     rr <- rref(t(x))
##     b <- t(x)[, rr$pivot_vars, drop=FALSE]
##     t(b)
## }

## ns <- function(x){
##     vv = sympy_func(x, "nullspace")
##     ww <- vv$pyobj |> reticulate::py_to_r()
##     xx <- ww  |> lapply(caracas:::construct_symbol_from_pyobj)
##     do.call(cbind, xx)
## }

## cs <- function(x){
##     vv = sympy_func(x, "columnspace")
##     ww <- vv$pyobj |> reticulate::py_to_r()
##     xx <- ww  |> lapply(caracas:::construct_symbol_from_pyobj)
##     do.call(cbind, xx)
## }

## rs <- function(x){
##     vv = sympy_func(x, "rowspace")
##     ww <- vv$pyobj |> reticulate::py_to_r()
##     xx <- ww  |> lapply(caracas:::construct_symbol_from_pyobj)
##     do.call(rbind, xx)
## }

## ns(x)
## cs(x)
## rs(x)


