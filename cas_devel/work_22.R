library(caracas)
load_all("caracas")

load_all()

L1 <- as_sym(matrix(c(1,1,1,1), byrow=T))
L2 <- as_sym(matrix(c(1,0,1,0,0,1,0,1), byrow=T, ncol=2))
L <- cbind(L1, L2)


columnspace(L)
get_basis(L)

x <- vector_sym(3)
get_basis(x)

W <- matrix(c("r_1", "r_1", "r_2", "r_2", "0", "0", "u_1", "u_2"), nrow=4)
W <- as_sym(W)
get_basis(W)



L1c <- nullspace(t(L1))
L2c <- nullspace(t(L2))

A <- cbind(L1c, -L2c)
An <- nullspace(A)


L1c %*% An[1:3,]
L2c %*% An[-(1:3),]





nulls

sympy_func(t(M), "nullspace")

do_la(t(M), "nullspace")

a <- nullspace(t(M), F)
cbind(a)

columnspace(M)

do.call(cbind, a)

def_sym(y, n, p, x, s, b)
x
x
symbol_is_matrix(x)
xb <- x*b

symbol_is_matrix(x)

x$pyobj$is_MatrixExpr

m <- matrix_sym(2,2)
v <- vector_sym(2)
rr <- b$pyobj$is_Matrix

load_all("caracas")
symbol_is_matrix(xb)

res <- xb$pyobj$is_Matrix

dim(xb)
sum(xb)

dim(xb)
symbol_is_matrix(xb)

load_all("caracas"); aa <- caracas:::sum_worker(xb)
aa
load_all("caracas"); bb <- sum(xb)
is_symbol(xb)

a <- sum(xb)
1
