
#' Create matrix symbol
#' 
#' @param x Name of matrix symbol
#' @param nrow Number of rows
#' @param ncol Number of columns
#' 
#' @concept caracas_symbol
#' 
#' @export
matrix_symbol <- function(x, nrow = 1L, ncol = 1L) {
  ensure_sympy()
  verify_variable_name(x)
  
  cmd <- paste0(x, " = MatrixSymbol('", x, "', ", nrow, ", ", ncol, ")")
  s <- reticulate::py_run_string(cmd, convert = FALSE)
  res <- s[[x]]
  y <- construct_symbol_from_pyobj(res)
  
  return(y)
}



#' Test
#' 
#' A  B
#' C  D
#' 
#' @return Columns of result
#' 
#' @export
blockmat_inv <- function(A, B, C, D) {
  ensure_sympy()
  
  minv <- function(x) {
    x$inverse()
  }
  
  s <- get_sympy()
  
  mult <- function(x, y) {
    s$MatMul(x, y)
  }
  
  add <- function(x, y) {
    s$MatAddl(x, y)
  }
  
  neg <- function(x) {
    s$matrix_multiply_elementwise(x, 1)
  }
  
  A <- A$pyobj
  B <- B$pyobj
  C <- C$pyobj
  D <- D$pyobj
  
  Ainv <- A$inverse()
  
  DCAiBinv <- inv(D - C %*% Ainv %*% B)
  
  e11 <- Ainv + Ainv %*% B %*% DCAiBinv %*% C %*% Ainv
  e12 <- - Ainv %*% B %*% DCAiBinv
  e21 <- - DCAiBinv %*% C %*% Ainv
  e22 <- DCAiBinv
  
  # Columns
  return(list(
    list(e11, e21),
    list(e12, e22))
  )
}

#' @export
sympy_declare <- function(x, val) {
  caracas:::ensure_sympy()
  
  cmd <- paste0(x, " = ", val)
  s <- reticulate::py_run_string(cmd, convert = FALSE)
  res <- s[[x]]
  y <- caracas:::construct_symbol_from_pyobj(res)
  
  return(y)
}






























devtools::load_all("~/gits/r-cas/caracas/")

W <- matrix_symbol("W")
J <- matrix_symbol("J")
O <- sympy_declare("O", "ZeroMatrix(1, 1)")


# L =  J   0
#     -W   J
Linv <- blockmat_inv(J, O, -W, J)
Linv


L <- matrix(c("J", "-W", "0", "J"), nrow = 2) %>% as_sym(declare_symbols = FALSE)
L

v <- symbol("v")
Ve <- matrix(c("J", "0", "0", "v^2*J"), nrow = 2) %>% as_sym(declare_symbols = FALSE)

Ve
Linv %*% Ve %*% t(Linv)


Ve <- matrix(c("I", "0", "0", "v^2*I"), nrow = 2) %>% as_sym()
L <- matrix(c("I", "-W", "0", "I"), nrow = 2) %>% as_sym()
Linv <- inv(L)
V <- Linv %*% Ve %*% t(Linv)





```{r}
library(caracas)

sympy_declare <- function(x, val) {
  caracas:::ensure_sympy()
  
  cmd <- paste0(x, " = ", val)
  s <- reticulate::py_run_string(cmd, convert = FALSE)
  res <- s[[x]]
  y <- caracas:::construct_symbol_from_pyobj(res)
  
  return(y)
}

W <- sympy_declare("W", "MatrixSymbol('W', 1, 1)")
J <- sympy_declare("J", "MatrixSymbol('J', 1, 1)")
L <- sympy_declare("L", "BlockMatrix([[J, ZeroMatrix(1, 1)], [-W, J]])")
L
as.character(L)
v <- symbol("v")
Ve <- sympy_declare("J", "BlockMatrix([[J, ZeroMatrix(1, 1)], [ZeroMatrix(1, 1), J*v**2]])")
Ve
as.character(Ve)

Linv <- caracas:::construct_symbol_from_pyobj(L$pyobj$inverse())
as.character(Linv)

V <- Linv %*% Ve %*% t(Linv)
V


sympy <- get_sympy()
A <- sympy$MatMul(Linv$pyobj, Ve$pyobj)
A <- sympy$MatMul(A, Linv$pyobj$transpose())
sympy$block_collapse(A)   


Ve <- matrix(c("J", "0", "0", "v^2*J"), nrow = 2) %>% as_sym(declare_symbols = FALSE)
Ve
L <- matrix(c("J", "-W", "0", "J"), nrow = 2) %>% as_sym(declare_symbols = FALSE)
L

Linv <- inv(L)
Linv

sympy <- get_sympy()
Linv <- caracas:::construct_symbol_from_pyobj(sympy$Inverse(L$pyobj))
Linv

V <- Linv %*% Ve %*% t(Linv)
V
```


```{r}
library(caracas)

W <- symbol("W")
J <- symbol("J")  
v <- symbol("v")
Ve <- matrix(c("J", "0", "0", "v^2*J"), nrow = 2) %>% as_sym(declare_symbols = FALSE)
Ve
L <- matrix(c("J", "-W", "0", "J"), nrow = 2) %>% as_sym(declare_symbols = FALSE)
L

Linv <- inv(L)
Linv
Linv <- matrix(c("J", "W", "0", "J"), nrow = 2) %>% as_sym(declare_symbols = FALSE)
Linv

V <- Linv %*% Ve %*% t(Linv)
V
subs(V, "J", "1")

M <- as_sym(matrix(c("A", "0", "C", "D"), nrow=2))
M[2, 1]
M[1, 1]
t(M[1, 1])

library(caracas)
W <- matrix_symbol("W")
W$pyobj$transpose()

a <- symbol("a") 
a$pyobj$transpose()
```

