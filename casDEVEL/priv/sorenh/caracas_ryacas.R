library(caracas)

Amat <- toeplitz(letters[1:4])
Amat
Ac <- as_sym(Amat)
Ac

## Matrix inversions; various alternatives to inv()

inv_yac <- function(x){
    ##Ai <- as_y(Amat) %>% y_fn("Inverse") %>% yac_str()
    caracas:::stopifnot_symbol(x)
    stopifnot(caracas:::symbol_is_matrix(x))
    A_ <- as_character_matrix(x)
    Ay <- as_y(A_)
    Ai <- Ay %>% y_fn("Inverse") %>% yac_str()
    B <- as_sym(as_r(Ai))
    return(B)
}

inv_cf <- function(x){
    caracas:::stopifnot_symbol(x)
    stopifnot(caracas:::symbol_is_matrix(x))
    return(t(sympy_func(x, "cofactor_matrix")) / det(x))
}

inv_lu <- function(x){
    caracas:::stopifnot_symbol(x)
    stopifnot(caracas:::symbol_is_matrix(x))
    caracas:::construct_symbol_from_pyobj(x$pyobj$inv(method="LU"))
}


Ai1 <- inv(Ac)
Ai2 <- inv_cf(Ac)
Ai3 <- inv_lu(Ac)
Ai4 <- inv_yac(Ac)

(Ai1 - Ai2)
(Ai1 - Ai3) %>% simplify()
(Ai1 - Ai4) %>% simplify()

Ac %*% Ai1


## Hard coding of inverse of 1x1, 2x2, 3x3 matrices:

## Library; to be computed once
## How to store these in package; perhaps just store $pyobj
A1  <- matrix_sym(1,1)
A2  <- matrix_sym(2,2)
A3  <- matrix_sym(3,3)
A4  <- matrix_sym(4,4)
A5  <- matrix_sym(5,5)
A6  <- matrix_sym(6,6)
A1i <- inv_yac(A1)
A2i <- inv_yac(A2)
A3i <- inv_yac(A3)
A4i <- inv_yac(A4)
A5i <- inv_yac(A5)
A6i <- inv_yac(A6)
A   <- list(A1,  A2,  A3,  A4,  A5,  A6)
Ai  <- list(A1i, A2i, A3i, A4i, A5i, A6i)

inv_lib <- function(x){
    ## ensure_sympy()
    if (nrow(x) != ncol(x)){
        stop("Not a square matrix\n")
    }
    d <- nrow(x)
    z <- as_character_matrix(x)
    out <- subs(Ai[[d]], A[[d]], z) ## FIXME: A fast version of subs is needed...
    return(out)
}

## 2x2
B2 <- as_sym(toeplitz(letters[1:2]))
d <- inv_lib(B2) - inv_yac(B2)
d %>% simplify

## 3x3
B3 <- as_sym(toeplitz(letters[1:3]))
d <- inv_lib(B3) - inv_yac(B3)
d %>% simplify

## 4x4
B4 <- as_sym(toeplitz(letters[1:4]))
d <- inv_lib(B4) - inv_yac(B4)
d %>% simplify

## 5x5
B5 <- as_sym(toeplitz(letters[1:5]))
d <- inv_lib(B5) - inv_yac(B5)
d %>% simplify

## 6x6
B6 <- as_sym(toeplitz(letters[1:6]))
## d <- inv_lib(B6) - inv_yac(B6)
## d %>% simplify

## Benchmark
library(microbenchmark)
mb <- microbenchmark(
    inv_lib(B2), inv_yac(B2), inv_cf(B2), inv_lu(B2), inv(B2),
    inv_lib(B3), inv_yac(B3), inv_cf(B3), inv_lu(B3), inv(B3),
    inv_lib(B4), inv_yac(B4), inv_cf(B4), inv_lu(B4), inv(B4),
    ## inv_lib(B5), inv_yac(B5), inv_cf(B5), inv_lu(B5),
    ## inv_lib(B6), inv_yac(B6), 
    times=3
)






