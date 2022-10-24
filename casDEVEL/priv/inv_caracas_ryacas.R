## Matrix inversions; various alternatives to inv()

library(caracas)

## yacas matrix inversion
inv_yac <- function(x){
    ##Ai <- as_y(Amat) %>% y_fn("Inverse") %>% yac_str()
    caracas:::stopifnot_symbol(x)
    stopifnot(caracas:::symbol_is_matrix(x))
    A_ <- as_character_matrix(x)
    Ay <- Ryacas::as_y(A_)
    Ai <- Ay %>% Ryacas::y_fn("Inverse") %>% Ryacas::yac_str()
    B <- as_sym(Ryacas::as_r(Ai))
    return(B)
}

## brute force cofactor method
inv_cf <- function(x){
    caracas:::stopifnot_symbol(x)
    stopifnot(caracas:::symbol_is_matrix(x))
    return(t(sympy_func(x, "cofactor_matrix")) / det(x))
}

## lu decomposition
inv_lu <- function(x){
    caracas:::stopifnot_symbol(x)
    stopifnot(caracas:::symbol_is_matrix(x))
    caracas:::construct_symbol_from_pyobj(x$pyobj$inv(method="LU"))
}


## Library hard coded inverses 1x1 ... pxp

## p    <- 5
## A    <- lapply(1:p, function(n) matrix_sym(n, n))
## Ainv <- lapply(A, function(Aj) inv_yac(Aj))
## A_list    <- lapply(A, as.character)
## Ainv_list <- lapply(Ainv, as.character) ## as.character is SLOW
## save(A_list, Ainv_list, file="inv_library.RData")


load("inv_library.RData")
A <- lapply(A_list, as_sym)
Ainv <- lapply(Ainv_list, as_sym)

inv_lib <- function(x){
    
    if (nrow(x) != ncol(x)){
        stop("Not a square matrix\n")
    }
    d <- nrow(x)
    if (d > length(A_list)){
        return(inv_yac(x))
    }
    z <- as_character_matrix(x)

    ## As  <- A_list[[d]]
    ## Ais <- Ainv_list[[d]]
    
    ## As <- gsub("\\^", "**", As)
    ## As <- as_sym(As)
    
    ## Ais <- gsub("\\^", "**", Ais)
    ## Ais <- as_sym(Ais)

    As <- A[[d]]
    Ais <- Ainv[[d]]
    
    out <- subs(Ais, As, z) 
    return(out)
}

## m <- as_sym(toeplitz(letters[1:3]))
## mi <- inv_lib(m)
## (m %*% mi) %>% simplify()





