make_ar1 <- function(r, size) {
    if (length(size)==1 && is.numeric(size)){
        toeplitz_(paste0(r, "^", (0:(size-1))))
    } else {
        z <- outer(size, size, function(x,y) abs(x-y))
        r <- paste0(r, "^",z)
        dim(r) <- dim(z)
        as_sym(r)
    }
}

block_ar1 <- function(formula., data., parm="r") {
    pgf <- parseGroupFormula(formula.)
    print(formula.)
    print(pgf)
#    pgf <<- pgf
    print("JJJJJJJJJJJJJJ")   
    
    print(pgf$groupFormula)
    
    dat_lst <- data. |> split_by(pgf$groupFormula)
    print("JJJJJJJJJJJJJJ")   
    
        mm <- pgf$model
    
    if (identical(deparse(mm[[2]]), "1")){
        out <- dat_lst |>   
            lapply(function(x) {
                make_ar1("r", nrow(x))
            })
    } else {
        out <- dat_lst |>   
            lapply(function(x) {
                tvar <- model.matrix(update(mm, ~. - 1), data=x)
                tvar <- as.numeric(tvar)
                make_ar1("r", as.numeric(tvar))
            })
    }
    out <- bdiag_(out)
    return(out)
}

block_ri <- function(formula., data., parm="omega") {
    formula. <- update(formula., ~. -1)
    Z <- model.matrix(formula., data=data.)
    G <- diag_(paste0(parm,"^2"), ncol(Z))
    Z <- as_sym(Z)
    ZGZt <- Z %*% G %*% t(Z)
    out <- list(G=G, Z=Z, ZGZt=ZGZt)
    return(out)
}

