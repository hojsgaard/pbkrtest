Mv <- function(x, entry="v") {
    if (is_sym(x) && symbol_is_matrix(x)) {
        b <- vector_sym(ncol(x), entry=entry)
        return(x %*% b)        
    }
    if (is.numeric(x)) {
        return(vector_sym(x, entry=entry))        
    }
}
## should be poly???
matrix_sym_power <- function(nrow, pow, entry="v") {
    v <- paste(entry, 1:nrow, sep="")
    w <- lapply(v, function(v_){paste0(v_, "^", pow, sep="")})
    x <- do.call(rbind, w)
    B <- matrix_(x, nrow=length(v))
    B
}

intersectionspace <- function(U, V){
    A <- cbind(U, -V)
    N <- A |> nullspace()
    S1 <- N[1:ncol(U),]
    W = U %*% S1

    aa <- colSums_(W^2) |> as_character()
    bb <- c(aa != 0)
    if (any(bb)) {
        W[, which(bb)]
    } else {
        NULL
    }
}


orthogonalcomplement <- function(L2, L1=NULL){
    NS <- nullspace(t(L2))
    if (is.null(L1))
        return(NS)
    else {
        return(intersectionspace(NS, L1))
    }
}
