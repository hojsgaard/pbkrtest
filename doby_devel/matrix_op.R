## FIXME: DO WE NEED THESE TWO FUNCTIONS???


##' Matrix operations based on matching dimensions
##'
##' @param m1,m2 matrices with dimnames
##' @param op the operation to be performed 
##' @examples
##' nms1 <- letters[1:4]
##' mat1 <- matrix(1:8, nrow=4, ncol=4, dimnames=list(nms1, nms1))
##'
##' nms2 <- letters[2:3]
##' mat2 <- matrix(11:18, nrow=2, ncol=4, dimnames=list(nms2, nms1))
##'
##' matrix_op(mat1, mat2)
##' matrix_op(mat1, mat2, `*`)
##'
##' @export
matrix_op <- function(m1, m2, op=`+`) {
    
    dn1 <- dimnames(m1)
    dn2 <- dimnames(m2)

    if (is.null(dn1)) stop("m1 has no dimnames\n")
    if (is.null(dn2)) stop("m2 has no dimnames\n")
   
    if (all(dn1[[1]] %in% dn2[[1]]) && all(dn1[[2]] %in% dn2[[2]])){
        lg <- m2
        sm <- m1
        dnl <- dn2
        dns <- dn1
    } else {
        if (all(dn2[[1]] %in% dn1[[1]]) && all(dn2[[2]] %in% dn1[[2]])){
            lg <- m1
            sm <- m2
            dnl <- dn1
            dns <- dn2
            
        } else {
            stop("ss\n")
        }
    }
    
    lg[dns[[1]], dns[[2]]] <- op(lg[dns[[1]], dns[[2]]], sm)
    lg
}


##' @title Character vector to matrix
##'
##' @param x character vector
##' @param value value in matrix
##'
##' @details creates square matrix with `x` as row and column names and `val` as values
##' @examples
##'
##' d1 <- letters[1:3]
##' chr_to_matrix(d1, 3:5)
##' 
##' @export
chr_to_matrix <- function(x, value=0){
    imat <- matrix(value, nrow=length(x),
               ncol=length(x),
               dimnames=list(x, x))
    imat
}
