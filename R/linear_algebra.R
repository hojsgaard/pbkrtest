#' Compare column spaces
#'
#' Compare column spaces of two matrices
#'
#' @param X1,X2 matrices with the same number of rows
#'
#' @return
#'
#' * -1 : Either C(X1)=C(X2), or the spaces are not nested.
#' * 0 : C(X1) is contained in C(X2)
#' * 1 : C(X2) is contained in C(X1)
#'
#' @examples
#' 
#' A1 <- matrix(c(1,1,1,1,2,3), nrow=3)
#' A2 <- A1[, 1, drop=FALSE]
#'
#' compare_column_space(A1, A2)
#' compare_column_space(A2, A1)
#' compare_column_space(A1, A1)
#' 
#' @export 
compare_column_space <- function(X1, X2){
    if (!inherits(X1, "matrix")) stop("'X1' is not at matrix\n")
    if (!inherits(X2, "matrix")) stop("'X2' is not at matrix\n")
    if (nrow(X1) != nrow(X2)) stop("'X1' and 'X2' do not have same number of rows\n")

    ## -1 : Either C(X1)=C(X2), or the spaces are not nested.
    ## 0  : C(X1) is contained in C(X2)
    ## 1  : C(X2) is contained in C(X1)
        
    r1 <- rankMatrix_(X1)
    r2 <- rankMatrix_(X2)
    rboth  <- rankMatrix(cbind(X1, X2)) ## NOTE: Should NOT be rankMatrix_
    
    if (rboth == pmax(r1, r2)) {
        if (r2 < r1) {
            out <- 1
        } else {
            if (r2 > r1) {
                out <- 0
            } else {
                out <- -1
            }
        }
    } else {
        out <- -1
    }
    out
}



rankMatrix_ <- function(X){
    rankMatrix(X)
    ## rankMatrix(crossprod(X), method="qr.R")
}
