library(caracas)


N <- symbol("N")
r <- symbol("r")
i <- symbol("i")
j <- symbol("j")
R <- sympy_declare("R", "FunctionMatrix(N, N, Lambda((i, j), (0**Abs(i-j)) + r*(1-0**Abs(i-j))))")
R$pyobj
as_explicit(R)


x <- sympy_declare("x", "FunctionMatrix(N, 1, Lambda((i, j), 1))")
x




#' Make symbol explicit
#'
#' @param x A `caracas_symbol`
#' 
#' @examples 
#' if (has_sympy()) {
#'    N <- symbol("N")
#'    r <- symbol("r")
#'    i <- symbol("i")
#'    j <- symbol("j")
#'    R <- sympy_declare("R", "FunctionMatrix(6, 6, Lambda((i, j), (0**Abs(i-j)) + r*(1-0**Abs(i-j))))")
#'    R
#'    as_explicit(R)
#' }
#'
#' @concept caracas_symbol
#'
#' @export
as_explicit <- function(x) {
  stopifnot_symbol(x)
  
  ensure_sympy()
  
  if (!is.null(x$pyobj) && !is.null(x$pyobj$as_explicit)) {
    y <- construct_symbol_from_pyobj(x$pyobj$as_explicit())
    return(y)
  }
  
  stop("Could not as_explicit()")
}


