
#' @export
VarCorr.gls <- function(x, sigma=1, ...){
  out <- pbkrtest::cov_par(x)
  return(out)
}

