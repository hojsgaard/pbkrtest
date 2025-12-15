#' Compute Marginal Covariance Matrix of the Response
#'
#' @description These methods compute the marginal covariance matrix
#'     \eqn{V = Var(y)} for fitted linear and linear mixed models of
#'     the form \deqn{y = X \beta + Z u + \epsilon,} where \eqn{u} and
#'     \eqn{\epsilon} are random effects and residual errors.
#'
#' The returned matrix represents the implied covariance structure of
#' the response vector, combining contributions from both random
#' effects and residuals.
#'
#' @param fit A fitted model object. Supported classes: \code{lmerMod} (from \code{lme4}), \code{lme} and \code{gls} (from \code{nlme}).
#' @param ... Additional arguments (currently ignored).
#'
#' @return A sparse matrix (class \code{"dgCMatrix"}) representing the marginal covariance matrix \eqn{V}.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{cov_matrix.lmerMod}}{For \code{lmerMod} models from the \code{lme4} package. Computes \eqn{V = Z D Z' + \sigma^2 I}.}
#'   \item{\code{cov_matrix.lme}}{For \code{lme} models from the \code{nlme} package. Computes block-diagonal covariance with group-specific structures.}
#'   \item{\code{cov_matrix.gls}}{For \code{gls} models from the \code{nlme} package. Includes optional correlation structures.}
#' }
#'
#' @examples
#' if (require(nlme) && require(Matrix)) {
#'   ## gls example
#'   fit_gls <- gls(distance ~ age, data = Orthodont)
#'   V_gls <- cov_matrix(fit_gls)
#'   print(V_gls)
#'
#'   ## lme example
#'   fit_lme <- lme(distance ~ age, random = ~ 1 | Subject, data = Orthodont)
#'   V_lme <- cov_matrix(fit_lme)
#'   print(V_lme)
#' }
#'
#' if (require(lme4) && require(Matrix)) {
#'   ## lmerMod example
#'   fit_lmer <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'   V_lmer <- cov_matrix(fit_lmer)
#'   print(V_lmer)
#' }
#'
#' @rdname cov_matrix
#' @export
cov_matrix <- function(fit, ...) {
  UseMethod("cov_matrix")
}

#' @rdname cov_matrix
#' @export
cov_matrix.lmerMod <- function(fit, ...) {
  if (!inherits(fit, "lmerMod")) stop("fit must be an 'lmerMod' object")

  Z <- lme4::getME(fit, "Z")
  Lambda <- lme4::getME(fit, "Lambda")
  Sigma_u <- Matrix::tcrossprod(Lambda)
  sigma2 <- sigma(fit)^2
  D <- sigma2 * Sigma_u
  R <- Matrix::Diagonal(n = nrow(Z), x = sigma2)
  V <- Z %*% D %*% Matrix::t(Z) + R
  as(V, "sparseMatrix")
}

#' @rdname cov_matrix
#' @export
cov_matrix.lme <- function(fit, ...) {
  if (!inherits(fit, "lme")) stop("fit must be an 'lme' object")

  data <- nlme::getData(fit)
  groupvar <- names(fit$groups)
  groups <- split(data, data[[groupvar]])
  
  vc <- nlme::VarCorr(fit)
  varcomps <- as.numeric(vc[, "Variance"])
  var_names <- rownames(vc)
  if (tail(var_names, 1) != "Residual")
      stop("Last variance component is assumed to be residual.")
  Dvars <- varcomps[-length(varcomps)]
  sigma2 <- varcomps[length(varcomps)]
  D <- Matrix::Diagonal(x = Dvars)

  if (!is.null(fit$modelStruct$corStruct)) {
    Rlist <- nlme::corMatrix(fit$modelStruct$corStruct)
  } else {
    Rlist <- lapply(groups, function(g) Matrix::Diagonal(nrow(g)))
  }

  re_formula <- as.formula(paste("~", attr(fit$modelStruct$reStruct[[1]], "formula")[2]))

  Vlist <- mapply(function(gdat, R) {
    Zi <- model.matrix(re_formula, gdat)
    Vi <- Zi %*% D %*% t(Zi) + sigma2 * R
    Vi
  }, groups, Rlist, SIMPLIFY = FALSE)

  Vfull <- Matrix::bdiag(Vlist)
  as(Vfull, "sparseMatrix")
}

#' @rdname cov_matrix
#' @export
cov_matrix.gls <- function(fit, ...) {
  sigma2 <- sigma(fit)^2

  if (is.null(fit$modelStruct$corStruct)) {
    n <- length(fitted(fit))
    return(Matrix::Diagonal(n, sigma2))
  }

  C <- nlme::corMatrix(fit$modelStruct$corStruct)

  if (is.matrix(C)) {
    V <- sigma2 * C
  } else if (is.list(C)) {
    V <- sigma2 * as.matrix(Matrix::bdiag(C))
  } else {
    stop("Unknown correlation structure in gls model.")
  }

  as(V, "sparseMatrix")
}


#' @rdname cov_matrix
#' @export
cov_matrix.lm <- function(fit, ...) {
  sigma2 <- sigma(fit)^2
  n <- length(fitted(fit))
  Matrix::Diagonal(n, sigma2)
}
