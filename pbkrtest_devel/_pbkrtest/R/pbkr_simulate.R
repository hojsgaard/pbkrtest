#' @title Simulate Response Vectors from Fitted nlme Models
#'
#' @description These methods generate simulated response vectors from
#'     the multivariate normal distribution implied by a fitted
#'     \code{gls} or \code{lme} model. The simulation accounts for the
#'     fitted mean and covariance structure of the model.
#'
#' @param object A fitted \code{gls} or \code{lme} object (from the \code{nlme} package).
#' @param nsim Number of simulated datasets to generate (default is 1).
#' @param seed Optional random seed for reproducibility.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A data frame with \code{nsim} simulated response vectors as columns. Each column represents one simulated replicate of the response under the model's implied distribution.
#'
#' @section Details:
#' For \code{gls} objects, the mean vector is the fitted values and the covariance matrix is obtained via \code{cov_matrix()}.  
#' For \code{lme} objects, the mean vector is the level-0 (population) predicted values and the covariance matrix is also obtained via \code{cov_matrix()}.
#'
#' @examples
#' if (require(nlme)) {
#'   ## Example for gls
#'   fit_gls <- gls(distance ~ age, data = Orthodont)
#'   sims_gls <- simulate(fit_gls, nsim = 5, seed = 123)
#'   head(sims_gls)
#'
#'   ## Example for lme
#'   fit_lme <- lme(distance ~ age, random = ~ 1 | Subject, data = Orthodont)
#'   sims_lme <- simulate(fit_lme, nsim = 5, seed = 123)
#'   head(sims_lme)
#' }
#'
#' @rdname simulate-nlme
#' @export
simulate.gls <- function(object, nsim = 1, seed = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)
  
  mu <- fitted(object)
  Sigma <- as.matrix(cov_matrix(object))
  
  out <- MASS::mvrnorm(nsim, mu = mu, Sigma = Sigma, empirical = FALSE)
  out <- t(out)
  out <- as.data.frame(out)
  
  return(out)
}

#' @rdname simulate-nlme
#' @export
simulate.lme <- function(object, nsim = 1, seed = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)
  
  mu <- predict(object, level = 0)
  Sigma <- as.matrix(cov_matrix(object))
  
  out <- MASS::mvrnorm(nsim, mu = mu, Sigma = Sigma, empirical = FALSE)
  out <- t(out)
  rownames(out) <- NULL
  out <- as.data.frame(out)
  
  return(out)
}












## #' @rdname simulate-nlme
## #' @export
## simulate.gls <- function(object, nsim = 1, seed = NULL, ...) {
##   mu <- fitted(object)
##   Sigma <- as.matrix(cov_matrix(object))
##   out <- MASS::mvrnorm(nsim, mu = mu, Sigma = Sigma, empirical = FALSE)
##   out <- t(out)
##   out <- as.data.frame(out)
##   return(out)
## }

