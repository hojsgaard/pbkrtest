
#' @title Simulate Response Vectors from a Fitted Mixed Model
#'
#' @description Simulates response vectors from the marginal
#'     distribution implied by a fitted model.  For mixed models, this
#'     means integrating over random effects (i.e. predicting at level
#'     0).
#'
#' @param object A fitted model object. Supports objects of class \code{lme}
#'   (from \pkg{nlme}), \code{gls} (from \pkg{nlme}), or \code{lmerMod} (from \pkg{lme4}).
#' @param nsim Number of simulated response vectors to generate. Default is 1.
#' @param seed Optional seed for reproducibility.
#' @param ... Currently ignored.
#'
#' @return A data frame with \code{length(mu)} rows and \code{nsim} columns. Each column is one simulated response vector.
#'
#' @details
#' For \code{gls} models, the mean is the fitted values.  
#' For \code{lme} models, the mean is the level-0 prediction (no random effects).  
#' For \code{lmerMod} models, the mean is the marginal mean with random effects set to zero (re.form=NA).
#'
#' The covariance matrix is obtained via \code{cov_matrix()}, representing the implied marginal covariance structure.
#'
#' @examples
#' if (require(nlme) && require(lme4) && require(MASS)) {
#'   data(Orthodont, package = "nlme")
#'   NSIM <- 6
#'   # GLS model
#'   fit_gls <- gls(distance ~ age, data = Orthodont)
#'   sim_gls <- simulate0(fit_gls, nsim = NSIM, seed = 123)
#'   head(sim_gls)
#'
#'   # LME model
#'   fit_lme <- lme(distance ~ age, random = ~1 | Subject, data = Orthodont)
#'   sim_lme <- simulate0(fit_lme, nsim = NSIM, seed = 123)
#'   head(sim_lme)
#'
#'   # lmerMod model
#'   fit_lmer <- lme4::lmer(distance ~ age + (1 | Subject), data = Orthodont)
#'   sim_lmer <- simulate0(fit_lmer, nsim = NSIM, seed = 123)
#'   head(sim_lmer)
#'
#'   # lm
#'   fit_lm <- lm(distance ~ age, data = Orthodont)
#'   sim_lm <- simulate0(fit_lm, nsim = NSIM, seed = 123)
#'   head(sim_lm)  
#' }
#'
#' @export
simulate0 <- function(object, nsim = 1, seed = NULL, ...) {
  
  if (!inherits(object, c("lme", "gls", "lmerMod", "lm"))) {
    stop("simulate0 currently only supports objects of class lme, gls, lm, or lmerMod")
  }
  

  if (!is.null(seed)) set.seed(seed)
  
  ## Explicit mean vector extraction
  mu <- if (inherits(object, "gls")) {
    fitted(object)
  } else if (inherits(object, "lm")) {
    predict(object)
  } else if (inherits(object, "lme")) {
    predict(object, level = 0)
  } else if (inherits(object, "lmerMod")) {
    predict(object, re.form = NA)
  } else {
    stop("Unsupported model class")
  }
  
  ## Covariance
  Sigma <- as.matrix(cov_matrix(object))
  
  ## Simulate
  out <- MASS::mvrnorm(nsim, mu = mu, Sigma = Sigma, empirical = FALSE)
  out <- t(out)
  rownames(out) <- NULL
  as.data.frame(out)
}



































## ## NOTE to self: There is a simulate method in nlme, but it does
## ## something different than simulate in lme4. Hence this code here.

## #' @title Simulate Response Vectors from Fitted nlme Models
## #'
## #' @description These methods generate simulated response vectors from
## #'     the multivariate normal distribution implied by a fitted
## #'     \code{gls} or \code{lme} model. The simulation accounts for the
## #'     fitted mean and covariance structure of the model.
## #'
## #' @param object A fitted \code{gls} or \code{lme} object (from the \code{nlme} package).
## #' @param nsim Number of simulated datasets to generate (default is 1).
## #' @param seed Optional random seed for reproducibility.
## #' @param ... Additional arguments (currently ignored).
## #'
## #' @return A data frame with \code{nsim} simulated response vectors as
## #'     columns. Each column represents one simulated replicate of the
## #'     response under the model's implied distribution.
## #'
## #' @section Details: For \code{gls} objects, the mean vector is the
## #'     fitted values and the covariance matrix is obtained via
## #'     \code{cov_matrix()}.  For \code{lme} objects, the mean vector
## #'     is the level-0 (population) predicted values and the covariance
## #'     matrix is also obtained via \code{cov_matrix()}.
## #'
## #' @examples
## #' if (require(nlme)) {
## #'   NSIM <- 5
## #'   ## Example for gls
## #'   fit_gls <- gls(distance ~ age, data = Orthodont)
## #'   sims_gls2 <- simulate2(fit_gls, nsim = NSIM, seed = 123)
## #'   head(sims_gls2)
## #'
## #'   ## Example for lme
## #'   fit_lme <- lme(distance ~ age, random = ~ 1 | Subject, data = Orthodont)
## #'   sims_lme <- simulate2(fit_lme, nsim = NSIM, seed = 123)
## #'   head(sims_lme)
## #'
## #'   fit_lmer <- lmer(distance ~ age + (1|Subject), data = Orthodont)
## #'   sims_lmer <- simulate(fit_lmer, nsim = NSIM, seed = 123, re.form=NA)
## #'   head(sims_lmer) 
## #' }
## #'
## #'
## #' @rdname simulate-nlme
## #' @export
## simulate2 <- function(fit, ...) {
##   UseMethod("simulate2")
## }

## #' @rdname simulate-nlme
## #' @export
## simulate2.gls <- function(object, nsim = 1, seed = NULL, ...) {
##     mu <- fitted(object)

##     if (!is.null(seed)) set.seed(seed)
  
##     Sigma <- as.matrix(cov_matrix(object))
##     out <- MASS::mvrnorm(nsim, mu = mu, Sigma = Sigma, empirical = FALSE)
##     out <- t(out)
##     out <- as.data.frame(out)
    
##     return(out)
## }

## #' @rdname simulate-nlme
## #' @export
## simulate2.lme <- function(object, nsim = 1, seed = NULL, ...) {
##     mu <- predict(object, level = 0)

##     if (!is.null(seed)) set.seed(seed)
  
##     Sigma <- as.matrix(cov_matrix(object))
    
##     out <- MASS::mvrnorm(nsim, mu = mu, Sigma = Sigma, empirical = FALSE)
##     out <- t(out)
##     rownames(out) <- NULL
##     out <- as.data.frame(out)
    
##     return(out)
## }






