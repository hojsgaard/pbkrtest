## #' Refit a model with new response
## #'
## #' @param object A fitted model object
## #' @param newresp A vector of new response values
## #' @param ... Additional arguments (unused)
## #' @return A refitted model
## #'
## #' @examples
## #'
## #' y <- rev(sleepstudy$Reaction)
## #'
## #' lmer1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy, REML = FALSE)
## #' lmer2 <- refit(lmer1, y)
## #'
## #' lm1 <- lm(Reaction ~ Days, sleepstudy)
## #' lm2 <- refit(lm1, y)
## #'
## #' lme1 <- lme(Reaction ~ Days, random = ~1 | Subject, sleepstudy)
## #' lme2 <- refit(lme1, y)
## #'
## #' gls1 <- gls(Reaction ~ Days, corCompSymm(form= ~1|Subject),
## #'     method="ML", data = sleepstudy)
## #'
## #' gls2 <- refit(gls1, y)
## #'
## #' fixef(lme1)
## #' fixef(lme2)
## #'
## #' coef(lm1)
## #' coef(lm2)
## #'
## #' coef(gls1)
## #' coef(gls2)

## ## #' @export
## ## refit <- function(object, newresp, ...) {
## ##   UseMethod("refit")
## ## }

## #* @importFrom lme4 refit
## #' @export
## refit.lm <- function(object, newresp, ...) {
##   mf <- model.frame(object)
##   mf[[1]] <- newresp
##   update(object, data = mf)
## }
## #' @export
## refit.lme <- function(object, newresp, ...) {
##   d <- getData(object)
##   d[[all.vars(formula(object))[1]]] <- newresp
##   update(object, data = d)
## }
## #' @export
## refit.gls <- function(object, newresp, ...) {
##   d <- getData(object)
##   d[[all.vars(formula(object))[1]]] <- newresp
##   update(object, data = d)
## }




#' @title Refit nlme model with New Response
#'
#' @description
#' Re-estimates a model using the same formula and design but with a
#' new response vector.
#'
#' The `refit` generic allows simulation-based workflows (e.g.,
#' parametric bootstrap) where new synthetic responses are drawn from
#' a fitted model and the model is re-fitted with the same design
#' structure.
#'
#' @param object A fitted model object. Supported classes include \code{lm}, \code{lme} (from \pkg{nlme}), and \code{gls} (from \pkg{nlme}).
#' @param newresp A numeric vector of new response values of the same length as the original data.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A new fitted model object of the same class as the original.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{refit.lm}}{Refits a linear model with the same formula but a new response vector.}
#'   \item{\code{refit.lme}}{Refits a linear mixed-effects model (from \pkg{nlme}) with new response data.}
#'   \item{\code{refit.gls}}{Refits a generalized least squares model (from \pkg{nlme}) with new response data.}
#' }
#'
#' @examples
#' if (require(nlme) && require(lme4)) {
#'   data(Orthodont, package = "nlme")
#'
#'   # Fit models
#'   fit_lm  <- lm(distance ~ age, data = Orthodont)
#'   fit_gls <- gls(distance ~ age, data = Orthodont)
#'   fit_lme <- lme(distance ~ age, random = ~ 1 | Subject, data = Orthodont)
#'
#'   # Simulate new response vectors
#'   set.seed(123)
#'   new_y <- rnorm(nrow(Orthodont), mean = mean(Orthodont$distance), sd = sd(Orthodont$distance))
#'
#'   # Refit models with new response
#'   refit(fit_lm, newresp = new_y)
#'   refit(fit_gls, newresp = new_y)
#'   refit(fit_lme, newresp = new_y)
#' }
#'
#' @rdname refit
#' @export
refit.lm <- function(object, newresp = NULL, ...) {
  if (is.null(newresp)) stop("newresp must be provided.")
  
  mf <- model.frame(object)
  response_name <- names(mf)[1]
  
  if (length(newresp) != nrow(mf)) {
    stop("newresp must have same length as original response.")
  }
  
  mf[[response_name]] <- newresp
  
  updated_call <- object$call
  updated_call$data <- mf
  
  eval(updated_call, parent.frame())
}

#' @importFrom lme4 refit
#' @rdname refit
#' @export
refit.lme <- function(object, newresp = NULL, ...) {
  refit_nlme_worker(object, newresp = newresp, ...)
}

#' @rdname refit
#' @export
refit.gls <- function(object, newresp = NULL, ...) {
  refit_nlme_worker(object, newresp = newresp, ...)
}

# Internal worker
refit_nlme_worker <- function(fit, newresp = NULL, ...) {
  if (!inherits(fit, c("lme", "gls"))) stop("Model must be of class 'lme' or 'gls'.")
  
  call <- getCall(fit)
  data <- getData(fit)
  
  if (!is.null(newresp)) {
    response_name <- all.vars(formula(fit))[1]
    if (length(newresp) != nrow(data))
      stop("Length of new response must match number of observations.")
    data[[response_name]] <- newresp
  }
  
  call$data <- quote(data)
  eval(call)
}


