### model matrix

#' @importFrom stats model.matrix

#' @export
model.matrix.gls <- function(object, ...) {
    model.matrix(formula(object), data = getData(object))
}

#' @export
model.matrix.lme <- function(object, ...) {
    model.matrix(formula(object), data = getData(object))
}


### fixef

#' @export
fixef.gls <- function(object, ...) {
    coef(object, ...)
}

#' @export
fixef.lm <- function(object, ...) {
    coef(object, ...)
}


### ranef

#' @importFrom lme4 ranef
#' @importFrom nlme ranef
#' @export
ranef.gls <- function(object, ...) {
  NULL
}

#' @importFrom lme4 ranef
#' @importFrom nlme ranef
#' @export
ranef.lm <- function(object, ...) {
  NULL
}


#' Return fitted values
#' @param object A fitted model
#' @param ... Not used
#' @export
fitted0 <- function(object, ...) {
    if (!inherits(object, c("lme", "gls", "lmerMod", "lm"))) {
        stop("fitted0 currently only supports objects of class lme, gls, lm, or lmerMod")
    }

    as.numeric(model.matrix(object) %*% fixef(object))
}



#' Residuals ignoring random effects (population-level residuals)
#'
#' @description
#' Computes residuals of the form \eqn{y - X\beta}, i.e., the difference between the observed response
#' and the fixed-effects prediction. These are also called population-level or marginal residuals.
#' For models without random effects (e.g., `gls`), this is equivalent to the usual residuals.
#'
#' @param object A fitted model object. Supported classes are `lmerMod` (from \pkg{lme4}),
#' `lme` or `gls` (from \pkg{nlme}).
#' @param ... Not used.
#'
#' @return A numeric vector of residuals.
#'
#' @examples
#' if (require(nlme)) {
#'   fit_lme <- lme(distance ~ age, random = ~1 | Subject, data = Orthodont)
#'   head(residuals0(fit_lme))
#'
#'   fit_gls <- gls(distance ~ age, data = Orthodont)
#'   head(residuals0(fit_gls))
#' }
#'
#' if (require(lme4)) {
#'   fit_lmer <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
#'   head(residuals0(fit_lmer))
#' }
#'
#' @export
residuals0 <- function(object, ...) {
  UseMethod("residuals0")
}

#' @export
residuals0.lmerMod <- function(object, ...) {
  y <- model.response(model.frame(object))
  y - predict(object, re.form = NA)
}

#' @export
residuals0.lme <- function(object, ...) {
  dat <- getData(object)
  y <- model.response(model.frame(formula(object), data = dat))
  y - predict(object, level = 0)
}


#' @export
residuals0.gls <- function(object, ...) {
  residuals(object)
}

#' @export
residuals0.lm <- function(object, ...) {
  residuals(object)
}



