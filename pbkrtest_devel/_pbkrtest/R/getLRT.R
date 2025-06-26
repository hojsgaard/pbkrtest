#' @title Likelihood Ratio Test Between Nested Models
#'
#' @description
#' Performs a likelihood ratio test (LRT) between two nested models. Supports
#' models of class `lm`, `lmerMod`, `glmerMod`, `lme`, and `gls`.
#'
#' @param fit1 A model object representing the more complex (full) model.
#' @param fit0 A model object representing the simpler (nested) model.
#'
#' @return A named numeric vector with:
#' \describe{
#'   \item{tobs}{Test statistic (twice the difference in log-likelihoods).}
#'   \item{df}{Degrees of freedom (difference in number of parameters).}
#'   \item{p.value}{P-value from the chi-squared distribution.}
#' }
#'
#' @examples
#' ## lm
#' fit1 <- lm(mpg ~ wt + hp, data = mtcars)
#' fit0 <- lm(mpg ~ wt, data = mtcars)
#' getLRT(fit1, fit0)
#'
#' ## lmerMod
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   library(lme4)
#'   fit1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy, REML = FALSE)
#'   fit0 <- lmer(Reaction ~ 1 + (Days | Subject), sleepstudy, REML = FALSE)
#'   getLRT(fit1, fit0)
#' }
#'
#' ## glmerMod
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   library(lme4)
#'   data(cbpp)
#'   fit1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
#'                 data = cbpp, family = binomial)
#'   fit0 <- glmer(cbind(incidence, size - incidence) ~ 1 + (1 | herd),
#'                 data = cbpp, family = binomial)
#'   getLRT(fit1, fit0)
#' }
#'
#' ## lme
#' if (requireNamespace("nlme", quietly = TRUE)) {
#'   library(nlme)
#'   fit1 <- lme(distance ~ age + Sex, random = ~1 | Subject,
#'               data = Orthodont, method = "ML")
#'   fit0 <- lme(distance ~ age, random = ~1 | Subject,
#'               data = Orthodont, method = "ML")
#'   getLRT(fit1, fit0)
#' }
#'
#' ## gls
#' if (requireNamespace("nlme", quietly = TRUE)) {
#'   library(nlme)
#'   fit1 <- gls(mpg ~ wt + hp, data = mtcars, method = "ML")
#'   fit0 <- gls(mpg ~ wt, data = mtcars, method = "ML")
#'   getLRT(fit1, fit0)
#' }
#'
#' @export
getLRT <- function(fit1, fit0) {
  UseMethod("getLRT")
}

#' @export
getLRT.lm <- function(fit1, fit0) {
  logL1 <- logLik(fit1)
  logL0 <- logLik(fit0)
  tobs  <- 2 * (logL1 - logL0)
  df    <- attr(logL1, "df") - attr(logL0, "df")
  pval  <- 1 - pchisq(tobs, df)
  c(tobs = tobs, df = df, p.value = pval)
}

#' @export
getLRT.lmerMod <- function(fit1, fit0) {
  logL1 <- logLik(update(fit1, REML = FALSE))
  logL0 <- logLik(update(fit0, REML = FALSE))
  tobs  <- 2 * (logL1 - logL0)
  df    <- attr(logL1, "df") - attr(logL0, "df")
  pval  <- 1 - pchisq(tobs, df)
  c(tobs = tobs, df = df, p.value = pval)
}

#' @export
getLRT.glmerMod <- function(fit1, fit0) {
  logL1 <- logLik(update(fit1))
  logL0 <- logLik(update(fit0))
  tobs  <- 2 * (logL1 - logL0)
  df    <- attr(logL1, "df") - attr(logL0, "df")
  pval  <- 1 - pchisq(tobs, df)
  c(tobs = tobs, df = df, p.value = pval)
}

#' @export
getLRT.lme <- function(fit1, fit0) {
  logL1 <- logLik(update(fit1, method = "ML"))
  logL0 <- logLik(update(fit0, method = "ML"))
  tobs  <- 2 * (logL1 - logL0)
  df    <- attr(logL1, "df") - attr(logL0, "df")
  pval  <- 1 - pchisq(tobs, df)
  c(tobs = tobs, df = df, p.value = pval)
}

#' @export
getLRT.gls <- function(fit1, fit0) {
  logL1 <- logLik(update(fit1, method = "ML"))
  logL0 <- logLik(update(fit0, method = "ML"))
  tobs  <- 2 * (logL1 - logL0)
  df    <- attr(logL1, "df") - attr(logL0, "df")
  pval  <- 1 - pchisq(tobs, df)
  c(tobs = tobs, df = df, p.value = pval)
}







## #' @export
## getLRT <- function(largeModel, smallModel){
##   UseMethod("getLRT")
## }

## #' @export
## getLRT.lmerMod <-
##         function(largeModel, smallModel){
##     logL_small <- logLik(update(smallModel, REML=FALSE))
##     logL_large <- logLik(update(largeModel, REML=FALSE))
##     tobs     <- 2 * (logL_large - logL_small)
##     df11     <- attr(logL_large, "df") - attr(logL_small, "df")
##     p.X2     <- 1 - pchisq(tobs, df11)
##     c(tobs=tobs, df=df11, p.value=p.X2)
## }

## #' @export
## getLRT.glmerMod <-
##         function(largeModel, smallModel){
##     logL_small <- logLik(update(smallModel))
##     logL_large <- logLik(update(largeModel))
##     tobs     <- 2 * (logL_large - logL_small)
##     df11     <- attr(logL_large, "df") - attr(logL_small, "df")
##     p.X2     <- 1 - pchisq(tobs, df11)
##     c(tobs=tobs, df=df11, p.value=p.X2)
## }

## #' @export
## getLRT.lm <- function(largeModel, smallModel){
##   logL_small <- logLik(smallModel)
##   logL_large <- logLik(largeModel)
##   tobs     <- 2 * (logL_large - logL_small)
##   df11     <- attr(logL_large, "df") - attr(logL_small, "df")
##   p.X2     <- 1 - pchisq(tobs, df11)
##   c(tobs=tobs, df=df11, p.value=p.X2)
## }
