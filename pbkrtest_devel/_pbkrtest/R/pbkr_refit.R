#' Refit a model with new response
#'
#' @param object A fitted model object
#' @param newresp A vector of new response values
#' @param ... Additional arguments (unused)
#' @return A refitted model
#'
#' @examples
#'
#' y <- rev(sleepstudy$Reaction)
#'
#' lmer1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy, REML = FALSE)
#' lmer2 <- refit(lmer1, y)
#'
#' lm1 <- lm(Reaction ~ Days, sleepstudy)
#' lm2 <- refit(lm1, y)
#'
#' lme1 <- lme(Reaction ~ Days, random = ~1 | Subject, sleepstudy)
#' lme2 <- refit(lme1, y)
#'
#' gls1 <- gls(Reaction ~ Days, corCompSymm(form= ~1|Subject),
#'     method="ML", data = sleepstudy)
#'
#' gls2 <- refit(gls1, y)
#'
#' fixef(lme1)
#' fixef(lme2)
#'
#' coef(lm1)
#' coef(lm2)
#'
#' coef(gls1)
#' coef(gls2)

## #' @export
## refit <- function(object, newresp, ...) {
##   UseMethod("refit")
## }

#* @importFrom lme4 refit
#' @export
refit.lm <- function(object, newresp, ...) {
  mf <- model.frame(object)
  mf[[1]] <- newresp
  update(object, data = mf)
}
#' @export
refit.lme <- function(object, newresp, ...) {
  d <- getData(object)
  d[[all.vars(formula(object))[1]]] <- newresp
  update(object, data = d)
}
#' @export
refit.gls <- function(object, newresp, ...) {
  d <- getData(object)
  d[[all.vars(formula(object))[1]]] <- newresp
  update(object, data = d)
}
