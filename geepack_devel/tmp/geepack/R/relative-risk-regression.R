## This is a wrapper function for relative risk regression
## for binary data with log link using the copy method


#' @title Fit a Relative Risk Model for Binary data with Log Link
#' 
#' @description Fit a Relative Risk Model for Binary data with Log
#'     Link using the COPY method.
#' 
#' @param formula same as in \code{geese}
#' @param id same as in \code{geese}
#' @param waves same as in \code{geese}
#' @param data same as in \code{geese}
#' @param subset same as in \code{geese}
#' @param contrasts same as in \code{geese}
#' @param na.action same as in \code{geese}
#' @param corstr same as in \code{geese}
#' @param ncopy the number of copies of the original data in
#'     constructing weight.
#' @param control same as in \code{geese}
#' @param b initial values for regression coefficients as in
#'     \code{geese} but more difficult to obtain due to the log link.
#' @param alpha same as in \code{geese}
#' @return An object of class \code{"geese"} representing the fit.
#' @author Jun Yan \email{jyan.stat@@gmail.com}
#'
#' @references Lumley, T., Kornmal, R. and Ma, S. (2006). Relative
#'     risk regression in medical research: models, contrasts,
#'     estimators, and algorithms. UW Biostatistics Working Paper
#'     Series 293, University of Washington.
#' @keywords models
#' @examples
#' 
#' ## this example was used in Yu and Yan (2010, techreport)
#' data(respiratory)
#' respiratory$treat <- relevel(respiratory$treat, ref = "P")
#' respiratory$sex <- relevel(respiratory$sex, ref = "M")
#' respiratory$center <- as.factor(respiratory$center)
#' ## 1 will be the reference level
#' 
#' fit <- relRisk(outcome ~ treat + center + sex + age + baseline + visit,
#'                id = id, corstr = "ar1", data = respiratory, ncopy=10000)
#' summary(fit)
#' ## fit <- relRisk(outcome ~ treat + center + sex + age + baseline + visit,
#' ##               id = id, corstr = "ex", data = respiratory)
#' ## summary(fit)
#' ## fit <- relRisk(outcome ~ treat + center + sex + age + baseline + visit,
#' ##                id = id, corstr = "indep", data = respiratory)
#' ## summary(fit)
#' 
#' @export relRisk
relRisk<- function(formula, id, waves = NULL,
                   data = parent.frame(), subset = NULL,
                   contrasts = NULL, na.action = na.omit,
		   corstr = "indep",
                   ncopy = 1000, control = geese.control(),
                   b = NULL, alpha = NULL) {
  
  family <- binomial("log") ## fixed
  
  scall <- match.call()
  mnames <- c("", "formula", "data", "offset", "subset", "na.action", "id", "waves")
  cnames <- names(scall)
  cnames <- cnames[match(mnames,cnames,0)]
  mcall <- scall[cnames]
  if (is.null(mcall$id)) mcall$id <- as.name("id")
  mcall[[1]] <- as.name("model.frame")
  m <- eval(mcall, parent.frame())

  y <- model.extract(m, "response")
  if (is.null(dim(y))) N <- length(y) else N <- dim(y)[1]
  mterms <- attr(m, "terms")
  x <- model.matrix(mterms, m, contrasts)
  offset <- model.extract(m, "offset")
  if (is.null(offset)) offset <- rep(0, N)

  w <- rep(1 - 1 / ncopy, N)
  w.copy <- rep(1 / ncopy, N)

  y.copy <- 1 - y
  id <- model.extract(m, id)
  waves <- model.extract(m, waves)

  ## augmented data
  Y <- c(y, y.copy)
  W <- c(w, w.copy)
  X <- rbind(x, x)
  ID <- c(id, id + max(id))
  Waves <- c(waves, waves)
  Offset <- c(offset, offset)
  Freq <- rep(c(2, 1), each = N)
  
  ## get initial values
  fit0 <- glm.fit(X, Y, offset = Offset, weights = Freq, family = family)
  fit1 <- glm.fit(X, Y, offset = Offset, family = family, weights = W, start = fit0$coefficients)

  ## feed geese
  ans <- geese.fit(X, Y, ID, Offset, weights = W, waves = Waves,
            family = family, control = control, corstr = corstr,
            b = fit1$coefficients, scale.fix = TRUE)
  ans <- c(ans, list(call=scall, formula=formula)) 
  class(ans) <- "geese"
  ans

}
