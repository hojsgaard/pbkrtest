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


#' @title Extract (or "get") components from a \code{KRmodcomp} or
#'     \code{SATmodcomp} object.
#'
#' @description Extract (or "get") components from a \code{KRmodcomp}
#'     or \code{SATmodcomp} object. In particular, get denominator
#'     degrees of freedom.
#' 
#' @name get_modcomp
#' 
#' @param object A \code{KRmodcomp} object, which is the result of the
#'     \code{KRmodcomp} function
#' @param name The available slots. If \code{name} is missing or \code{NULL}
#'     then everything is returned.
#' @author Søren Højsgaard \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{KRmodcomp}}, \code{\link{PBmodcomp}},
#'     \code{\link{vcovAdj}}
#' @references Ulrich Halekoh, Søren Højsgaard (2014)., A Kenward-Roger
#'     Approximation and Parametric Bootstrap Methods for Tests in Linear Mixed
#'     Models - The R Package pbkrtest., Journal of Statistical Software,
#'     58(10), 1-30., \url{https://www.jstatsoft.org/v59/i09/}
#' @keywords utilities
#' @examples
#'
#' (fm0 <- lmer(Reaction ~ (Days|Subject), sleepstudy))
#' (fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
#' (fm2 <- lmer(Reaction ~ Days + I(Days^2) + (Days|Subject), sleepstudy))
#'
#' x10 <- KRmodcomp(fm1, fm0)
#' getKR(x10, "ddf")
#'
#' KRmodcomp(fm1, fm0) |> getKR("ddf")
#' KRmodcomp(fm2, fm0) |> getKR("ddf")
#' KRmodcomp(fm2, fm1) |> getKR("ddf")
#'
#' ## For comparison:
#' 
#' SATmodcomp(fm1, fm0) |> getSAT("ddf")
#' SATmodcomp(fm2, fm0) |> getSAT("ddf")
#' SATmodcomp(fm2, fm1) |> getSAT("ddf")
#'
#' @export
#' @rdname get_modcomp
getKR <- function (object, name = c("ndf", "ddf", "Fstat", "p.value", "F.scaling", "FstatU", "p.valueU", "aux")) 
{	
  stopifnot(is(object, "KRmodcomp"))
  if (missing(name) || is.null(name)){
    return(object$stats)
  } else {
    stopifnot(length(name <- as.character(name)) == 1)
    name <- match.arg(name)
    object$stats[[name]]
  }
}


#' @export
#' @rdname get_modcomp
getSAT <- function (object, name = c("ndf", "ddf", "Fstat", "p.value")) 
{	
  stopifnot(is(object, "SATmodcomp"))
  if (missing(name) || is.null(name)){
    return(object$test) ## FIXME Should be stats
  } else {
    stopifnot(length(name <- as.character(name)) == 1)
    name <- match.arg(name)
    object$test[[name]] ## FIXME Should be stats
  }
}


## '
## ' \dontrun{
## ' p <- PBrefdist(fm1, fm0, seed=123, nsim=50, cl=1)
## ' e <- mean(p)
## ' e
## ' -2*e/(1-e)
## ' 
## ' x <- PBmodcomp(fm1, fm0, nsim=50, seed=123, cl=1)
## ' summary(x)$test$ddf[5]
## '
## ' x <- PBmodcomp(fm2, fm0, nsim=50, cl=2)
## ' summary(x)$test$ddf[5]
## '
## ' x <- PBmodcomp(fm2, fm1, nsim=50, cl=2)
## ' summary(x)$test$ddf[5]
## ' }
## ' 



#' @title Adjusted denominator degrees of freedom for linear estimate for linear
#'     mixed model.
#' 
#' @description Get adjusted denominator degrees freedom for testing Lb=0 in a
#'     linear mixed model where L is a restriction matrix.
#'
#' @name get_ddf_Lb
#' 
#' @aliases get_Lb_ddf get_Lb_ddf.lmerMod Lb_ddf
#' 
#' @param object A linear mixed model object.
#' @param L A vector with the same length as \code{fixef(object)} or a matrix
#'     with the same number of columns as the length of \code{fixef(object)}
#' @param V0,Vadj The unadjusted and the adjusted covariance matrices for the fixed
#'     effects parameters. The unadjusted covariance matrix is obtained with
#'     \code{vcov()} and adjusted with \code{vcovAdj()}.
#' @return Adjusted degrees of freedom (adjustment made by a Kenward-Roger
#'     approximation).
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{KRmodcomp}}, \code{\link{vcovAdj}},
#'     \code{\link{model2restriction_matrix}},
#'     \code{\link{restriction_matrix2model}}
#' @references Ulrich Halekoh, Søren Højsgaard (2014)., A Kenward-Roger
#'     Approximation and Parametric Bootstrap Methods for Tests in Linear Mixed
#'     Models - The R Package pbkrtest., Journal of Statistical Software,
#'     58(10), 1-30., \url{https://www.jstatsoft.org/v59/i09/}
#'
#' @keywords inference models
#' @examples
#' 
#' (fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
#' (fm0 <- lmer(Reaction ~ 1 + (Days|Subject), sleepstudy))
#' anova(fm1, fm0)
#' 
#' KRmodcomp(fm1, fm0)  ## 17 denominator df's
#' get_Lb_ddf(fm1, c(0, 1)) ## 17 denominator df's
#' 
#' # Notice: The restriction matrix L corresponding to the test above
#' # can be found with
#' L <- model2restriction_matrix(fm1, fm0)
#' L
#' 




#' @export
#' @rdname get_ddf_Lb
get_Lb_ddf <- function(object, L){
    UseMethod("get_Lb_ddf")
}

#' @export
#' @rdname get_ddf_Lb
get_Lb_ddf.lmerMod <- function(object, L){
    Lb_ddf(L, vcov(object), vcovAdj(object))
}


#' @export
#' @rdname get_ddf_Lb
Lb_ddf <- function(L, V0, Vadj) {
    if (!is.matrix(L))
        L = matrix(L, nrow = 1)
    Theta <- t(L) %*% solve(L %*% V0 %*% t(L), L)
   
    P <- attr(Vadj, "P")
    W <- attr(Vadj, "W")
    A1 <- A2 <- 0
    ThetaV0 <- Theta %*% V0
    n.ggamma <- length(P)
    for (ii in 1:n.ggamma) {
        for (jj in c(ii:n.ggamma)) {
            e <- ifelse(ii == jj, 1, 2)
            ui <- ThetaV0 %*% P[[ii]] %*% V0
            uj <- ThetaV0 %*% P[[jj]] %*% V0
            A1 <- A1 + e * W[ii, jj] * (.spur(ui) * .spur(uj))
            A2 <- A2 + e * W[ii, jj] * sum(ui * t(uj))
        }
    }
    
    q <- nrow(L)        # instead of finding rank
    B <- (1/(2 * q)) * (A1 + 6 * A2)
    g <- ((q + 1) * A1 - (q + 4) * A2)/((q + 2) * A2)
    c1 <- g/(3 * q + 2 * (1 - g))
    c2 <- (q - g)/(3 * q + 2 * (1 - g))
    c3 <- (q + 2 - g)/(3 * q + 2 * (1 - g))
    EE <- 1 + (A2/q)
    VV <- (2/q) * (1 + B)
    EEstar <- 1/(1 - A2/q)
    VVstar <- (2/q) * ((1 + c1 * B)/((1 - c2 * B)^2 * (1 - c3 * B)))
    V0 <- 1 + c1 * B
    V1 <- 1 - c2 * B
    V2 <- 1 - c3 * B
    V0 <- ifelse(abs(V0) < 1e-10, 0, V0)
    rho <- 1/q * (.divZero(1 - A2/q, V1))^2 * V0/V2
    df2 <- 4 + (q + 2)/(q * rho - 1)
    df2
}








##
## FIXME Backward compatibility with Russ Lenths work. Not sure if
## needed any more...
##


## COMES FROM RUSS LENTHS LSMEANS PACKAGE (he took it from pbkrtest)
#' @rdname get_ddf_Lb
#' @param Lcoef Linear contrast matrix
get_ddf_Lb <- function(object, Lcoef){
    UseMethod("get_ddf_Lb")
}

## COMES FROM RUSS LENTHS LSMEANS PACKAGE (he took it from pbkrtest)
#' @rdname get_ddf_Lb
get_ddf_Lb.lmerMod <- function(object, Lcoef){
    ddf_Lb(vcovAdj(object), Lcoef, vcov(object))
}

## COMES FROM RUSS LENTHS LSMEANS PACKAGE (he took it from pbkrtest)
#' @rdname get_ddf_Lb
#' @param VVa Adjusted covariance matrix
#' @param VV0 Unadjusted covariance matrix
#' @export
ddf_Lb <- function(VVa, Lcoef, VV0=VVa){

    if (!is.matrix(Lcoef))
        Lcoef = matrix(Lcoef, ncol = 1)

    vlb = sum(Lcoef * (VV0 %*% Lcoef))
    Theta = Matrix(as.numeric(outer(Lcoef, Lcoef) / vlb), nrow=length(Lcoef))
    
    P = attr(VVa, "P")
    W = attr(VVa, "W")

    A1 = A2 = 0
    ThetaVV0 = Theta%*%VV0
    n.ggamma = length(P)
    for (ii in 1:n.ggamma) {
        for (jj in c(ii:n.ggamma)) {
            e = ifelse(ii==jj, 1, 2)
            ui = ThetaVV0 %*% P[[ii]] %*% VV0
            uj = ThetaVV0 %*% P[[jj]] %*% VV0
            A1 =  A1 +  e* W[ii,jj] * (.spur(ui) * .spur(uj))
            A2 =  A2 +  e* W[ii,jj] *  sum(ui * t(uj))
        }}

    ## substituted q = 1 in pbkrtest code and simplified
    B  =  (A1 + 6 * A2) / 2
    g  =  (2 * A1 - 5 * A2)  / (3 * A2)
    c1 =  g/(3 + 2 * (1 - g))
    c2 =  (1 - g) / (3 + 2 * (1 - g))
    c3 =  (3 - g) / (3 + 2 * (1 - g))
    EE =  1 + A2
    VV =  2 * (1 + B)
    EEstar  =  1/(1 - A2)
    VVstar  =  2 * ((1 + c1 * B)/((1 - c2 * B)^2  *  (1 - c3 * B)))
    V0 = 1 + c1 * B
    V1 = 1 - c2 * B
    V2 = 1 - c3 * B
    V0 = ifelse(abs(V0) < 1e-10, 0, V0)
    rho  = (.divZero(1 - A2, V1))^2 * V0/V2
    df2  =  4 + 3 / (rho - 1)
    ## cat(sprintf("Lcoef: %s\n", toString(Lcoef)))
    ## cat(sprintf("df2: %f\n", df2))
    df2
}


