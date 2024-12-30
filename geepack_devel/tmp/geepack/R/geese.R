#' @title Function to fit a Generalized Estimating Equation Model
#' 
#' @description Produces an object of class `geese' which is a Generalized Estimating
#' Equation fit of the data.
#'
#' @details 
#' when the correlation structure is \code{fixed}, the specification of
#' \code{Zcor} should be a vector of length \code{sum(clusz * (clusz - 1)) /
#' 2.}
#' 
#' @aliases geese geese.fit print.geese summary.geese
#'     print.summary.geese
#' 
#' @param formula a formula expression as for \code{glm}, of the form
#'     \code{response ~ predictors}. See the documentation of lm and
#'     formula for details. As for glm, this specifies the linear
#'     predictor for modeling the mean. A term of the form
#'     \code{offset(expression)} is allowed.
#' @param sformula a formula expression of the form \code{ ~
#'     predictor}, the response being ignored. This specifies the
#'     linear predictor for modeling the dispersion. A term of the
#'     form \code{offset(expression)} is allowed.
#' @param id a vector which identifies the clusters.  The length of
#'     `id' should be the same as the number of observations.  Data
#'     are assumed to be sorted so that observations on a cluster are
#'     contiguous rows for all entities in the formula.
#' @param waves an integer vector which identifies components in
#'     clusters. The length of \code{waves} should be the same as the
#'     number of observation.  components with the same \code{waves}
#'     value will have the same link functions.
#' @param data an optional data frame in which to interpret the
#'     variables occurring in the \code{formula}, along with the
#'     \code{id} and \code{n} variables.
#' @param subset expression saying which subset of the rows of the
#'     data should be used in the fit.  This can be a logical vector
#'     (which is replicated to have length equal to the number of
#'     observations), or a numeric vector indicating which observation
#'     numbers are to be included, or a character vector of the row
#'     names to be included.  All observations are included by
#'     default.
#' @param na.action a function to filter missing data.  For \code{gee}
#'     only \code{na.omit} should be used here.
#' @param contrasts a list giving contrasts for some or all of the
#'     factors appearing in the model formula.  The elements of the
#'     list should have the same name as the variable and should be
#'     either a contrast matrix (specifically, any full-rank matrix
#'     with as many rows as there are levels in the factor), or else a
#'     function to compute such a matrix given the number of levels.
#' @param weights an optional vector of weights to be used in the
#'     fitting process. The length of \code{weights} should be the
#'     same as the number of observations. This weights is not (yet)
#'     the weight as in sas proc genmod, and hence is not recommended
#'     to use.
#' @param zcor a design matrix for correlation parameters.
#' @param corp known parameters such as coordinates used for
#'     correlation coefficients.
#' @param control a list of iteration and algorithmic constants. See
#'     \code{\link{geese.control}} for their names and default
#'     values. These can also be set as arguments to \code{geese}
#'     itself.
#' @param b an initial estimate for the mean parameters.
#' @param alpha an initial estimate for the correlation parameters.
#' @param gm an initial estimate for the scale parameters.
#' @param family a description of the error distribution and link
#'     function to be used in the model, as for \code{\link{glm}}.
#' @param mean.link a character string specifying the link function
#'     for the means. The following are allowed: \code{"identity"},
#'     \code{"logit"}, \code{"probit"}, \code{"cloglog"},
#'     \code{"log"}, and \code{"inverse"}.  The default value is
#'     determined from family.
#' @param variance a character string specifying the variance function
#'     in terms of the mean. The following are allowed:
#'     \code{"gaussian"}, \code{"binomial"}, \code{"poisson"}, and
#'     \code{"gamma"}. The default value is determined from family.
#' @param cor.link a character string specifying the link function for
#'     the correlation coefficients. The following are allowed:
#'     \code{"identity"}, and \code{"fisherz"}.
#' @param sca.link a character string specifying the link function for
#'     the scales. The following are allowed: \code{"identity"}, and
#'     \code{"log"}.
#' @param link.same a logical indicating if all the components in a
#'     cluster should use the same link.
#' @param scale.fix a logical variable; if true, the scale parameter
#'     is fixed at the value of \code{scale.value}.
#' @param scale.value numeric variable giving the value to which the
#'     scale parameter should be fixed; used only if \code{scale.fix
#'     == TRUE}.
#' @param corstr a character string specifying the correlation
#'     structure.  The following are permitted: \code{"independence"},
#'     \code{"exchangeable"}, \code{"ar1"}, \code{"unstructured"},
#'     \code{"userdefined"}, and \code{"fixed"}
## ' @param x,y \code{x} is a design matrix of dimension \code{n * p},
## '     and \code{y} is a vector of observations of length \code{n}.
## ' @param offset,soffset vector of offset for the mean and for the
## '     scale, respectively.
## ' @param zsca a design matrix of dimension \code{n * r} for the
## '     scales.
#' @param \dots further arguments passed to or from other methods.
#' @return An object of class \code{"geese"} representing the fit.
#' @author Jun Yan \email{jyan.stat@@gmail.com}
#' @seealso \code{\link{glm}}, \code{\link{lm}}, \code{\link{ordgee}}.
#' @references Yan, J. and J.P. Fine (2004) Estimating Equations for
#'     Association Structures.  \emph{Statistics in Medicine},
#'     \bold{23}, 859--880.
#' @keywords nonlinear models
#' @examples
#' 
#' data(seizure)
#' ## Diggle, Liang, and Zeger (1994) pp166-168, compare Table 8.10
#' seiz.l <- reshape(seizure,
#'                   varying=list(c("base","y1", "y2", "y3", "y4")),
#'                   v.names="y", times=0:4, direction="long")
#' seiz.l <- seiz.l[order(seiz.l$id, seiz.l$time),]
#' seiz.l$t <- ifelse(seiz.l$time == 0, 8, 2)
#' seiz.l$x <- ifelse(seiz.l$time == 0, 0, 1)
#' m1 <- geese(y ~ offset(log(t)) + x + trt + x:trt, id = id,
#'             data=seiz.l, corstr="exch", family=poisson)
#' summary(m1)
#' m2 <- geese(y ~ offset(log(t)) + x + trt + x:trt, id = id,
#'             data = seiz.l, subset = id!=49,
#'             corstr = "exch", family=poisson)
#' summary(m2)
#' ## Using fixed correlation matrix
#' cor.fixed <- matrix(c(1, 0.5, 0.25, 0.125, 0.125,
#'                       0.5, 1, 0.25, 0.125, 0.125,
#'                       0.25, 0.25, 1, 0.5, 0.125,
#'                       0.125, 0.125, 0.5, 1, 0.125,
#'                       0.125, 0.125, 0.125, 0.125, 1), 5, 5)
#' cor.fixed
#' zcor <- rep(cor.fixed[lower.tri(cor.fixed)], 59)
#' m3 <- geese(y ~ offset(log(t)) + x + trt + x:trt, id = id,
#'             data = seiz.l, family = poisson,
#'             corstr = "fixed", zcor = zcor)
#' summary(m3)
#' 
#' data(ohio)
#' fit <- geese(resp ~ age + smoke + age:smoke, id=id, data=ohio,
#'              family=binomial, corstr="exch", scale.fix=TRUE)
#' summary(fit)
#' fit.ar1 <- geese(resp ~ age + smoke + age:smoke, id=id, data=ohio,
#'                  family=binomial, corstr="ar1", scale.fix=TRUE)
#' summary(fit.ar1)
#' 
#' ###### simulated data
#' ## a function to generate a dataset
#' gendat <- function() {
#'   id <- gl(50, 4, 200)
#'   visit <- rep(1:4, 50)
#'   x1 <- rbinom(200, 1, 0.6) ## within cluster varying binary covariate
#'   x2 <- runif(200, 0, 1)   ## within cluster varying continuous covariate
#'   phi <- 1 + 2 * x1         ## true scale model
#'   ## the true correlation coefficient rho for an ar(1)
#'   ## correlation structure is 0.667.
#'   rhomat <- 0.667 ^ outer(1:4, 1:4, function(x, y) abs(x - y))
#'   chol.u <- chol(rhomat)
#'   noise <- as.vector(sapply(1:50, function(x) chol.u %*% rnorm(4)))
#'   e <- sqrt(phi) * noise
#'   y <- 1 + 3 * x1 - 2 * x2 + e
#'   dat <- data.frame(y, id, visit, x1, x2)
#'   dat
#' }
#' 
#' dat <- gendat()
#' fit <- geese(y ~ x1 + x2, id = id, data = dat, sformula = ~ x1,
#'              corstr = "ar1", jack = TRUE, j1s = TRUE, fij = TRUE)
#' summary(fit)
#' 
#' 
#' #### create user-defined design matrix of unstrctured correlation.
#' #### in this case, zcor has 4*3/2 = 6 columns, and 50 * 6 = 300 rows
#' zcor <- genZcor(clusz = rep(4, 50), waves = dat$visit, "unstr")
#' zfit <- geese(y ~ x1 + x2, id = id, data = dat, sformula = ~ x1,
#'               corstr = "userdefined", zcor = zcor,
#'               jack = TRUE, j1s = TRUE, fij = TRUE)
#' summary(zfit)
#' 
#' #### Now, suppose that we want the correlation of 1-2, 2-3, and 3-4
#' #### to be the same. Then zcor should have 4 columns.
#' z2 <- matrix(NA, 300, 4)
#' z2[,1] <- zcor[,1] + zcor[,4] + zcor[,6]
#' z2[,2:4] <- zcor[, c(2, 3, 5)]
#' summary(geese(y ~ x1 + x2, id = id, data = dat, sformula = ~ x1,
#'               corstr = "userdefined", zcor = z2,
#'               jack = TRUE, j1s = TRUE, fij = TRUE))
#' 
#' #### Next, we introduce non-constant cluster sizes by
#' #### randomly selecting 60 percent of the data
#' good <- sort(sample(1:nrow(dat), .6 * nrow(dat))) 
#' mdat <- dat[good,]
#' 
#' summary(geese(y ~ x1 + x2, id = id, data = mdat, waves = visit,
#'               sformula = ~ x1, corstr="ar1",
#'               jack = TRUE, j1s = TRUE, fij = TRUE))
#' 
#' 
#' @export geese
geese <- function(formula = formula(data),
                  sformula = ~ 1,
                  id, waves = NULL,
                  data = parent.frame(), subset = NULL, na.action = na.omit,
                  contrasts = NULL, weights = NULL,
                  ## zcor is design matrix for alpha,
                  ## corp is known paratemers to correlation coef. rho
                  zcor = NULL, corp = NULL,
                  ## zsca is constructed from sformula
                  ## control parameters
                  control = geese.control(...),
                  ## param 
                  b = NULL, alpha = NULL, gm = NULL,
                  ## geestr
                  family = gaussian(),
                  mean.link = NULL,
                  variance = NULL,
                  cor.link = "identity",
                  sca.link = "identity",
                  link.same = TRUE,
                  scale.fix = FALSE, scale.value = 1.0,
                  ## corr
                  corstr = "independence",
                  ...) {
  scall <- match.call()
  mnames <- c("", "formula", "data", "offset", "weights", "subset", "na.action", "id", "waves", "corp")
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
  w <- model.extract(m, "weights")
  if (is.null(w)) w <- rep(1, N)
  id <- model.extract(m, id)
  waves <- model.extract(m, "waves")
  corp <- model.extract(m, "corp")
  if (is.null(id)) stop("id variable not found.")

  ##print(control)
  
  ## setting up the scale model;
  ## borrowed idea from S+ function dglm by Gordon Smyth
  mcall$formula <- formula
  mcall$formula[3] <- switch(match(length(sformula), c(0,2,3)),
                             1, sformula[2], sformula[3])
  m <- eval(mcall, parent.frame())
  terms <- attr(m, "terms")
  zsca <- model.matrix(terms, m, contrasts)
  soffset <- model.extract(m, "offset")
  if (is.null(soffset)) soffset <- rep(0, N)  
 
  if (is.character(family)) family <- get(family)
  if (is.function(family))  family <- family()
  ans <- geese.fit(x, y, id, offset, soffset, w,
                   waves, zsca, zcor, corp, 
                   control,
                   b, alpha, gm,
                   family, mean.link, variance, cor.link, sca.link,
                   link.same, scale.fix, scale.value, 
                   corstr, ...)
  ans <- c(ans, list(call=scall, formula=formula)) 
  class(ans) <- "geese"
  ans
}

#' @export
geese.fit <- function(x, y, id,
                      offset=rep(0,N), soffset=rep(0,N), weights=rep(1,N),
                      waves = NULL, zsca = matrix(1,N,1),
                      zcor = NULL, corp = NULL,
                      control = geese.control(...),
                      ## param 
                      b = NULL, alpha = NULL, gm = NULL,
                      ## geestr
                      family = gaussian(),
                      mean.link = NULL,
                      variance = NULL,
                      cor.link = "identity",
                      sca.link = "identity",
                      link.same = TRUE,
                      scale.fix = FALSE, scale.value = 1.0,
                      ## corr
                      corstr = "independence", ...) {
  N <- length(id)
  ##clusz <- unlist(lapply(split(id, id), length))
  clusnew <- c(which(diff(as.numeric(id)) != 0), length(id))
  clusz <- c(clusnew[1], diff(clusnew))
  maxclsz <- max(clusz)
  if (is.null(waves)) waves <- unlist(sapply(clusz, function(x) 1:x))
  waves <- as.integer(waves)

  LINKS <- c("identity", "logit", "probit", "cloglog", "log", "inverse", "fisherz", "lwybc2", "lwylog")
  VARIANCES <- c("gaussian", "binomial", "poisson", "Gamma") ## quasi is not supported yet

  if (is.null(mean.link)) mean.link <- family$link
  if (is.null(variance)) variance <- family$family
  mean.link.v <- pmatch(mean.link, LINKS, -1, TRUE)
  cor.link.v <- pmatch(cor.link, LINKS, -1, TRUE)
  sca.link.v <- pmatch(sca.link, LINKS, -1, TRUE)
  variance.v <- pmatch(variance, VARIANCES, -1, TRUE)
  if (any(mean.link.v == -1)) stop("mean.link invalid.")
  if (any(cor.link.v == -1)) stop("cor.link invalid.")
  if (any(sca.link.v == -1)) stop("sca.link invalid.")
  if (any(variance.v == -1)) stop("variance invalid.")
  if (length(mean.link.v) != length(variance.v))
    stop("mean.link and variance not same length.")
  if (length(mean.link.v) != length(sca.link.v))
    stop("mean.link and sca.link not same lehgnt.")
      
  if (length(id) != length(y)) stop("id and y not same length.")
  if (length(offset) != length(y)) stop("offset and y not same length")
  if (length(soffset) != length(y)) stop("sca.offset and y not same length")
  if (nrow(zsca) != length(y)) stop("nrow(zsca) and length(y) not match")
  
  if (link.same) linkwaves <- rep(1, N)
  else {
    if (max(waves) != maxclsz) stop("maximum waves and maximum cluster size not equal")
    if (length(mean.link.v) != maxclsz) stop("length of mean.link not equal to the maximum cluster size.")
    linkwaves <- waves
  }
  linkwaves <- as.integer(linkwaves)
  geestr <- list(length(mean.link.v), as.integer(mean.link.v),
                 as.integer(variance.v), as.integer(sca.link.v),
                 as.integer(cor.link.v), as.integer(scale.fix))

  CORSTRS <- c("independence", "exchangeable", "ar1", "unstructured", "userdefined", "fixed")
  corstrv <- pmatch(corstr, CORSTRS, -1)
  if (corstrv == -1) stop("invalid corstr.")
  corr <- list(as.integer(corstrv), maxclsz)
  
  if (is.null(zcor)) {
    if (corstrv == 5) stop("need zcor matrix for userdefined corstr.") 
    else zcor <- genZcor(clusz, waves, corstrv)
  }
  else {
    if (!is.matrix(zcor)) zcor <- as.matrix(zcor)
    if (corstrv >= 4 && nrow(zcor) != sum(clusz * (clusz - 1) / 2)) stop("nrow(zcor) need to be equal sum(clusz * (clusz - 1) / 2) for unstructured or userdefined corstr.")
    if (corstrv %in% c(2,3) && nrow(zcor) != length(clusz)) stop("nrow(zcor) need to be equal to the number of clusters for exchangeable or ar1 corstr.")
  }
  if (!is.matrix(zcor)) zcor <- as.matrix(zcor)
  if (is.null(corp)) corp <- as.double(waves)

  p <- ncol(x)
  q <- ncol(zcor)
  r <- ncol(zsca)
  
  ## Initial values setup
  ## This may fail for binomial model with log link (relative risk)
  ## fit0 <- glm.fit(x, y, weights=weights, offset=offset, family=family)
  if (is.null(b)){
    ##b <- rep(1,p)
    fit0 <- glm.fit(x, y, weights=weights, offset=offset, family=family)
    b <- fit0$coef
  }
  if (is.null(alpha)) {
    if (corstrv == 6) alpha <- 1
    else alpha <- rep(0,q)
  }
  if (is.null(gm)) {
    ##gm <- rep(scale.value, r)
    qlf <- quasi(LINKS[sca.link.v])$linkfun
    ## pr2 <- (residuals.glm(fit0, type="pearson")) ^ 2
    mu <- quasi(LINKS[mean.link.v])$linkinv(x %*% b)
    pr2 <- (y - mu) ^ 2 / family$variance(mu)
    gm <- lm.fit(zsca, qlf(pr2), offset = soffset)$coef
  }
  param <- list(b, alpha, gm)

  ans <- .Call("gee_rap", y, x, offset, soffset, weights,
               linkwaves, zsca, zcor, corp,
               clusz, geestr, corr, param, control, PACKAGE = "geepack")
  names(ans) <- c("beta", "alpha", "gamma", "vbeta", "valpha", "vgamma",
                  "vbeta.naiv", "valpha.naiv", "valpha.stab",
                  "vbeta.ajs", "valpha.ajs", "vgamma.ajs",
                  "vbeta.j1s", "valpha.j1s", "vgamma.j1s",
                  "vbeta.fij", "valpha.fij", "vgamma.fij",
                  "error")
  ans$xnames <- dimnames(x)[[2]]
  ans$zsca.names <- dimnames(zsca)[[2]]
  ans$zcor.names <- dimnames(zcor)[[2]]
  if (is.null(ans$zcor.names)) ans$zcor.names = paste("alpha", 1:ncol(zcor), sep=":")
  names(ans$beta) <- ans$xnames
  names(ans$gamma) <- ans$zsca.names
  if (length(ans$alpha) > 0)  names(ans$alpha) <- ans$zcor.names

  param <- list(ans$beta, ans$alpha, ans$gamma)
  infls <- .Call("infls_rap",  y, x, offset, soffset, weights,
               linkwaves, zsca, zcor, corp,
               clusz, geestr, corr, param, control, PACKAGE = "geepack")
  rownames(infls) <- c(paste("beta", names(ans$beta), sep="_"),
                       if (length(ans$gamma) > 0) paste("gamma", names(ans$gamma), sep="_") else NULL,
                       if (length(ans$alpha) > 0) paste("alpha", names(ans$alpha), sep="_") else NULL)

  ans <- c(ans,
           list(infls=infls,
                clusz=clusz, control=control,
                model=list(mean.link=mean.link,
                  variance=variance, sca.link=sca.link,
                  cor.link=cor.link, corstr=corstr, scale.fix=scale.fix)))
  ans
}



#' @title Auxiliary for Controlling GEE Fitting
#' 
#' @description Auxiliary function as user interface for `gee' fitting. Only used when
#' calling `geese' or `geese.fit'.
#' 
#' @details When `trace' is true, output for each iteration is printed to the screen by
#' the c++ code. Hence, `options(digits = *)' does not control the precision.
#' 
#' @param epsilon positive convergence tolerance epsilon; the
#'     iterations converge when the absolute value of the difference
#'     in parameter estimate is below \code{epsilon}.
#' @param maxit integer giving the maximal number of Fisher Scoring
#'     iteration.
#' @param trace logical indicating if output should be produced for
#'     each iteration.
#' @param scale.fix logical indicating if the scale should be fixed.
#' @param jack logical indicating if approximate jackknife variance
#'     estimate should be computed.
#' @param j1s logical indicating if 1-step jackknife variance estimate
#'     should be computed.
#' @param fij logical indicating if fully iterated jackknife variance
#'     estimate should be computed.
#' @return A list with the arguments as components.
#' @author Jun Yan \email{jyan.stat@@gmail.com}
#' @seealso `geese.fit', the fitting procedure used by `geese'.
#' @keywords optimize models
#' 
#' @export geese.control
geese.control <- function (epsilon = 1e-04, maxit = 25, trace = FALSE,
                           scale.fix = FALSE, jack = FALSE,
                           j1s = FALSE, fij = FALSE) {
  if (!is.numeric(epsilon) || epsilon <= 0) 
    stop("value of epsilon must be > 0")
  if (!is.numeric(maxit) || maxit <= 0) 
    stop("maximum number of iterations must be > 0")
  list(trace = as.integer(trace),
       jack = as.integer(jack), j1s = as.integer(j1s), fij = as.integer(fij),
       maxit = as.integer(maxit), epsilon = epsilon)
}


## compare coefficients

#' @title Compare Regression Coefficiente between Nested Models
#' 
#' @description Comparing regression coefficients between models when one model is nested
#' within another for clustered data.
#' 
#' @param fit0 a fitted object of class \code{geese}
#' @param fit1 another fitted object of class \code{geese}
#'
#' @return a list of two components: \item{delta}{estimated difference
#'     in the coefficients of common covariates from \code{fit0} and
#'     \code{fit1}} \item{variance}{estimated variance matrix of
#'     delta}
#' @author Jun Yan \email{jyan.stat@@gmail.com}
#' @references Allison, P. D. (1995). The impact of random predictors
#'     on comparisons of coefficients between models: Comment on
#'     Clogg, Petkova, and Haritou.  \emph{American Journal of
#'     Sociology}, \bold{100}(5), 1294--1305.
#' 
#' Clogg, C. C., Petkova, E., and Haritou, A. (1995). Statistical methods for
#' comparing regression coefficients between models.  \emph{American Journal of
#' Sociology}, \bold{100}(5), 1261--1293.
#' 
#' Yan, J., Aseltine, R., and Harel, O. (2011). Comparing Regression
#' Coefficients Between Nested Linear Models for Clustered Data with
#' Generalized Estimating Equations. \emph{Journal of Educational and
#' Behaviorial Statistics}, Forthcoming.
#' 
#' @keywords models
#' @examples
#' 
#' ## generate clustered data
#' gendat <- function(ncl, clsz) {
#' ## ncl: number of clusters
#' ## clsz: cluster size (all equal)
#'   id <- rep(1:ncl, each = clsz)
#'   visit <- rep(1:clsz, ncl)
#'   n <- ncl * clsz
#'   x1 <- rbinom(n, 1, 0.5) ## within cluster varying binary covariate
#'   x2 <- runif(n, 0, 1)   ## within cluster varying continuous covariate
#'   ## the true correlation coefficient rho for an ar(1)
#'   ## correlation structure is 2/3
#'   rho <- 2/3
#'   rhomat <- rho ^ outer(1:4, 1:4, function(x, y) abs(x - y))
#'   chol.u <- chol(rhomat)
#'   noise <- as.vector(sapply(1:ncl, function(x) chol.u %*% rnorm(clsz)))
#'   y <- 1 + 3 * x1 - 2 * x2 + noise
#'   dat <- data.frame(y, id, visit, x1, x2)
#'   dat
#' }
#' 
#' simdat <- gendat(100, 4)
#' fit0 <- geese(y ~ x1, id = id, data = simdat, corstr = "un")
#' fit1 <- geese(y ~ x1 + x2, id = id, data = simdat, corstr = "un")
#' compCoef(fit0, fit1)
#' 
#' @export compCoef
#' 
compCoef <- function(fit0, fit1) {
  v0 <- names(fit0$beta)
  v1 <- names(fit1$beta)
  v0idx <- (1:length(v0))[v0 %in% v1]
  v1idx <- (1:length(v1))[v1 %in% v0]
  delta <- fit0$beta[v0idx] - fit1$beta[v1idx]
  infls <- fit0$infls[v0idx,] - fit1$infls[v1idx,]
  robvar <- infls %*% t(infls)
  list(delta = delta, variance = robvar)  
}
