#' @title GEE for Clustered Ordinal Responses
#' 
#' @description Produces an object of class `geese' which is a
#'     Generalized Estimating Equation fit of the clustered ordinal
#'     data.
#' 
#' 
#' @param formula a formula expression as for \code{glm}, of the form
#'     \code{response ~ predictors}. See the documentation of lm and
#'     formula for details. As for glm, this specifies the linear
#'     predictor for modelling the mean. A term of the form
#'     \code{offset(expression)} is allowed.
#' @param ooffset vector of offset for the odds ratio model.
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
#'     same as the number of observations.
#' @param z a design matrix for the odds ratio model. The number of
#'     rows of z is \deqn{c^2 \sum n_i(n_i - 1)/2,} where \eqn{n_i} is
#'     the cluster size, and \eqn{c} is the number of categories minus
#'     1.
#' @param mean.link a character string specifying the link function
#'     for the means. The following are allowed: \code{"logit"},
#'     \code{"probit"}, and \code{"cloglog"}.
#' @param corstr a character string specifying the log odds. The
#'     following are allowed: \code{"independence"},
#'     \code{"exchangeable"}, \code{"unstructured"}, and
#'     \code{"userdefined"}.
#' @param control a list of iteration and algorithmic constants. See
#'     \code{\link{geese.control}} for their names and default
#'     values. These can also be set as arguments to \code{geese}
#'     itself.
#' @param b an initial estimate for the mean parameters.
#' @param alpha an initial estimate for the odds ratio parameters.
#' @param scale.fix a logical variable indicating if scale is fixed;
#'     it is set at TRUE currently (it can not be FALSE yet!).
#' @param scale.val this argument is ignored currently.
#' @param int.const a logical variable; if true, the intercepts are
#'     constant, and if false, the intercepts are different for
#'     different components in the response.
#' @param rev a logical variable. For example, for a three level
#'     ordered response Y = 2, the accumulated indicator is coded as
#'     (1, 0, 0) if true and (0, 1, 1) if false.
#' @param \dots further arguments passed to or from other methods.
#' @return An object of class \code{"geese"} representing the fit.
#' @author Jun Yan \email{jyan.stat@@gmail.com}
#' @seealso \code{\link{glm}}, \code{\link{lm}}, \code{\link{geese}}.
#' @references Heagerty, P.J. and Zeger, S.L. (1996) Marginal
#'     regression models for clustered ordinal measurements.
#'     \emph{JASA}, \bold{91} 1024--1036.
#' @keywords nonlinear models
#' @examples
#' 
#' data(respdis)
#' resp.l <- reshape(respdis, varying =list(c("y1", "y2", "y3", "y4")),
#'                   v.names = "resp", direction = "long")
#' resp.l <- resp.l[order(resp.l$id, resp.l$time),]
#' fit <- ordgee(ordered(resp) ~ trt, id=id, data=resp.l, int.const=FALSE)
#' summary(fit)
#' 
#' data(ohio)
#' ohio$resp <- ordered(as.factor(ohio$resp))
#' fit <- ordgee(resp ~ age + smoke + age:smoke, id = id, data=ohio)
#' summary(fit)
#' 
#' @export ordgee
ordgee <- function(formula = formula(data), ooffset = NULL,
                   id, waves = NULL,
                   data=parent.frame, subset=NULL, na.action=na.omit,
                   contrasts=NULL, weights=NULL,
                   z=NULL, ##family=binomial(),
                   mean.link="logit",
                   corstr="independence",
                   control=geese.control(...),
                   b=NA, alpha=NA,
                   scale.fix=TRUE, scale.val=1,
                   int.const=TRUE, rev=FALSE, ##rev TRUE for coding in HZ 1996.
                   ...) {
### y is sum(n_i) * c x 1
### x is sum(n_i) * c x (p + c)
  scall <- match.call()
  mnames <- c("", "formula", "data", "offset", "weights", "subset", "id", "waves")
  cnames <- names(scall)
  cnames <- cnames[match(mnames,cnames,0)]
  mcall <- scall[cnames]
  if (is.null(mcall$id)) mcall$id <- as.name("id")
  mcall[[1]] <- as.name("model.frame")
  m <- eval(mcall, parent.frame())

  id <- model.extract(m, "id")
##  N <- length(unique(id))
  clusz <- unlist(lapply(split(id, id), length))
  maxclsz <- max(clusz)
  if (is.null(waves)) waves <- unlist(sapply(clusz, function(x) 1:x))
  else waves <- model.extract(m, "waves")
#   if (is.na(b)){
#    foo <- polr(formula, data, ...)
#    b <- c(foo$zeta, foo$coef)
#   }

  y <- model.extract(m, "response")
  if (length(y) != length(id)) stop("response and id are not of the same length.")

  if (class(y)[1] != 'ordered') stop("response is not an ordered factor.")
  lev <- levels(y)

  nlev <- length(lev)
  ncat <- nlev - 1
  y <- unclass(y)
  Y <- rep(y, rep(ncat, sum(clusz)))
  if (rev) Y <- as.double(Y <= rep(1:ncat, sum(clusz)))
  else Y <- as.double(Y > rep(1:ncat, sum(clusz)))
  
  mterms <- attr(m, "terms")
  x <- model.matrix(mterms, m, contrasts)
  xvars <- as.character(attr(mterms, "variables"))[-1]
  if ((yvar <- attr(mterms, "response")) > 0) 
    xvars <- xvars[-yvar]
  xlev <- if (length(xvars) > 0) {
    xlev <- lapply(m[xvars], levels)
    xlev[!sapply(xlev, is.null)]
  }
  xint <- match("(Intercept)", colnames(x), nomatch = 0)
  n <- nrow(x)
  pc <- ncol(x)
  if (xint > 0) {
    x <- x[, -xint, drop = FALSE]
    pc <- pc - 1
  }
  else warning("an intercept is needed and assumed")
  ind <- gl(sum(clusz), ncat)
  x <- x[ind,, drop=FALSE]
    
  if (int.const) {
    xc <- matrix(diag(ncat), sum(clusz) * ncat, ncat, byrow=TRUE)
    colnames(xc) <- paste("Inter", lev[1:ncat], sep=":")
  }
  else {
    foo <- sapply(waves,
                  function(x, maxclsz, ncat) {
                    bar <- matrix(0, maxclsz*ncat, ncat)
                    bar[(x-1)*ncat + 1:ncat,] <- diag(ncat)
                    bar
                  }, maxclsz=maxclsz, ncat=ncat)
    xc <- matrix(unlist(foo), ncol=maxclsz*ncat, byrow=TRUE)
    colnames(xc) <- paste("Inter", paste(rep(1:maxclsz, rep(ncat, maxclsz)), rep(lev[1:ncat], maxclsz), sep=":"), sep=":")
    ##b <- c(rep(b[1:ncat], maxclsz), b[-(1:ncat)])
  }

  xmat <- cbind(xc, x) # note the negate sign!!!
  p <- ncol(xmat)

  offset <- model.extract(m, "offset")
  if (is.null(offset)) offset <- rep(0, length(id))
  offset <- - rep(offset, rep(ncat, sum(clusz)))

  w <- model.extract(m, "weights")
  if (is.null(w)) w <- rep(1, length(id))
  w <- rep(w, rep(ncat, sum(clusz)))

  CORSTRS <- c("independence", "exchangeable", "NA_ar1", "unstructured", "userdefined")
  CORSTRS.ALLOWED <- c("independence", "exchangeable", "unstructured", "userdefined")
  corstrv <- pmatch(corstr, CORSTRS.ALLOWED, -1)
  if (corstrv == -1) stop("invalid corstr.")
  corstrv <- pmatch(corstr, CORSTRS)
  corr <- list(as.integer(corstrv), maxclsz)

  if (is.null(ooffset)) ooffset <- rep(0, sum(clusz*(clusz-1)/2) * ncat^2)
  if (is.null(z)) {
    if (corstrv == 5) stop("need z matrix for userdefined corstr.") 
    else z <- genZodds(clusz, waves, corstrv, ncat)
  }

  if (length(ooffset) != sum(clusz*(clusz-1)/2) * ncat^2) 
	stop("length(ooffset) != sum(clusz*(clusz-1)) * ncat^2 detected.")

  if (corstrv > 1 && nrow(z) != sum(clusz*(clusz-1)/2) * ncat^2) 
	stop("nrow(z) != sum(clusz*(clusz-1)) * ncat^2 detected.")
  
  waves <- rep(waves, rep(ncat, sum(clusz)))
  if (is.null(id)) stop("ID variable not found.")

  LINKS <- c("NA_identity", "logit", "probit", "cloglog", "NA_log", "NA_inverse", "NA_fisherz", "NA_lwybc2", "NA_lwylog")
  LINKS.ALLOWED <- c("logit", "probit", "cloglog")
  mean.link.v <- pmatch(mean.link, LINKS.ALLOWED, -1)
  if (mean.link.v == -1) stop("mean.link invalid.")
  mean.link.v <- pmatch(mean.link, LINKS, -1)
  
  geestr <- list(maxwave=maxclsz,
                 mean.link=rep(mean.link.v, maxclsz),
                 variance=rep(2, maxclsz),
                 sca.link=rep(1, maxclsz),
                 cor.link=5,
                 scale.fix=as.integer(scale.fix))

  p <- ncol(xmat)
  q <- ncol(z)
  if (!is.matrix(z)) z <- as.matrix(z)

  if (is.na(b)) {
    link <- mean.link
    b <- glm.fit(xmat, Y, w, family=binomial(link))$coef
  }
  if (is.na(alpha)) alpha <- rep(0,q);
  param <- list(b, alpha, gm=rep(scale.val, 1))


  ans <- .Call("ordgee_rap", Y, xmat, offset, ooffset, w, waves, z,
               clusz, ncat, rev, geestr, corr, param, control,
               PACKAGE = "geepack")

    names(ans) <- c("beta", "alpha", "gamma", "vbeta", "valpha", "vgamma",
                  "vbeta.naiv", "valpha.naiv", "valpha.stab",
                  "vbeta.ajs", "valpha.ajs", "vgamma.ajs",
                  "vbeta.j1s", "valpha.j1s", "vgamma.j1s",
                  "vbeta.fij", "valpha.fij", "vgamma.fij",
                  "error")
    ans$xnames <- dimnames(xmat)[[2]]
    ans$zcor.names <- dimnames(z)[[2]]
    names(ans$beta) <- ans$xnames
    names(ans$alpha) <- ans$zcor.names

    ans <- c(ans, list(call=scall, clusz=clusz, control=control,
                     model=list(mean.link=mean.link,
                                variance="binomial", sca.link=NULL,
                                cor.link="log", corstr=corstr, scale.fix=scale.fix)))
    class(ans) <- "geese"
    ans
}
