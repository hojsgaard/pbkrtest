## ##########################################################################
##
#' @title F-test and degrees of freedom based on Satterthwaite approximation
#' @description An approximate F-test based on the Satterthwaite approach.
#' @name sat-modcomp
#' 
## ##########################################################################

#' @details
#'
## #' The model \code{object} must be fitted with restricted maximum
## #'     likelihood (i.e. with \code{REML=TRUE}). If the object is fitted with
## #'     maximum likelihood (i.e. with \code{REML=FALSE}) then the model is
## #'     refitted with \code{REML=TRUE} before the p-values are calculated. Put
## #'     differently, the user needs not worry about this issue.
## #' 
## #' An F test is calculated according to the approach of Kenward and Roger
## #' (1997).  The function works for linear mixed models fitted with the
## #' \code{lmer} function of the \pkg{lme4} package. Only models where the
## #' covariance structure is a sum of known matrices can be compared.
## #' 
## #' The \code{largeModel} may be a model fitted with \code{lmer} either using
## #' \code{REML=TRUE} or \code{REML=FALSE}.  The \code{smallModel} can be a model
## #' fitted with \code{lmer}. It must have the same covariance structure as
## #' \code{largeModel}. Furthermore, its linear space of expectation must be a
## #' subspace of the space for \code{largeModel}.  The model \code{smallModel}
## #' can also be a restriction matrix \code{L} specifying the hypothesis \eqn{L
## #' \beta = L \beta_H}, where \eqn{L} is a \eqn{k \times p}{k X p} matrix and
## #' \eqn{\beta} is a \eqn{p} column vector the same length as
## #' \code{fixef(largeModel)}.
#' 
## #' The \eqn{\beta_H} is a \eqn{p} column vector.
## #' 
## #' Notice: if you want to test a hypothesis \eqn{L \beta = c} with a \eqn{k}
## #' vector \eqn{c}, a suitable \eqn{\beta_H} is obtained via \eqn{\beta_H=L c}
## #' where \eqn{L_n} is a g-inverse of \eqn{L}.
#' 
#' Notice: It cannot be guaranteed that the results agree with other
#' implementations of the Satterthwaite approach!
#' 
#' @param largeModel An \code{lmerMod} model.
#' @param smallModel An \code{lmerMod} model, a restriction matrix or
#'     a model formula. See example section.
#' @param eps A small number.
#' @param details If larger than 0 some timing details are printed.
#'
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' 
#' @seealso \code{\link{getKR}}, \code{\link{lmer}}, \code{\link{vcovAdj}},
#'     \code{\link{PBmodcomp}}
#' 
#' @references Ulrich Halekoh, Søren Højsgaard (2014)., A Kenward-Roger
#'     Approximation and Parametric Bootstrap Methods for Tests in Linear Mixed
#'     Models - The R Package pbkrtest., Journal of Statistical Software,
#'     58(10), 1-30., \url{https://www.jstatsoft.org/v59/i09/}
#' 
#' @keywords models inference
#' @examples
#'
#' (fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
#' L1 <- cbind(0,1)
#' SATmodcomp(fm1, L1)
#'
#' (fm2 <- lmer(Reaction ~ Days + I(Days^2) + (Days|Subject), sleepstudy))
#'
#' ## Test for no effect of Days. There are three ways of using the function:
#' 
#' ## 1) Define 2-df contrast - since L has 2 (linearly independent) rows
#' ## the F-test is on 2 (numerator) df:
#' L2 <- rbind(c(0, 1, 0), c(0, 0, 1))
#' SATmodcomp(fm2, L2)
#'
#' ## 2) Use two model objects 
#' fm3 <- update(fm2, ~. - Days - I(Days^2))
#' SATmodcomp(fm2, fm3)
#'
#' ## 3) Specify restriction as formula
#' SATmodcomp(fm2, ~. - Days - I(Days^2))

#' @export
#' @rdname sat-modcomp
SATmodcomp <- function(largeModel, smallModel, details=0, eps=sqrt(.Machine$double.eps)){
    UseMethod("SATmodcomp")
}

#' @export
#' @rdname sat-modcomp
SATmodcomp.lmerMod <- function(largeModel, smallModel, details=0, eps=sqrt(.Machine$double.eps)){
    SATmodcomp_internal(largeModel=largeModel, smallModel=smallModel, eps=eps)
}


SATmodcomp_internal <- function(largeModel, smallModel, eps=sqrt(.Machine$double.eps)){

    if (inherits(smallModel, "formula"))
        smallModel  <- update(largeModel, smallModel)

    w <- modcomp_init(largeModel, smallModel, matrixOK = TRUE)

    if (w == -1) stop('Models have equal mean stucture or are not nested...')
    if (w == 0){
        ## First given model is submodel of second; exchange the models
        tmp <- largeModel; largeModel <- smallModel; smallModel <- tmp
    }

    ## All computations are based on 'largeModel' and the restriction matrix 'L'
    ## -------------------------------------------------------------------------
    
    t0    <- proc.time()    
    L     <- model2restriction_matrix(largeModel, smallModel)
     
    beta <- getME(largeModel, "beta")
    aux  <- compute_auxiliary(largeModel)
    vcov_Lbeta <- L %*% aux$vcov_beta %*% t(L) # Var(contrast) = Var(Lbeta)

    eig_vcov_Lbeta <- eigen(vcov_Lbeta)
    P   <- eig_vcov_Lbeta$vectors
    d   <- eig_vcov_Lbeta$values
    tol <- max(eps * d[1], 0)
    pos <- d > tol
    qq  <- sum(pos) # rank(vcov_Lbeta)
    
    PtL <- crossprod(P, L)[1:qq,, drop=FALSE]
    ## print(PtL)
    
    t2     <- drop(PtL %*% beta)^2 / d[1:qq]
    Fvalue <- sum(t2) / qq

    grad_PLcov <- lapply(1:qq, function(m) {
        vapply(aux$jacobian_list, function(J)
            qform(PtL[m, ], J), numeric(1L))
    })

    ## 2D_m^2 / g'Ag
    nu_m <- vapply(1:qq, function(m) {
        2*(d[m])^2 / qform(grad_PLcov[[m]], aux$vcov_varpar)
    }, numeric(1L)) 

    ## Compute ddf for the F-value:
    ddf <- get_Fstat_ddf(nu_m, tol=1e-8)

    out <- list(test=data.frame(statistic=Fvalue, ndf=qq, ddf=ddf, p.value=1 - pf(Fvalue, df1=qq, df2=ddf)),
                sigma=getME(largeModel, "sigma"),
                formula.large=formula(largeModel),
                formula.small=L,
                ctime=(proc.time() - t0)[3],
                L=L
                )
    class(out) <- "SATmodcomp"
    out
}



#' @export
print.SATmodcomp <- function(x, ...){
    cat("large : ")
    print(x$formula.large)

    if (inherits(x$formula.small, "formula")) cat("small : ")
    else cat("small (restriction matrix) : \n")
    prform(x$formula.small)
    dd <- as.data.frame(x$test[c("statistic", "ndf", "ddf", "p.value")])
    printCoefmat(dd, has.Pvalue=TRUE)
    invisible(x)
}

prform  <- function(form){
    if (!inherits(form, c("formula", "matrix"))) stop("'form' must be formula or matrix")
    if (inherits(form, "formula"))
        print(form)
    else
        prmatrix(form, collab = rep_len("", ncol(form)), rowlab = rep_len("", ncol(form)))
    invisible(form)    
}


## #' @export
## tidy.KRmodcomp <- function (x, conf.int = FALSE, conf.level = 0.95, exponentiate = FALSE, 
##     ...) 
## {
##     ##co <- stats::coef(summary(x))
##     ## ret <- as_tidy_tibble(co, c("estimate", "std.error", "statistic", 
##     ##                             "p.value")[1:ncol(co)])

##     ret <- x$test
##     rr <<- ret


##     if (conf.int) {
##         ci <- broom:::broom_confint_terms(x, level = conf.level)
##         ret <- dplyr::left_join(ret, ci, by = "term")
##     }
##     ## if (exponentiate) {
##     ##     if (is.null(x$family) || (x$family$link != "logit" && 
##     ##         x$family$link != "log")) {
##     ##         warning(paste("Exponentiating coefficients, but model did not use", 
##     ##             "a log or logit link function"))
##     ##     }
##     ##     ret <- exponentiate(ret)
##     ## }
##     ret
## }


#' @export 
tidy.PBmodcomp <- function(x, ...){
    ret <- x$test    
    as_tibble(cbind(type=rownames(ret), ret))
}

#' @export 
tidy.KRmodcomp <- function(x, ...){
    ret <- x$test    
    as_tibble(cbind(type=rownames(ret), ret))
}

#' @export 
tidy.SATmodcomp <- function(x, ...){
    ret <- x$test
    as_tibble(cbind(type="Ftest", ret))    
}

#' @export 
as.data.frame.PBmodcomp <- function(x, ...){
    x$test
}

#' @export 
as.data.frame.KRmodcomp <- function(x, ...){
    x$test
}

#' @export 
as.data.frame.SATmodcomp <- function(x, ...){
    x$test
}








## #' @export
## summary.PBmodcomp <- function(object, ...){
##   ans <- .summarizePB(object$LRTstat, object$ref)
##   ans$formula.large <- object$formula.large
##   ans$formula.small <- object$formula.small
##   class(ans) <- "summaryPB"
##   ans
## }

## #' @export
## print.summaryPB <- function(x, ...){
##   .PBcommon(x)
##   ans <- x$test
##   printCoefmat(ans, tst.ind=1, na.print='', has.Pvalue=TRUE)
##   cat("\n")

## ##   ci <- x$ci
## ##   cat(sprintf("95 pct CI for PBtest   : [%s]\n", toString(ci)))
## ##   mo <- x$moment
## ##   cat(sprintf("Reference distribution : mean=%f var=%f\n", mo[1], mo[2]))
## ##   ga <- x$gamma
## ##   cat(sprintf("Gamma approximation    : scale=%f shape=%f\n", ga[1], ga[2]))

##   return(invisible(x))
## }





## Returns the deviance function for a linear mixed model.
get_devfun <- function(model){
    if (!inherits(model, "lmerMod")) stop("'model' not an 'lmerMod'")
    mc <- model@call
    args <- as.list(mc)
    args$devFunOnly <- TRUE
    Call <- as.call(c(list(quote(lme4::lmer)), args[-1]))
    devfun <- eval.parent(Call)
    devfun

}

## #######################################################
## ####### compute_auxillary
## ####### ################################################ #'
#'
#' Compute_auxiliary quantities needed for the Satterthwaite
#' approximation.
#'
#' Computes vcov of variance parameters (theta, sigma), jacobian of
#' each variance parameter etc.
#'
#' @param model A linear mixed model object
#' @param tol A tolerance
#'
#' @author Søren Højsgaard
#'
#' @return A list
#' @keywords internal

compute_auxiliary <- function(model, tol=1e-6){
    
    if (!inherits(model, "lmerMod")) stop("'model' not an 'lmerMod'")

    devfun <- get_devfun(model)
    
    ## tmp <- list(Call=Call, devfun=devfun) ## SH
    ## assign("tmp", tmp, envir=.GlobalEnv)
    
    out <- list(sigma=NULL, vcov_beta=NULL, vcov_varpar=NULL, jacobian_list=NULL)
 
    out$sigma <- sigma(model)
    out$vcov_beta <- as.matrix(vcov(model))                       

    ## The optimized variance parameters (theta, sigma)
    varpar_opt <- unname(c(getME(model, "theta"), getME(model, "sigma")))

    ## Compute Hessian:
    ## ----------------
    is_reml <- getME(model, "is_REML")
    h <- numDeriv::hessian(func=devfun_vp, x=varpar_opt,
                           devfun=devfun, reml=is_reml)
    
    ## Eigen decompose the Hessian:
    eig_h <- eigen(h, symmetric=TRUE)
    evals <- eig_h$values
    neg <- evals < -tol
    pos <- evals > tol
    zero <- evals > -tol & evals < tol
    if(sum(neg) > 0) { # negative eigenvalues
        ##eval_chr <- if(sum(neg) > 1) "eigenvalues" else "eigenvalue"
        evals_num <- paste(sprintf("%1.1e", evals[neg]), collapse = " ")
        warning(sprintf("Model failed to converge with %d negative eigenvalue(s): %s",
                        sum(neg), evals_num), call.=FALSE)
    }
    ## Note: we warn about negative AND zero eigenvalues:
    if(sum(zero) > 0) { # some eigenvalues are zero
        ##eval_chr <- if(sum(zero) > 1) "eigenvalues" else "eigenvalue"
        evals_num <- paste(sprintf("%1.1e", evals[zero]), collapse = " ")
        warning(sprintf("Model may not have converged with %d eigenvalue(s) close to zero: %s",
                        sum(zero), evals_num))
    }
    
    ## Compute vcov(varpar):
    ## ---------------------
    pos <- eig_h$values > tol
    q <- sum(pos)

    ## Moore-Penrose generalized inverse for h:
    h_inv <- with(eig_h, {
        vectors[, pos, drop=FALSE] %*% diag(1/values[pos], nrow=q) %*%
            t(vectors[, pos, drop=FALSE]) })

    out$vcov_varpar <- 2 * h_inv # vcov(varpar)

    
    ## Compute Jacobian of cov(beta)
    ## -----------------------------

    ## Compute Jacobian for each varpar and save in list:
    jac <- numDeriv::jacobian(func=get_covbeta, x=varpar_opt, devfun=devfun)

    ## List of jacobian matrices
    out$jacobian_list <- lapply(1:ncol(jac), function(i)
        array(jac[, i], dim=rep(length(getME(model, "beta")), 2)))

    out
}

qform <- function(x, A) {
  sum(x * (A %*% x)) 
}

## ##############################################
## ######## get_Fstat_ddf()
## ##############################################
#' Compute denominator df for F-test
#'
#' From a vector of denominator df from independent t-statistics (\code{nu}),
#' the denominator df for the corresponding F-test is computed.
#'
#' Note that if any \code{nu <= 2} then \code{2} is returned. Also, if all nu
#' are within tol of each other the simple average of the nu-vector is returned.
#' This is to avoid downward bias.
#'
#' @param nu vector of denominator df for the t-statistics
#' @param tol tolerance on the consecutive differences between elements of nu to
#' determine if mean(nu) should be returned
#'
#' @author Rune Haubo B. Christensen. Adapted to pbkrtest by Søren Højsgaard.
#'
#' @return the denominator df; a numerical scalar
#' @keywords internal

get_Fstat_ddf <- function(nu, tol=1e-8) {
  # Computes denominator df for an F-statistic that is derived from a sum of
  # squared t-statistics each with nu_m degrees of freedom.
  #
  # nu : vector of denominator df for the t-statistics
  # tol: tolerance on the consequtive differences between elements of nu to
  #      determine if mean(nu) should be returned.
  #
  # Result: a numeric scalar
  #
  # Returns nu if length(nu) == 1. Returns mean(nu) if all(abs(diff(nu)) < tol;
  # otherwise ddf appears to be downward biased.
  fun <- function(nu) {
    if(any(nu <= 2)) 2 else {
      E <- sum(nu / (nu - 2))
      2 * E / (E - (length(nu))) # q = length(nu) : number of t-statistics
    }
  }
  stopifnot(length(nu) >= 1,
            # all(nu > 0), # returns 2 if any(nu < 2)
            all(sapply(nu, is.numeric)))
  if(length(nu) == 1L) return(nu)
  if(all(abs(diff(nu)) < tol)) return(mean(nu))
  if(!is.list(nu)) fun(nu) else vapply(nu, fun, numeric(1L))
}


##############################################
######## devfun_vp()
##############################################
#' Compute Deviance of an LMM as a Function of Variance Parameters
#'
#' This function is used for extracting the asymptotic variance-covariance matrix
#'   of the variance parameters.
#'
#' @param varpar variance parameters; \code{varpar = c(theta, sigma)}.
#' @param devfun deviance function as a function of theta only.
#' @param reml if \code{TRUE} the REML deviance is computed;
#'   if \code{FALSE}, the ML deviance is computed.
#'
#' @return the REML or ML deviance.
#' @author Rune Haubo B. Christensen. Adapted to pbkrtest by Søren Højsgaard.
#' @keywords internal
devfun_vp <- function(varpar, devfun, reml) {
  nvarpar <- length(varpar)
  sigma2  <- varpar[nvarpar]^2
  theta   <- varpar[-nvarpar]
  df_envir <- environment(devfun)

  ## SH: call below not stored anywhere. Is it being used?
  devfun(theta) # Evaluate deviance function at varpar

  n <- nrow(df_envir$pp$V)
  # Compute deviance for ML:
  dev <- df_envir$pp$ldL2() + (df_envir$resp$wrss() + df_envir$pp$sqrL(1)) / sigma2 +
      n * log(2 * pi * sigma2)

  if (!reml) return(dev)

  ## Adjust if REML is used:
  RX <- df_envir$pp$RX() # X'V^{-1}X ~ crossprod(RX^{-1}) = cov(beta)^{-1} / sigma^2
  dev + 2*c(determinant(RX)$modulus) - ncol(RX) * log(2 * pi * sigma2)
}


##############################################
######## get_covbeta()
##############################################
#' Compute cov(beta) as a function of varpar of an LMM
#'
#' At the optimum cov(beta) is available as vcov(lmer-model). This function
#' computes cov(beta) at non (RE)ML estimates of \code{varpar}.
#'
#' @inheritParams devfun_vp
#'
#' @return cov(beta) at supplied varpar values.
#' @author Rune Haubo B. Christensen. Adapted to pbkrtest by Søren Højsgaard.
#' @keywords internal
#' 
get_covbeta <- function(varpar, devfun) {
  nvarpar <- length(varpar)
  sigma <- varpar[nvarpar]        # residual std.dev.
  theta <- varpar[-nvarpar]       # ranef var-par
  devfun(theta)                   # evaluate REML or ML deviance 'criterion'
  df_envir <- environment(devfun) # extract model environment
  sigma^2 * tcrossprod(df_envir$pp$RXi()) # vcov(beta)
}


