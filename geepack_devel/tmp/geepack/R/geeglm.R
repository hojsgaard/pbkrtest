#' @title Fit Generalized Estimating Equations (GEE)
#' 
#' @description The geeglm function fits generalized estimating equations using
#'     the 'geese.fit' function of the 'geepack' package for doing the actual
#'     computations. geeglm has a syntax similar to glm and returns an object
#'     similar to a glm object.  An important feature of geeglm, is that an
#'     anova method exists for these models.
#' 
#' @name geeglm
#' 
#' @details In the case of corstr="fixed" one must provide the zcor vector if
#'     the clusters have unequal sizes. Clusters with size one must not be
#'     represented in zcor.
#'
#' @param formula See corresponding documentation to \code{glm}
#' @param family See corresponding documentation to \code{glm}
#' @param data See corresponding documentation to \code{glm}
#' @param weights See corresponding documentation to \code{glm}
#' @param subset See corresponding documentation to \code{glm}
#' @param na.action No action is taken. Indeed geeglm only works on complete
#'     data.
#' @param start See corresponding documentation to \code{glm}
#' @param etastart See corresponding documentation to \code{glm}
#' @param mustart See corresponding documentation to \code{glm}
#' @param offset See corresponding documentation to \code{glm}
#' @param control See corresponding documentation to \code{glm}
#' @param method See corresponding documentation to \code{glm}
## #' @param x See corresponding documentation to \code{glm}
## #' @param y See corresponding documentation to \code{glm}
#' @param contrasts See corresponding documentation to \code{glm}
#' 
#' @param id a vector which identifies the clusters.  The length of
#'     `id' should be the same as the number of observations.  Data
#'     are assumed to be sorted so that observations on each cluster
#'     appear as contiguous rows in data. If data is not sorted this
#'     way, the function will not identify the clusters correctly. If
#'     data is not sorted this way, a warning will be issued. Please
#'     consult the package vignette for details.
#' 
#' @param waves Wariable specifying the ordering of repeated
#'     mesurements on the same unit.  Also used in connection with
#'     missing values. Please consult the package vignette for details.
#'
#' @param zcor Used for entering a user defined working correlation
#'     structure.
#' @param corstr a character string specifying the correlation
#'     structure. The following are permitted: '"independence"',
#'     '"exchangeable"', '"ar1"', '"unstructured"' and '"userdefined"'
#' @param scale.fix a logical variable; if true, the scale parameter
#'     is fixed at the value of 'scale.value'.
#' @param scale.value numeric variable giving the value to which the
#'     scale parameter should be fixed; used only if 'scale.fix =
#'     TRUE'.
#' @param std.err Type of standard error to be calculated. Defualt
#'     'san.se' is the usual robust estimate.  Other options are
#'     'jack': if approximate jackknife variance estimate should be
#'     computed.  'j1s': if 1-step jackknife variance estimate should
#'     be computed.  'fij': logical indicating if fully iterated
#'     jackknife variance estimate should be computed.
#' @param \dots further arguments passed to or from other methods.
#' @return An object of type 'geeglm'
#'
#' @note See the documentation for the 'geese' function for additional
#'     information. geeglm only works for complete data. Thus if there are NA's
#'     in data you can specify data=na.omit(mydata).
#'
#' @section Warning : Use "unstructured" correlation structure only with great
#'     care. (It may cause R to crash).
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' 
#' @seealso \code{\link{geese}}, \code{\link{glm}}, \code{\link{anova.geeglm}}
#' 
#' @references
#'
#' Halekoh, U.; Højsgaard, S. and Yan, J (2006)
#' The R Package geepack for Generalized Estimating Equations.
#' Journal of Statistical Software, 15, 2, 1-11"
#' 
#' Liang, K.Y. and Zeger, S.L. (1986) Longitudinal data analysis using
#' generalized linear models. Biometrika, *73* 13-22.
#' 
#' Prentice, R.L. and Zhao, L.P. (1991). Estimating equations for
#' parameters in means and covariances of multivariate discrete and
#' continuous responses.  Biometrics, *47* 825-839.
#' 
#' @keywords models
#' @examples
#' 
#' data(dietox)
#' dietox$Cu     <- as.factor(dietox$Cu)
#' mf <- formula(Weight ~ Cu * (Time + I(Time^2) + I(Time^3)))
#' gee1 <- geeglm(mf, data=dietox, id=Pig, family=poisson("identity"), corstr="ar1")
#' gee1
#' coef(gee1)
#' vcov(gee1)
#' summary(gee1)
#' coef(summary(gee1))
#' 
#' mf2 <- formula(Weight ~ Cu * Time + I(Time^2) + I(Time^3))
#' gee2 <- geeglm(mf2, data=dietox, id=Pig, family=poisson("identity"), corstr="ar1")
#' anova(gee2)
#'
#' # Notice the difference here: Clusters of observations must
#' # appear as chunks in data. 
#'
#' set.seed(1)
#' chick1 <- ChickWeight
#' chick2 <- chick1[sample(nrow(chick1)),]
#' chick3 <- chick2[order(chick2$Chick),]
#'
#' fit1 <- geeglm(weight~Time, id=Chick, data=chick1)
#' fit2 <- geeglm(weight~Time, id=Chick, data=chick2)
#' fit3 <- geeglm(weight~Time, id=Chick, data=chick3)
#' 
#' vcov(fit1)
#' vcov(fit2)
#' vcov(fit3)
#'
#'
#' 
#' @export geeglm
geeglm<- function (formula, family = gaussian, data = parent.frame(), 
                   weights, subset, na.action, start = NULL, etastart, mustart, 
                   offset, control = geese.control(...), method = "glm.fit", 
                   ##x = FALSE, y = TRUE,
                   contrasts = NULL, id, waves = NULL, 
                   zcor = NULL, corstr = "independence", scale.fix = FALSE, 
                   scale.value = 1, std.err = "san.se", ...) 
{

  ## Standard errors

  STDERRS <- c("san.se", "jack", "j1s", "fij")

  stderrv <- pmatch(std.err, STDERRS, -1)
  std.err <- STDERRS[stderrv]
  jackB <- j1sB <- fijB <- FALSE

  if (std.err == "jack")  jackB <- TRUE
  if (std.err == "j1s")   j1sB  <- TRUE
  if (std.err == "fij")   fijB  <- TRUE
  
  control$jack <- as.integer(jackB)
  control$j1s  <- as.integer(j1sB)
  control$fij  <- as.integer(fijB)

  if (corstr=="fixed" && is.null(zcor)){
    stop("When corstr is 'fixed' then 'zcor' must be given\n")
  }

  ## Working correlation structures

  CORSTRS <- c("independence", "exchangeable", "ar1", "unstructured", 
    "userdefined", "fixed")
  
  corstrv <- pmatch(corstr, CORSTRS, -1)
  corstr  <- CORSTRS[corstrv]

  ## Dummy glm object
  
  call <- match.call(expand.dots = TRUE)

  glmcall <- call
  glmcall$id <- glmcall$jack <- glmcall$control <- glmcall$corstr <-
    glmcall$waves <- glmcall$zcor <- glmcall$std.err <-
      glmcall$scale.fix <- glmcall$scale.value <- NULL
  glmcall[[1]] <- as.name("glm")
  glmFit <- eval(glmcall, parent.frame())
  

  
  
  modelmat <- model.matrix(glmFit)
  qqrr     <- qr(modelmat)
  if (qqrr$rank < ncol(modelmat)){
    print(head(modelmat))
    stop("Model matrix is rank deficient; geeglm can not proceed\n")
  }
  
  mf <- call
  mf[[1]] <- as.name("model.frame")
  
  mftmp <- mf
  to_delete <- c("family", "corstr", "control", "zcor", "std.err", "scale.fix")
  mftmp[match(to_delete, names(mftmp))] <- NULL
  
  ## mftmp$family <- mftmp$corstr <- mftmp$control <- mftmp$zcor <- mftmp$std.err <- NULL    
  ## mftmp$scale.fix <- NULL
  
  mf <- eval(mftmp, parent.frame())

  
  ## Clustering variable
  id <- model.extract(mf, id)
  if (is.null(id)) stop("id variable not found.")
  
  ## Waves
  waves <- model.extract(mf, waves)
  if (!is.null(waves)) waves <- as.factor(waves)
  
  ## Construct X and Y
  
  mt <- attr(mf, "terms")
  Y  <- model.response(mf, "numeric")
  X  <- if (!is.empty.model(mt)) 
          model.matrix(mt, mf, contrasts)
  else matrix(, NROW(Y), 0)
    
  ## Check that factors in model do not have unused levels in data 
  ## (otherwise R crashes).
  vars <- all.vars(formula)
  stopIt <- FALSE
  for(ii in seq_along(vars)){
    vv <- vars[ii]
    if(!is.na(match(vv, names(mf))) && is.factor(mf[,vv])){
      if (length(unique(mf[,vv])) != length(levels(mf[,vv]))){
        cat("Factors not allowed to have unused levels...\n")
        cat(" Levels of factor", vv,":", paste(levels(mf[, vv]), sep=' '),"\n")  
        cat(" Used levels of factor", vv,":", paste(unique(mf[, vv]), sep=' '),"\n")
        stopIt <- TRUE
      }
    }
    }

  if (stopIt)
    stop("Can not continue...\n")
  


  
  N <- NROW(Y)
  yy <- Y
  xx <- X
  soffset <- rep(0, N)
  mnames <- c("", "formula", "data", "offset", "weights", "subset", 
    "na.action")
  cnames <- names(call)
  cnames <- cnames[match(mnames, cnames, 0)]
  mcall  <- call[cnames]
  mcall$drop.unused.levels <- TRUE
  mcall[[1]] <- as.name("model.frame")
  mcall$formula <- formula
  sformula <- ~1
  mcall$formula[3] <- switch(match(length(sformula), c(0, 2, 
    3)), 1, sformula[2], sformula[3])
  m <- eval(mcall, parent.frame())
  terms <- attr(m, "terms")
  zsca <- model.matrix(terms, m, contrasts)
  colnames(zsca) <- c("(Intercept)")
  
  w <- model.weights(mf)
  if (is.null(w)) w <- rep(1, N)
  
  offset <- model.offset(mf)
  if (is.null(offset)) offset <- rep(0, N)
  
  if (glmFit$family$family == "binomial") {
    if (is.matrix(yy) && ncol(yy) == 2) {
      w <- apply(yy, 1, sum)
      yy <- yy[, 1] / w
    }
  }
  
  family <- glmFit$family
  nacoef <- as.numeric(which(is.na(glmFit$coef)))
  xx <- as.data.frame(xx)
  xx[, nacoef] <- NULL
  xx <- as.matrix(xx)
  
  if (is.null(start)) 
    start <- glmFit$coef
   
  ans <- geese.fit(xx, yy, id, offset, soffset, w, waves = waves, 
    zsca, zcor = zcor, corp = NULL, control = control, b = start, 
    alpha = NULL, gm = NULL, family, mean.link = NULL, variance = NULL, 
    cor.link = "identity", sca.link = "identity", link.same = TRUE, 
    scale.fix = scale.fix, scale.value = scale.value, corstr, 
    ...)
  ans <- c(ans, list(call = call, formula = formula))
  class(ans)  <- "geese"
  ans$X       <- xx
  ans$id      <- id
  ans$weights <- w


  out       <- glmFit  ## glmFit: Dummy object created in the beginning
  toDelete    <- c("R", "deviance", "aic", "null.deviance", "iter", 
    "df.null", "converged", "boundary")
  out[match(toDelete, names(out))] <- NULL
  
  out$method       <- "geese.fit"
  out$geese        <- ans
  out$weights      <- ans$weights
  out$coefficients <- ans$beta
  out$offset       <- offset
  
  if (is.null(out$offset)){
    out$linear.predictors <- ans$X %*% ans$beta
  } else {
    out$linear.predictors <- out$offset + ans$X %*% ans$beta
  }
  
  out$fitted.values <- family(out)$linkinv(out$linear.predictors)
  out$modelInfo <- ans$model
  out$id        <- ans$id
  out$call      <- ans$call
  out$corstr    <- ans$model$corstr
  out$cor.link  <- ans$model$cor.link
  out$control   <- ans$control
  out$std.err   <- std.err
  class(out)    <- c("geeglm", "gee", "glm", "lm")
  out
}

#' @export
vcov.geeglm <- function(object, ...){
    out <- switch(object$std.err,
                  'jack'={object$geese$vbeta.ajs},
                  'j1s' ={object$geese$vbeta.j1s},
                  'fij' ={object$geese$vbeta.fij},
                  object$geese$vbeta
                  )
    pn <- names(coef(object))
    dimnames(out) <- list(pn, pn)
    out
}

#' @export
summary.geeglm <- function(object,...){

    class(object) <- "glm"
    value <- summary.glm(object)
    class(object) <- c("geeglm","glm")
    class(value) <- "summary.geeglm"
    toDelete <- c("deviance", "aic", "null.deviance", "iter", "df.residual",
                  "df.null", "converged", "boundary")
    idx <- match(toDelete, names(value))
    value[idx]  <- NULL
    
    covmat <- 
        switch(object$std.err,
               'jack'={object$geese$vbeta.ajs},
               'j1s' ={object$geese$vbeta.j1s},
               'fij' ={object$geese$vbeta.fij},
               object$geese$vbeta
               )
    value$cov.scaled   <-   value$cov.unscaled <-  covmat
    
    mean.sum  <- data.frame(estimate = object$geese$beta, std.err=sqrt(diag(covmat)))
    mean.sum$wald <- (mean.sum$estimate / mean.sum$std.err)^2
    mean.sum$p <- 1 - pchisq(mean.sum$wald, df=1)
    ## names(mean.sum) <- c("Estimate", "Std.err", "Wald", "p(>W)")
    names(mean.sum) <- c("Estimate", "Std.err", "Wald", "Pr(>|W|)") ## Thanks, Achim
    
    value$coefficients <- mean.sum
    
    covmatgam <- 
        switch(object$std.err,
               'jack'={object$geese$vgamma.ajs},
               'j1s' ={object$geese$vgamma.j1s},
               'fij' ={object$geese$vgamma.fij},
               object$geese$vgamma
               )
    
    scale.sum  <- data.frame(Estimate = object$geese$gamma, Std.err=sqrt(diag(covmatgam)))
    ##scale.sum$wald <- (scale.sum$Estimate / scale.sum$Std.err)^2
    ##scale.sum$p <- 1 - pchisq(scale.sum$wald, df=1)
    
    if (!is.null(object$geese$zsca.names)) rownames(scale.sum) <- object$geese$zsca.names
    value$dispersion   <-  scale.sum
    
    covmatalpha <- 
        switch(object$std.err,
               'jack'={object$geese$valpha.ajs},
               'j1s' ={object$geese$valpha.j1s},
               'fij' ={object$geese$valpha.fij},
               object$geese$valpha
               )
    
    corr.sum <- data.frame(Estimate = object$geese$alpha, Std.err=sqrt(diag(covmatalpha)))
    ##corr.sum$wald <- (corr.sum$Estimate / corr.sum$Std.err)^2
    ##corr.sum$p <- 1 - pchisq(corr.sum$wald, df=1)
    ##if (nrow(corr.sum) > 0) rownames(corr.sum) <- object$geese$zcor.names
    value$corr <- corr.sum
    
    value$corstr    <- object$geese$model$corstr
    value$scale.fix <- object$geese$model$scale.fix
    value$cor.link  <- object$geese$model$cor.link

    v1 <- summary.geese(object$geese)
    
    value$clusz <- v1$clusz
    value$error <- object$geese$error
    value$geese <- v1
    return(value)
}

#  colnames(mean.sum) <- c("Estimate","Std.Error","ajs.SE","j1s.SE","fij.SE",
#                          "Wald","Pr(>|z|)")

#' @export
print.summary.geeglm <- function (x,
         digits = max(3, getOption("digits") - 3),
         quote = FALSE, prefix = "", ...) # Thanks, Achim...

#print.summary.geeglm <- function (x, digits = NULL, quote = FALSE, prefix = "", ...) 
{
    if (is.null(digits)) 
        digits <- options()$digits
    else options(digits = digits)
    cat("\nCall:\n");   print(x$call)
    cat("\n Coefficients:\n");
    ##print(as.matrix(x$coef), digits = digits)
    printCoefmat(as.matrix(x$coef), digits = digits) ## Thanks, Achim
    
    cat("\nCorrelation structure =", x$corstr, "\n")
    if (!(x$scale.fix)) {
        cat("Estimated Scale Parameters:\n\n")
        print(x$dispersion[1:2], digits = digits)
    }
    else cat("Scale is fixed.\n\n")
    
    if (pmatch(x$corstr, "independence", 0) == 0) {
        cat("  Link =", x$cor.link, "\n")
        cat("\nEstimated Correlation Parameters:\n")
        print(x$corr, digits = digits)
    }
    
    cat("Number of clusters:  ", length(x$clusz),
        " Maximum cluster size:", max(x$clusz), "\n")
                                        #cat("\nReturned Error Value:    ")
                                        #cat(x$error, "\n")
    invisible(x)
}


#' @export
print.geeglm <- function (x, digits = NULL, quote = FALSE, prefix = "", ...) 
{

    xg <- x$geese
    if (is.null(digits))
        digits <- options()$digits
    else options(digits = digits)

    
    cat("\nCall:\n")
    print(x$call)
    cat("\nCoefficients:\n")
    print(unclass(x$coefficients), digits = digits)
    
    cat("\nDegrees of Freedom:", length(x$y), "Total (i.e. Null); ", 
        x$df.residual, "Residual\n")
    
    
    
    if (!xg$model$scale.fix) {
        cat("\nScale Link:                  ", xg$model$sca.link)
        cat("\nEstimated Scale Parameters:  ")
        print(as.numeric(unclass(xg$gamma)), digits = digits)
    }
    else cat("\nScale is fixed.\n")
    
    cat("\nCorrelation:  Structure =",xg$model$corstr, " ")
    if (pmatch(xg$model$corstr, "independence", 0) == 0) {
        cat("  Link =", xg$model$cor.link, "\n")
        cat("Estimated Correlation Parameters:\n")
        print(unclass(xg$alpha), digits = digits)
    }
    cat("\nNumber of clusters:  ", length(xg$clusz), "  Maximum cluster size:", 
        max(xg$clusz), "\n\n")
    invisible(x)
}


#' @export
residuals.geeglm <- function (object, type = c("pearson", "working", "response"), ...) 
{
    type <- match.arg(type)
    y   <- object$y
    r   <- object$residuals
    mu  <- object$fitted.values
    wts <- object$prior.weights
    res <- switch(type,
#                   deviance = if (object$df.res > 0) {
#                     d.res <- sqrt(pmax((object$family$dev.resids)(y, mu, 
#                                                                   wts), 0))
#                     ifelse(y > mu, d.res, -d.res)
#                   }
#                   else rep.int(0, length(mu)),
                  
                  pearson = (y - mu) * sqrt(wts)/sqrt(object$family$variance(mu)), 
                  working = r,
                  response = y - mu,
                  partial = r)
    if (!is.null(object$na.action)) 
      res <- naresid(object$na.action, res)
#    if (type == "partial") 
#      res <- res + predict(object, type = "terms")
    res
}

#' @export
plot.geeglm <- function(x,...){
  xx <- fitted(x)
  rp <- residuals(x, "pearson")
  plot(xx, rp, ylab="Pearson residuals", xlab="Fitted values")
  abline(h = 0)
  m <- lowess(rp ~ xx)
  lines(m)
  
}


eprint <- function(x){
  #print(x)
}



# geeglm <- function (formula, family = gaussian, data=parent.frame(), weights, subset, 
#                   na.action, start = NULL, etastart, mustart, offset,
#                   control = geese.control(...), 
#                   method = "glm.fit", x = FALSE, y = TRUE,
#                   contrasts = NULL, 
#                   id, waves = NULL,
# 		  zcor=NULL,
#                   corstr = "independence",
#                   scale.fix = FALSE,
#                   scale.value =1,
#                   std.err = 'san.se',  
#                     ...) 
# {


#   STDERRS <- c("san.se", "jack", "j1s", "fij")
#   stderrv <- pmatch(std.err, STDERRS, -1)
#   std.err <- STDERRS[stderrv]

#   jackB <- j1sB <- fijB <- FALSE

#   if (std.err=='jack') jackB <- TRUE
#   if (std.err=='j1s')  j1sB  <- TRUE
#   if (std.err=='fij')  fijB  <- TRUE

#   control$jack <- as.integer(jackB)
#   control$j1s  <- as.integer(j1sB)
#   control$fij  <- as.integer(fijB)
  
#   CORSTRS <- c("independence", "exchangeable", "ar1", "unstructured", "userdefined")
#   eprint("SHDgeese.fit - corstr")
#   corstrv <- pmatch(corstr, CORSTRS, -1)
#   corstr<-CORSTRS[corstrv]

#   eprint("geeglm is called")
#   call <- match.call(expand.dots=TRUE)

#   glmcall <- call
#   glmcall$id <- glmcall$jack <- glmcall$control <- glmcall$corstr <- glmcall$waves <- glmcall$zcor<- glmcall$std.err <- glmcall$scale.fix <- glmcall$scale.value <- NULL


#   glmcall[[1]]  <- as.name("glm")
#   glmFit <- eval(glmcall, parent.frame())

#   mf <- call
#   ##call$data <- mf$data <- na.omit(eval(mf$data))
  
#   mf[[1]] <- as.name("model.frame")
#   mftmp <- mf
#   mftmp$family <- mftmp$corstr <- mftmp$control  <-   mftmp$zcor<- mftmp$std.err <- NULL
#   mf <- eval(mftmp, parent.frame())


# ### Copy from "geese" starts here
# #################################
#   id <- model.extract(mf, id)
#   if (is.null(id)) 
#     stop("id variable not found.")
  
#   waves <- model.extract(mf, waves)
#   if (!is.null(waves))		
#     waves <- as.factor(waves)
  
#   mt <- attr(mf, "terms")
#   Y <- model.response(mf, "numeric")
#   X <- if (!is.empty.model(mt)) 
#     model.matrix(mt, mf, contrasts)
#   else matrix(, NROW(Y), 0)
  
#   N <- NROW(Y)

#   yy <- Y
#   xx <- X

#   soffset <- rep(0, N)
  
#   mnames <- c("", "formula", "data", "offset", "weights", "subset", "na.action")
#   cnames <- names(call)
#   cnames <- cnames[match(mnames, cnames, 0)]
#   mcall <- call[cnames]
#   mcall$drop.unused.levels <- TRUE
#   mcall[[1]] <- as.name("model.frame")
  
#   mcall$formula <- formula
#   sformula <- ~1
#   mcall$formula[3] <-
#     switch(match(length(sformula),
#                  c(0, 2, 3)), 1, sformula[2], sformula[3])
#   m <- eval(mcall, parent.frame())
#   terms <- attr(m, "terms")
#   zsca <- model.matrix(terms, m, contrasts)

#   colnames(zsca) <- c("(Intercept)")
#                                         #corstr <- "independence"
#   w <- model.weights(mf)
#   if (is.null(w)) 
#     w <- rep(1, N)
  
#   offset <- model.offset(mf)
#   if (is.null(offset)) 
#     offset <- rep(0, N)
  
#   if (glmFit$family$family=="binomial"){
#     if (is.matrix(yy) && ncol(yy)==2){
#       w <- apply(yy,1,sum)
#       yy<- yy[,1]/w
#     }
#   }
  
#   family <- glmFit$family
#   nacoef <- as.numeric(which(is.na(glmFit$coef)))
#   xx <- as.data.frame(xx)
#   xx[,nacoef] <- NULL
#   xx <- as.matrix(xx)
#   if (is.null(start))
#     start <- glmFit$coef
  
#   ans <- geese.fit(xx, yy, id, offset, soffset, w, waves=waves, zsca, 
#                    zcor=zcor, corp=NULL, control=control, 
#                    b=start,
#                    alpha=NULL, gm=NULL, family, mean.link=NULL, 
#                    variance=NULL,
#                    cor.link="identity", sca.link="identity",
#                    link.same=TRUE, scale.fix=scale.fix, scale.value=scale.value, 
#                    corstr, ...)
#   ans <- c(ans, list(call = call, formula = formula))
#   class(ans) <- "geese"
# ### Copy from geese ends here
# #############################    

#   ans$X <- xx
#   ans$id <- id
#   ans$weights <- w
  
  
#   value <- glmFit
#   toDelete <- c("R","deviance","aic","null.deviance","iter","df.null",
#                 "converged","boundary")
#   value[match(toDelete,names(value))] <- NULL
  
#   value$method <- "geese.fit"
#   value$geese             <- ans
#   value$weights           <- ans$weights
#   value$coefficients      <- ans$beta
    
#   ## Kludgy..
#   value$offset <- offset
#   if(is.null(value$offset))
#     value$linear.predictors <- ans$X %*% ans$beta
#   else
#     value$linear.predictors <- value$offset + ans$X %*% ans$beta
  
#   value$fitted.values     <- family(value)$linkinv(value$linear.predictors)
#   value$modelInfo         <- ans$model
#   value$id                <- ans$id
#   value$call              <- ans$call
#   value$corstr <- ans$model$corstr
#   value$cor.link <- ans$model$cor.link
#   value$control <- ans$control
#   value$std.err <- std.err
#   class(value)            <- c("geeglm", "gee", "glm")
#   return(value)
# }











