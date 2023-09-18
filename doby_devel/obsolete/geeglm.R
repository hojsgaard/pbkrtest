
geeglm <- function (formula, family = gaussian, data=parent.frame(), weights, subset, 
                  na.action, start = NULL, etastart, mustart, offset,
                  control = geese.control(...), 
                  method = "glm.fit", x = FALSE, y = TRUE,
                  contrasts = NULL, 
                  id,
                  corstr = "independence",
                  scale.fix = FALSE,
                  scale.value =1,
                  ...) 
{
    call <- match.call(expand.dots=TRUE)

    glmcall <- call
    glmcall$id <- glmcall$jack <- glmcall$control <- glmcall$corstr <- NULL

    glmcall[[1]]  <- as.name("glm")
    glmFit <- eval(glmcall, parent.frame())
    mf <- call
    mf[[1]] <- as.name("model.frame")
    mftmp <- mf
    mftmp$family <- mftmp$corstr <- NULL
    mf <- eval(mftmp, parent.frame())

### Copy from "geese" starts here
#################################
    id <- model.extract(mf, id)
    if (is.null(id)) 
      stop("id variable not found.")

    mt <- attr(mf, "terms")
    Y <- model.response(mf, "numeric")
    X <- if (!is.empty.model(mt)) 
      model.matrix(mt, mf, contrasts)
    else matrix(, NROW(Y), 0)
    N <- NROW(Y)

    yy <- Y
    xx <- X
    
    soffset <- rep(0, N)
    
    mnames <- c("", "formula", "data", "offset", "weights", "subset", "na.action")
    cnames <- names(call)
    cnames <- cnames[match(mnames, cnames, 0)]
    mcall <- call[cnames]
    
    mcall[[1]] <- as.name("model.frame")
    mcall$formula <- formula
    sformula <- ~1
    mcall$formula[3] <-
      switch(match(length(sformula),
                   c(0, 2, 3)), 1, sformula[2], sformula[3])
    m <- eval(mcall, parent.frame())
    terms <- attr(m, "terms")
    zsca <- model.matrix(terms, m, contrasts)
    
    colnames(zsca) <- c("(Intercept)")
                                        #corstr <- "independence"
    w <- model.weights(mf)
    if (is.null(w)) 
      w <- rep(1, N)
    
    offset <- model.offset(mf)
    if (is.null(offset)) 
      offset <- rep(0, N)

    if (glmFit$family$family=="binomial"){
      if (is.matrix(yy) && ncol(yy)==2){
        w <- apply(yy,1,sum)
        yy<- yy[,1]/w
      }
    }

    family <- glmFit$family
    nacoef <- as.numeric(which(is.na(glmFit$coef)))
    xx <- as.data.frame(xx)
    xx[,nacoef] <- NULL
    xx <- as.matrix(xx)
    if (is.null(start))
      start <- glmFit$coef
    ans <- geese.fit(xx, yy, id, offset, soffset, w, waves=NULL, zsca, 
                     zcor=NULL, corp=NULL, control=geese.control(),
                     b=start,
                     alpha=NULL, gm=NULL, family, mean.link=NULL, 
                     variance=NULL,
                     cor.link="identity", sca.link="identity",
                     link.same=TRUE, scale.fix=scale.fix, scale.value=scale.value, 
                     corstr, ...)
    ans <- c(ans, list(call = call, formula = formula))
    class(ans) <- "geese"
### Copy from geese ends here
#############################    
    ans$X <- xx
    ans$id <- id
    ans$weights <- w

    value <- glmFit
    toDelete <- c("R","deviance","aic","null.deviance","iter","df.null",
                  "converged","boundary")
    value[match(toDelete,names(value))] <- NULL
  
    value$geese             <- ans
    value$weights           <- ans$weights
    value$coefficients      <- ans$beta
    value$linear.predictors <- ans$X %*% ans$beta
    value$fitted.values     <- family(value)$linkinv(value$linear.predictors)
    value$modelInfo         <- ans$model
    value$id                <- ans$id
    value$call              <- ans$call
    class(value)            <- c("geeglm", "gee", "glm")
    return(value)
  }





summary.geeglm <- function(object,...){
  v1 <- summary.geese(object$geese)
  class(object) <- "glm"
  value <- summary.glm(object)
  class(object) <- c("geeglm","glm")
  class(value) <- "summary.geeglm"
  toDelete <- c("deviance","aic","null.deviance","iter","df.residual","df.null",
                "converged","boundary")
  idx <- match(toDelete,names(value))
  value[idx]  <- NULL

  value$coefficients <- v1$mean
  value$dispersion   <-  v1$scale  
  value$cov.scaled   <-  object$geese$vbeta
  value$cov.unscaled <-  object$geese$vbeta
  value$corstr    <- object$geese$model$corstr
  value$scale.fix <- object$geese$model$scale.fix
  value$cor.link  <-  object$geese$model$cor.link
  value$corr <- v1$corr
  value$clusz <- v1$clusz
  value$error <- object$geese$error
  value$geese <- v1
  return(value)
}


#  colnames(mean.sum) <- c("Estimate","Std.Error","ajs.SE","j1s.SE","fij.SE",
#                          "Wald","Pr(>|z|)")


print.summary.geeglm <- function (x, digits = NULL, quote = FALSE, prefix = "", ...) 
{
  if (is.null(digits)) 
    digits <- options()$digits
  else options(digits = digits)
  cat("\nCall:\n");   print(x$call)
  cat("\n Coefficients:\n");
  print(as.matrix(x$coef), digits = digits)
  if (x$scale.fix == FALSE) {
    cat("\n Estimated Scale Parameters:\n")
    print(x$dispersion, digits = digits)
  }
  else cat("\nScale is fixed.\n")
  cat("\nCorrelation Structure:    ", x$corstr, "\n")
  if (pmatch(x$corstr, "independence", 0) == 0) {
    ##cat(" Correlation Link:         ", x$cor.link, "\n")
    cat("\nEstimated Correlation Parameters:\n")
    print(x$corr, digits = digits)
  }
  
  cat("Number of clusters:  ", length(x$clusz), "  Maximum cluster size:", 
      max(x$clusz), "\n")
  #cat("\nReturned Error Value:    ")
  #cat(x$error, "\n")
  invisible(x)
}


print.geeglm <- function (x, digits = NULL, quote = FALSE, prefix = "", ...) 
{
  xg <- x$geese
  if (is.null(digits)) 
    digits <- options()$digits
  else options(digits = digits)
  cat("\nCall: ", deparse(x$call), "\n")
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
  
  cat("\nCorrelation Structure:    ", xg$model$corstr, "\n")
  if (pmatch(xg$model$corstr, "independence", 0) == 0) {
    cat(" Correlation Link:         ", xg$model$cor.link, "\n")
    cat("\n Estimated Correlation Parameters:\n")
    print(unclass(xg$alpha), digits = digits)
  }
  cat("Number of clusters:  ", length(xg$clusz), "  Maximum cluster size:", 
      max(xg$clusz), "\n\n")
  invisible(x)
}


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

plot.geeglm <- function(x,...){
  xx <- fitted(x)
  rp <- residuals(x,"pearson")
  plot(xx,rp,ylab="Pearson residuals",xlab="Fitted values")
  abline(h=0)
  m <- lowess(rp ~ xx)
  lines(m)
  
}
model.matrix.geeglm <- function(object,...){
  fit <- object
 dat <- get(as.character(fit$call$data))
 mm <- model.matrix(formula(fit), dat)
 return(mm)
}





