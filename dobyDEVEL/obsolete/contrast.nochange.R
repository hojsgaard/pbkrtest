
# This function replaces the function of the same name defined
# in the Design package, which had a bug, making gendata unusable
# on non-Design objects
gendata.default <- function(fit, factors, ..., env=NULL)
{
   tt <- tryCatch(terms(fit), error=function(e) terms(formula(fit, env=env)))
   order <- attr(tt, 'order')
   tlabs <- attr(tt, 'term.labels')
   nam <- tlabs[order == 1]
   fnam <- names(factors)
   nf <- length(factors)

   if (nf == 0)
      stop('illegal factors argument')

   wh <- charmatch(fnam, nam, 0)
   if (any(wh == 0))
      stop(paste("factor(s) not in design:", paste(names(factors)[wh == 0], collapse=" ")))

   if (nf < length(nam))
      stop('not enough factors')

   settings <- list()
   for (i in 1:nf)
      settings[[fnam[i]]] <- factors[[i]]

   expand.grid(settings)
}





contrastCalc <- function(fit, a, b, cnames=NULL,
                         type=c('individual', 'average'),
                         weights='equal', conf.int=0.95, 
                         fcType = "simple",
                         fcFunc = I,
                         covType = NULL,
                         ..., 
                         env=parent.frame(2))
{
  type <- match.arg(type)
  idf <- fit$df.residual
  critVal <- if (length(idf) > 0)
    qt((1 + conf.int) / 2, idf)
  else
    qnorm((1 + conf.int) / 2)

  da <- do.call('gendata', list(fit=fit, factors=a, env=env))
  xa <- predictFrame(fit, da, env=env)
  ma <- nrow(xa)

  if (missing(b))
    {
      xb <- 0 * xa
      db <- da
    } else {
      db <- do.call('gendata', list(fit, factors=b, env=env))
      xb <- predictFrame(fit, db, env=env)
    }
  mb <- nrow(xb)


  vary <- NULL
  if (type == 'individual' && length(cnames) == 0)
    {
      ## If two lists have same length, label contrasts by any variable
      ## that has the same length and values in both lists
      if (ma == mb)
        {
          if (ncol(da) != ncol(db)) stop('program logic error')
          if (any(sort(names(da)) != sort(names(db)))) stop('program logic error')
          k <- integer(0)
          nam <- names(da)
          for (j in 1:length(da))
            {
              if (all(as.character(da[[nam[j]]]) == as.character(db[[nam[j]]])))
                k <- c(k, j)
            }
          if (length(k) > 0) vary <- da[k]
        } else if (max(ma, mb) > 1) {
          ## Label contrasts by values of longest variable in list if
          ## it has the same length as the expanded design matrix
          d <- if (ma > 1) a else b
          l <- sapply(d, length)
          vary <- if (sum(l == max(ma, mb)) == 1) d[l == max(ma, mb)]
        }
    }

  if (max(ma, mb) > 1 && min(ma, mb) == 1)
    {
      if (ma == 1)
        xa <- matrix(xa, nrow=mb, ncol=ncol(xb), byrow=TRUE)
      else
        xb <- matrix(xb, nrow=ma, ncol=ncol(xa), byrow=TRUE)
    } else if (mb != ma) {
      stop('number of rows must be the same for observations generated\nby a and b unless one has one observation')
    }

  X <- xa - xb
  p <- ncol(X)
  m <- nrow(X)

  modelCoef <- getCoefficients(fit)
  denom <- xb %*% modelCoef 
  numer <- xa %*% modelCoef 
  ratio <- fcFunc(numer) / fcFunc(denom) 
  fc <- switch(
               fcType,
               simple = ratio,
               log = log(ratio),
               signed = ifelse(ratio > 1, ratio, -1/ratio))
  
  if (is.character(weights))
    {
      if (weights!='equal') stop('weights must be "equal" or a numeric vector')
      weights <- rep(1, m)
    } else if (length(weights) > 1 && type == 'individual') {
      stop('can specify more than one weight only for type="average"')
    } else if (length(weights) != m) {
      stop(paste('there must be', m, 'weights'))
    }
  weights <- as.vector(weights)

  if (m > 1 && type == 'average')
    X <- matrix(apply(weights * X, 2, sum) / sum(weights), nrow=1,
                dimnames=list(NULL, dimnames(X)[[2]]))

  if(class(fit)[1] == "lm")
    {
      library(sandwich)
      if(is.null(covType)) covType <- "const"
      covMat <- vcovHC(fit, type = covType)
    } else covMat <- vcov(fit)
  
  res <- testStatistic(fit, X, critVal, modelCoef, covMat)
  res$df.residual <- idf
  res$cnames <- if (type == 'average') NULL else cnames
  res$nvary <- length(vary)
  res$foldChange <- fc
  res$aCoef <- xa
  res$bCoef <- xb
  res$model <- class(fit)[1]
  res$covType <- covType
  if (type == 'individual') res <- c(vary, res)
  structure(res, class='contrast')
}




# This function is used by contrast.lm instead of predictDesign,
# which only works on Design objects.
predictFrameSH <- function(object, newdata, env, na.action=na.fail)
{
   tt <- tryCatch(terms(object), error=function(e) terms(formula(object, env=env)))
   Terms <- delete.response(tt)

   if (is.null(object$xlevels))
   {
      mf <- model.frame(object, env=env)
      xlev <- .getXlevels(Terms, mf)
   } else {
      xlev <- object$xlevels
   }

   print(newdata)
   
   m <- model.frame(Terms, newdata, na.action=na.action, xlev=xlev)
   if (!is.null(cl <- attr(Terms, "dataClasses")))
      .checkMFClasses(cl, m)

   if (is.null(object$contrasts))
   {
      # this is the way gls does it.  is this appropriate for geese?
      contr <- lapply(m, function(el) if (inherits(el, "factor")) contrasts(el))
      contr <- contr[! unlist(lapply(contr, is.null))]
   } else {
      contr = object$contrasts
   }

   model.matrix(Terms, m, contrasts=contr)
}

