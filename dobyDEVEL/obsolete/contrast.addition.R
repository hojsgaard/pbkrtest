## #######################################################
##
## Also need contrasts for geeglm objects (from geepack)
##
## #######################################################

contrast.geeglm <- function(fit, ...)
{
  library(geepack)
  contrastCalc(fit, ...)
}

getCoefficients.geeglm <- function(fit, ...){
  library(geepack)
  coef(fit)
}


## #######################################################
##
## Calculate contrasts for mer objects (from lme4).  
##
## #######################################################

getCoefficients <- function(fit, ...) UseMethod("getCoefficients")

getCoefficients.mer <- function(fit, ...){
  library(lme4)
  lme4:::fixef(fit)
}

contrast.mer   <- function(fit, ...){
  contrastCalc_mer(fit, ...)
}

contrastCalc_mer<- function(fit, a, b, cnames=NULL,
                         type=c('individual', 'average'),
                         weights='equal', conf.int=0.95, 
                         fcType = "simple",
                         fcFunc = I,
                         covType = NULL,
                         ..., 
                         env=parent.frame(2))
{
  type <- match.arg(type)

  ## Addition here:
  dims<-fit@dims
  idf <- dims[["n"]]-( dims[["p"]] + dims[["np"]] + as.logical(dims[["useSc"]]))
  ## Replaces:
  ## idf <- fit$df.residual

  ## We also need the "pseudo-fit"
  fitlm <-  lm(attributes(fit@frame)$terms, data=fit@frame)

  
  critVal <- if (length(idf) > 0)
    qt((1 + conf.int) / 2, idf)
  else
    qnorm((1 + conf.int) / 2)


  da <- do.call('gendata', list(fit=fitlm, factors=a, env=env))
  ##da <- do.call('gendata', list(fit=fit, factors=a, env=env))

  xa <- predictFrame(fitlm, da, env=env)
  ##xa <- predictFrame(fit, da, env=env)
  ma <- nrow(xa)

  if (missing(b))
    {
      xb <- 0 * xa
      db <- da
    } else {
      db <- do.call('gendata', list(fitlm, factors=b, env=env))
      ##db <- do.call('gendata', list(fit, factors=b, env=env))
      xb <- predictFrame(fitlm, db, env=env)
      ##xb <- predictFrame(fit, db, env=env)
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

  covMat <- vcov(fit)
  
  res <- testStatistic_mer(fit, X, critVal, modelCoef, covMat)
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

testStatistic_mer <- function(fit, designMatrix, critVal, params=getCoefficients(fit), covMatrix=vcov(fit))
{

   est <- drop(designMatrix %*% params)
   v   <- drop((designMatrix %*% covMatrix) %*% t(designMatrix))
   ndf <- if (is.matrix(v)) nrow(v) else 1
   se  <- if (ndf == 1) sqrt(v) else sqrt(diag(v))
   testStat <- est / se

   df <- fit@dims[["n"]] -  fit@dims[["p"]] - fit@dims[["q"]] 
   P  <-  2 * (1 - pt(abs(testStat), df))

   list(Contrast=est,
        SE=se,
        Lower=est - critVal * se,
        Upper=est + critVal * se,
        testStat=testStat,
        df = df,
        Pvalue=P,
        var=v,
        X=designMatrix)
 }


## ################################################################
##
## Modified print method
##
## ################################################################

print.contrast <- function(x, X=FALSE, fun=function(u) u, ...)
{
   testLabels <- switch(
                        x$model,
                        lm =, glm =, lme =, gls = c("t", "Pr(>|t|)"),
                        ## Addition by Soren here                        
                        mer = c("t", "Pr(>|t|)"),
                        ## !end
                        geese = c("Z", "Pr(>|Z|)"))

   w <- x[c("Contrast", "SE", "Lower", "Upper", "testStat", "df", "Pvalue")]
   w$testStat <- round(w$testStat, 2)
   w$Pvalue <- round(w$Pvalue, 4)
   no <- names(w)
   no[no == 'SE'] <- 'S.E.'
   no[no == 'testStat'] <- testLabels[1]
   no[no == 'Pvalue'] <- testLabels[2]
   names(w) <- no
   
   cat(x$model, "model parameter contrast\n\n")

   cnames <- x$cnames
   if (length(cnames) == 0)
   {
      cnames <- if (x$nvary) rep('', length(x[[1]])) else as.character(1:length(x[[1]]))
   }
   attr(w, 'row.names') <- cnames
   attr(w, 'class') <- 'data.frame'
   w$Contrast <- fun(w$Contrast)
   w$SE <- fun(w$SE)
   if(x$model != "geese") w$df <- x$df
   w$Lower <- fun(w$Lower)
   w$Upper <- fun(w$Upper)
   print(as.matrix(w), quote=FALSE)
   if (X)
   {
      attr(x$X, "contrasts") <- NULL
      attr(x$X, "assign") <- NULL
      cat('\nContrast coefficients:\n')
      if (is.matrix(x$X)) dimnames(x$X) <- list(cnames, dimnames(x$X)[[2]])
      print(x$X)
   }

   if(x$model == "lm")
   {
      if(x$covType != "const") cat("\nThe", x$covType, "covariance estimator was used.\n")
    }

     
   invisible()
}



testStatistic <- function(fit, designMatrix, critVal, params=getCoefficients(fit), covMatrix=vcov(fit))
{
   est <- drop(designMatrix %*% params)
   v <- drop((designMatrix %*% covMatrix) %*% t(designMatrix))
   ndf <- if (is.matrix(v)) nrow(v) else 1
   se <- if (ndf == 1) sqrt(v) else sqrt(diag(v))
   testStat <- est / se

   # this is inconsistent theoretically, but consistent with each R model
   df <- switch(
      class(fit)[1],
      lm =, glm = fit$df.residual,
      gls = fit$dims$N -  fit$dims$p,
      lme = fit$dims$N -  length(params) - ncol(fit$apVar),      
      geese =, geeglm = NA)
   
   P <- switch(
      class(fit)[1],
      lm =, glm = 2 * (1 - pt(abs(testStat), df)),
      gls = 2 * (1 - pt(abs(testStat), df)),
      lme = 2 * (1 - pt(abs(testStat), df)),      
      geese =, geeglm= 2 * (1 - pnorm(abs(testStat))))

   list(Contrast=est,
        SE=se,
        Lower=est - critVal * se,
        Upper=est + critVal * se,
        testStat=testStat,
        df = df,
        Pvalue=P,
        var=v,
        X=designMatrix)
}
