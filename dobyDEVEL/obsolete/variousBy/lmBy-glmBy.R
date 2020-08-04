

lmBy <- function (formula, data, subset, weights, na.action, method = "qr",
    model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE,
    contrasts = NULL, offset, group, ...){

  if (missing(group)){
    stop("A grouping of data has not been specified")
  }
  mf<- match.call(expand.dots = TRUE)

  newdata <- eval(mf$data)
  newdata$.NUM. <- 1:nrow(newdata)
  mf$data <- newdata

  mf$modelClass <- 'lm'
  mf[[1]] <- as.name('fitBy')
  value <- eval(mf)
  attr(value,'data')<- newdata
  class(value) <- c('lmBy','fitBy')
  return(value)
}


glmBy <- function (formula, family = gaussian, data, weights, subset,
    na.action, start = NULL, etastart, mustart, offset, control = glm.control(...),
    model = TRUE, method = "glm.fit", x = FALSE, y = TRUE, contrasts = NULL,
    group, ...) {

  if (missing(group)){
    stop("A grouping of data has not been specified")
  }
  mf<- match.call(expand.dots = TRUE)

  newdata <- eval(mf$data)
  newdata$.NUM. <- 1:nrow(newdata)
  mf$data <- newdata

  mf$modelClass <- 'glm'
  mf[[1]] <- as.name('fitBy')
  value <- eval(mf)
  attr(value,'data')<- newdata
  class(value) <- c('glmBy','fitBy')
  return(value)
}

##
## fitBy is used for fitting both 'lm' and 'glm'
##
fitBy <- function(..., modelClass='lm'){
  mf <- match.call(expand.dots = TRUE)

  ## Split data
  v2 <- mf
  v2[[-1]]<-NULL
  v2$formula <- mf$group
  v2$data    <- mf$data
  v2[[1]]<-as.name('splitBy')
  groupData <- eval(v2)

  ## Model object
  v3 <- mf
  v3$group <- v3$modelClass <- NULL
  v3[[1]]<-as.name(modelClass)
  
  ## Fit models, one data set at the time
  lmList <- lapply(groupData,function(d){
    v3$data <- d
    eval(v3)
  })
  attr(lmList,'groupid') <- attr(groupData,'groupid')
  return(lmList)
}


##
## Returning residuals, fitted values etc.
##

fitted.fitBy <- function(object,...){
  returnByValues(object,...,returntype='fitted')
}

residuals.fitBy <- function(object,...){
  returnByValues(object,...,returntype='residuals')
}

returnByValues <- function(object,...,returntype){
  value <-lapply(1:length(object), function(i){
    cbind(
      .fitted=do.call(returntype, list(object[[i]], ...)),
      object[[i]]$call$data$.NUM.,
      attr(object,'groupid')[i,,drop=FALSE])
  })
  value <- do.call('rbind', value)
  value <- value$.fitted[order(value[,2])]
  return(value)
}
