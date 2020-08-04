## ##################################################################
##
## Banff, August 2013:
##
## All this linMeans stuff is a predecessor to the popMeans function
## Doubt that any of it is needed, but keep it for now
##
## ##################################################################


## linMeans <- function(object, at=NULL, engine="esticon", grid=TRUE, ...){
##   UseMethod("linMeans")
## }

## linMeans.default <- function(object, at=NULL, engine="esticon", grid=TRUE, ...){
##   cl  <- match.call()
##   mm  <- linMatrix(object, at=at)
##   ans <- do.call(engine, list(object, mm,...))

##   xtra <- attributes(mm)[[c("grid")]]

##   if (engine=="esticon"){
##     attr(ans,"X") <- mm
##     attr(ans,"call") <- cl
##     if (grid && !is.null(xtra))
##       ans <- cbind(ans, xtra)

##     class(ans)    <- c("linMeans", "conMeans", "data.frame")
##   }
##   attributes(ans)[c("grid","at")] <- attributes(mm)[c("grid","at")]
##   ans
## }

## linMeans.lme <- function(object, at=NULL, engine="esticon", grid=TRUE, ...){
##   cl <- match.call()
##   lm.pseudo <- lm(object,data=object$data)
##   cl[[2]]   <- lm.pseudo
##   cl[[1]]   <- as.name("linMeans.default")
##   eval(cl)
## }



## linMatrix <- function(object, at){
##   tt 	   <- terms(object)
##   Terms    <- delete.response(tt)
##   xlev     <- .get_xlevels(object)
##   ccc      <- .get_covariate_ave(object, at)

##   ## popMatrix and linMatrix only differ here
##   at.grid  <- expand.grid(at)
##   at.grid  <- as.data.frame(lapply(at.grid, as.character),stringsAsFactors=FALSE)
##   ## !!!
##   #dcat("at.grid:\n"); print(at.grid)

##   res <- list()
##   for (ii in 1:nrow(at.grid)){
##     conf  <- at.grid[ii,,drop=FALSE]
##                                         #cat("conf:\n"); print(conf)
##     ## "set" factor values specified in "at.grid"
##     xlev2 <- .set_xlevels(xlev,  at=conf)
##                                         #cat("xlev2 - 1:\n"); print(xlev2)

##     newdata <- expand.grid(xlev2)
##     ## "set" covariate values specified in "at"
##     newdata[,names(ccc)] <- ccc
##     mf  <- model.frame(Terms, newdata, xlev = .get_xlevels(object))
##     X   <- model.matrix(Terms, mf, contrasts.arg = .get_contrasts(object))
##     ans <- apply(X,2,mean)
##     res[[ii]] <- ans
##   }

##   grid <- at.grid
##   grid[names(at)]  <- at
##   grid[names(ccc)] <- ccc
##   res <- do.call(rbind, res)
##   attr(res,"grid")   <- grid
##   attr(res,"at")     <- at
##   class(res) <- c("linMatrix", "conMatrix","matrix")
##   res
## }

