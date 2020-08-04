## ##############################################################################################
##
## Create matrices for linear estimates
##
## Banff, august 2013
## popMatrix is a bad name
## ##############################################################################################

popMatrix <- function(object, effect=NULL, at=NULL, only.at=TRUE){
  UseMethod("popMatrix")
}

popMatrix.default <- function(object, effect=NULL, at=NULL, only.at=TRUE){
    Terms   <- delete.response( terms(object) )
    xlev    <- .get_xlevels( object ) ## gets the factor levels
    cov.ave <- .get_covariate_ave( object, at )
    vartype <- .get_vartypes( object )

    if (is.null(effect)){
        at.factor <- at[intersect(vartype$factor, names(at))]
        xxx       <- if(length(at.factor)>0)  at.factor
    } else {
        xlev    <- .set_xlevels( xlev, at=at )
        at.fact <- names(attr(xlev, "at.fact"))
        effect  <- setdiff( effect, at.fact )
        xxx     <- xlev[ c( effect, at.fact ) ]
    }

    if (is.null(xxx)){
        ##cat("No 'effect' and no 'at'; hence just a global average... \n")
        newdata <- expand.grid(xlev)
        newdata[,names(cov.ave)] <- cov.ave
        mf  <- model.frame(Terms, newdata, xlev = .get_xlevels(object))
        X   <- model.matrix(Terms, mf, contrasts.arg = .get_contrasts(object))

        res <- apply(X, 2, mean)
        res <- do.call(rbind, list(res))
        attr(res,"at") <- at[intersect(vartype$numeric, names(at))]
    } else {
        ##cat("The general case...\n")
        grid.data  <- expand.grid(xxx)
        grid.data  <- as.data.frame(lapply(grid.data, as.character), stringsAsFactors=FALSE)

        mmlist <- list()
        for (ii in 1:nrow(grid.data)){
            conf    <- grid.data[ ii, ,drop=FALSE ]
            xlev2   <- .set_xlevels(xlev,  at=conf) #cat("xlev2 (which defines the grid):\n"); str(xlev2)
            newdata <- expand.grid( xlev2 )
            newdata[, names(cov.ave)]  <- cov.ave
            mm             <- .getX(object, newdata)
            mmlist[[ ii ]] <- mm
        }

        grid.data[, names(cov.ave) ] <- cov.ave
        res                <-lapply( mmlist, function( mm ) apply( mm, 2, mean ) )
        res                <- do.call(rbind, res)
        attr(res,"grid")   <- grid.data
        attr(res,"at")     <- at
    }

    class(res) <- c("popMatrix", "conMatrix", "matrix")
    res
}

print.conMatrix <- function(x,...){
  prmatrix(x)
  atr <- attributes(x)[c("at","grid")]
  aa <- !unlist(lapply(atr, is.null))
  str(atr[aa])
  invisible(x)
}

## summary.conMatrix <- function(object, ...){
## 	print(object)
## 	cat("grid:\n")
## 	str(attr(object,"grid"))
## 	cat("at:\n")
## 	str(attr(object,"at"))
## }




















## pm <- function(object, effect=NULL, at=NULL, only.at=TRUE){
##     Terms   <- delete.response( terms(object) )
##     xlev    <- .get_xlevels( object ) ## gets the factor levels
##     cov.ave <- .get_covariate_ave( object, at )
##     vartype <- .get_vartypes( object )

##     if (is.null(effect)){
##         at.factor <- at[intersect(vartype$factor, names(at))]
##         xxx       <- if(length(at.factor)>0)  at.factor
##     } else {
##         xlev    <- .set_xlevels( xlev, at=at )
##         at.fact <- names(attr(xlev, "at.fact"))
##         effect  <- setdiff( effect, at.fact )
##         xxx     <- xlev[ c( effect, at.fact ) ]
##     }

##     str(list(xxx=xxx))
##     if (is.null(xxx)){
##         ##cat("No 'effect' and no 'at'; hence just a global average... \n")
##         newdata <- expand.grid(xlev)
##         newdata[,names(cov.ave)] <- cov.ave
##         mf  <- model.frame(Terms, newdata, xlev = .get_xlevels(object))
##         X   <- model.matrix(Terms, mf, contrasts.arg = .get_contrasts(object))

##         res <- apply(X, 2, mean)
##         res <- do.call(rbind, list(res))
##         attr(res,"at") <- at[intersect(vartype$numeric, names(at))]
##     } else {
##         cat("The general case...\n")
##         grid.data  <- expand.grid(xxx)
##         grid.data  <- as.data.frame(lapply(grid.data, as.character), stringsAsFactors=FALSE)
##         cat("grid.data:\n"); print(grid.data)
##         mmlist <- list()
##         for (ii in 1:nrow(grid.data)){
##             conf    <- grid.data[ ii, ,drop=FALSE ]
##             xlev2   <- .set_xlevels(xlev,  at=conf) #cat("xlev2 (which defines the grid):\n"); str(xlev2)
##             newdata <- expand.grid( xlev2 )
##             newdata[, names(cov.ave)]  <- cov.ave
##             mm             <- .getX(object, newdata)
##             mmlist[[ ii ]] <- mm
##         }

##         grid.data[, names(cov.ave) ] <- cov.ave
##         res                <-lapply( mmlist, function( mm ) apply( mm, 2, mean ) )
##         res                <- do.call(rbind, res)
##         attr(res,"grid")   <- grid.data
##         attr(res,"at")     <- at
##     }

##     class(res) <- c("popMatrix", "conMatrix", "matrix")
##     res
## }






        ## fact.lev <- .set_xlevels( fact.lev, at=at )   ## new fact.levels; factors are fixed at their 'at' levels
        ## new.fact.lev  <- fact.lev ##[ c( effect, at.fact ) ]  ##


        ## str(list(fact.lev=fact.lev, xxx=xxx))

        ## at.fact  <- names(attr(fact.lev, "at.fact"))
        ## str(list(at.fact=at.fact, fact.lev=fact.lev))
        ## effect   <- setdiff( effect, at.fact )    ## The same factor can appear twice; we need to remove the one in effect.


    ## if (is.null(effect)){
    ##     if (is.null(at)){
    ##         cat("No 'effect' and no 'at'-factors; hence just a global average... \n")
    ##     } else {
    ##         at.factors <- at[ intersect( vartype$factor, names(at) ) ]
    ##         xxx        <- if(length(at.factors)>0) at.factors
    ##     }
    ## } else {
    ##     if (is.null(at)){

    ##     } else {

    ##     }
    ## }

        ## mf  <- model.frame(trms, newdata, xlev = .get_xlevels(object))
        ## X   <- model.matrix(trms, mf, contrasts.arg = .get_contrasts(object))
