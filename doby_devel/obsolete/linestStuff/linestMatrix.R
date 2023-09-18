
linestMatrix <- function(object, effect=NULL, at=NULL){
  UseMethod("linestMatrix")
}


linestMatrix.default <- function(object, effect=NULL, at=NULL){
    res <- .get_linest_list( object, effect, at )
    res <- .finalize_linest_list ( res )
    class(res) <- c("linestMatrix", "matrix")
    res
}


.finalize_linest_list <- function (aa){
    res                <- lapply( aa, function( mm ) apply( mm, 2, mean ) )
    res                <- do.call(rbind, res)
    attr(res, "at")   <- attr(aa, "at")
    attr(res, "grid") <- attr(aa, "grid")
    res
}

print.linestMatrix <- function(x,...){
  prmatrix(x)
  ## atr <- attributes(x)[c("at","grid")]
  ## aa <- !unlist(lapply(atr, is.null))
  ## str(atr[aa])
  invisible(x)
}


.get_linest_list <- function(object, effect=NULL, at=NULL){
    trms     <- delete.response( terms(object) )
    fact.lev <- .get_xlevels( object )            ## factor levels
    cov.ave  <- .get_covariate_ave( object, at )  ## average of covariates (except those mentioned in 'at'
    vartype  <- .get_vartypes( object )           ## which are factors and which are numerics
    at.factor.name <- intersect( vartype$factor, names(at) )
    cov.ave.name   <- names( cov.ave )
    effect         <- setdiff( effect, at.factor.name )

    if (is.null(effect)){
        if (length( at.factor.name ) > 0){
            new.fact.lev <- at[ at.factor.name ]
        } else {
            new.fact.lev <- NULL
        }
    } else {
        new.fact.lev  <- .set_xlevels( fact.lev, at=at )
        new.fact.lev  <- new.fact.lev[c(effect, at.factor.name)]#
    }

    if (is.null(new.fact.lev)){
        ##cat("No 'effect' and no 'at'-factors; hence just a global average... \n")
        newdata <- expand.grid(fact.lev)
        newdata[, cov.ave.name] <- cov.ave

        XXlist <- list(.getX(object, newdata))
        attr(XXlist,"at")   <- at[intersect(vartype$numeric, names(at))]
        attr(XXlist,"grid") <- NULL
    } else {
        ##cat("The general case; there are 'effect' factors or 'at' factors...\n")
        grid.data <- expand.grid(new.fact.lev)
        grid.data <- as.data.frame(lapply(grid.data, as.character), stringsAsFactors=FALSE)
        XXlist    <- list()
        for (ii in 1:nrow(grid.data)){
            config    <- grid.data[ ii, ,drop=FALSE ]
            fact.lev2 <- .set_xlevels(fact.lev,  at=config)

            newdata   <- expand.grid( fact.lev2 )
            newdata[, cov.ave.name]  <- cov.ave
            XX             <- .getX(object, newdata)

            XXlist[[ ii ]] <- XX
        }

        grid.data[, names(cov.ave) ] <- cov.ave
        attr(XXlist,"at") <- at
        attr(XXlist,"grid") <- grid.data
    }
    class(XXlist) <- "linestList"
    XXlist
}

## linestMatrix2 <- linestMatrix.default <- function(object, effect=NULL, at=NULL){
##     trms     <- delete.response( terms(object) )
##     fact.lev <- .get_xlevels( object )            ## factor levels
##     cov.ave  <- .get_covariate_ave( object, at )  ## average of covariates (except those mentioned in 'at'
##     vartype  <- .get_vartypes( object )           ## which are factors and which are numerics
##     at.factor.name <- intersect( vartype$factor, names(at) )
##     cov.ave.name   <- names( cov.ave )
##     effect         <- setdiff( effect, at.factor.name )

##     if (is.null(effect)){
##         if (length( at.factor.name ) > 0){
##             new.fact.lev <- at[ at.factor.name ]
##         } else {
##             new.fact.lev <- NULL
##         }
##     } else {
##         new.fact.lev  <- .set_xlevels( fact.lev, at=at )
##         new.fact.lev  <- new.fact.lev[c(effect, at.factor.name)]#
##     }

##     if (is.null(new.fact.lev)){
##         ##cat("No 'effect' and no 'at'-factors; hence just a global average... \n")
##         newdata <- expand.grid(fact.lev)
##         newdata[, cov.ave.name] <- cov.ave
##         XXlist <- list(.getX(object, newdata))
##         res              <- lapply( XXlist, function( mm ) apply( mm, 2, mean ) )
##         res              <- do.call(rbind, res)
##         attr(res,"at")   <- at[intersect(vartype$numeric, names(at))]
##         attr(res,"grid") <- NULL
##     } else {
##         ##cat("The general case; there are 'effect' factors or 'at' factors...\n")
##         grid.data <- expand.grid(new.fact.lev)
##         grid.data <- as.data.frame(lapply(grid.data, as.character), stringsAsFactors=FALSE)
##         XXlist    <- list()
##         for (ii in 1:nrow(grid.data)){
##             config    <- grid.data[ ii, ,drop=FALSE ]
##             fact.lev2 <- .set_xlevels(fact.lev,  at=config)

##             newdata   <- expand.grid( fact.lev2 )
##             newdata[, cov.ave.name]  <- cov.ave
##             XX             <- .getX(object, newdata)

##             XXlist[[ ii ]] <- XX
##         }
##         res                <- lapply( XXlist, function( mm ) apply( mm, 2, mean ) )
##         res                <- do.call(rbind, res)
##         grid.data[, names(cov.ave) ] <- cov.ave
##         attr(res,"grid")   <- grid.data
##         attr(res,"at")     <- at
##     }
##     class(res) <- c("popMatrix", "conMatrix", "matrix")
##     res
## }


## linestMatrix.default <- function(object, effect=NULL, at=NULL){
##     trms     <- delete.response( terms(object) )
##     fact.lev <- .get_xlevels( object )            ## factor levels
##     cov.ave  <- .get_covariate_ave( object, at )  ## average of covariates (except those mentioned in 'at'
##     vartype  <- .get_vartypes( object )           ## which are factors and which are numerics
##     at.factor.name <- intersect( vartype$factor, names(at) )
##     cov.ave.name   <- names( cov.ave )
##     effect         <- setdiff( effect, at.factor.name )

##     if (is.null(effect)){
##         if (length( at.factor.name ) > 0){
##             new.fact.lev <- at[ at.factor.name ]
##         } else {
##             new.fact.lev <- NULL
##         }
##     } else {
##         new.fact.lev  <- .set_xlevels( fact.lev, at=at )
##         new.fact.lev  <- new.fact.lev[c(effect, at.factor.name)]#
##     }

##     if (is.null(new.fact.lev)){
##         ##cat("No 'effect' and no 'at'-factors; hence just a global average... \n")
##         newdata <- expand.grid(fact.lev)
##         newdata[, cov.ave.name] <- cov.ave

##         XXlist <- list(.getX(object, newdata))
##         attr(XXlist,"at")   <- at[intersect(vartype$numeric, names(at))]
##         attr(XXlist,"grid") <- NULL
##     } else {
##         ##cat("The general case; there are 'effect' factors or 'at' factors...\n")
##         grid.data <- expand.grid(new.fact.lev)
##         grid.data <- as.data.frame(lapply(grid.data, as.character), stringsAsFactors=FALSE)
##         XXlist    <- list()
##         for (ii in 1:nrow(grid.data)){
##             config    <- grid.data[ ii, ,drop=FALSE ]
##             fact.lev2 <- .set_xlevels(fact.lev,  at=config)

##             newdata   <- expand.grid( fact.lev2 )
##             newdata[, cov.ave.name]  <- cov.ave
##             XX             <- .getX(object, newdata)

##             XXlist[[ ii ]] <- XX
##         }

##         grid.data[, names(cov.ave) ] <- cov.ave
##         attr(XXlist,"at") <- at
##         attr(XXlist,"grid") <- grid.data
##     }
##     class(XXlist) <- "linestList"
##     XXlist
## }






