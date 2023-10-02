pbkrtest/R/


    ## res@Jac_list <- lapply(1:ncol(Jac), function(i)
    ##     array(Jac[, i], dim=rep(length(res@beta), 2))) 

    ## res@vcov_varpar <- 2 * h_inv # vcov(varpar)

    ## ## From lmer (in package)
    ## mc <- model@call
    ## ## model <- eval.parent(mc) ## NOTE Is this really needed??
    ## ## if(devFunOnly) return(model)
    ## ## Make an lmerModLmerTest object:
    ## args <- as.list(mc)
    ## args$devFunOnly <- TRUE
    ## Call <- as.call(c(list(quote(lme4::lmer)), args[-1]))
    ## devfun <- eval.parent(Call)

    ## Fra as_lmerModLT

    
    ## Set relevant slots of the new model object:
    ##res@sigma <- sigma(model)                                     ##     
    ##res@vcov_beta <- as.matrix(vcov(model))                       ##    

    
## .do_sampling <- function(largeModel, smallModel, nsim, cl, get_fun, details=0){
##     if (is.null(cl)){
##         cl <- getOption("cl")
##         if (is.numeric(cl)) cat("getting 'cl' from options; cl = ", cl, "\n")
##         else cat("getting 'cl' from options; length(cl) = ", length(cl), "\n")
##     }
        
##     if (is.null(cl)){
##         cat("setting cl=1\n")
##         cl <- 1
##     }

##     if (is.numeric(cl)){
##         if (!(length(cl)==1 && cl >= 1)) stop("Invalid numeric cl\n")           
##         cat("doing mclapply, cl = ", cl, "\n")
##         nsim.cl <- nsim %/% cl
##         ref <- unlist(mclapply(1:cl, function(i) {get_fun(largeModel, smallModel, nsim=nsim.cl)}, mc.cores=cl))
##     } else
##         if (inherits(cl, "cluster")){
##             cat("doing clusterCall, nclusters = ", length(cl), "\n")
##             nsim.cl <- nsim %/% length(cl)
##             clusterSetRNGStream(cl)
##             ref <- unlist(clusterCall(cl, fun=get_fun,
##                                       largeModel, smallModel, nsim=nsim.cl))                
##         }
##     else
##         stop("Invalid 'cl'\n")
## } 

    
    ## if (is.null(cl)){
    ##     cl <- getOption("cl")
    ##     if (is.numeric(cl)) cat("getting 'cl' from options; cl = ", cl, "\n")
    ##     else cat("getting 'cl' from options; length(cl) = ", length(cl), "\n")
    ## }
        
    ## if (is.null(cl)){
    ##     cat("setting cl=1\n")
    ##     cl <- 1
    ## }

    ## if (is.numeric(cl)){
    ##     if (!(length(cl)==1 && cl >= 1)) stop("Invalid numeric cl\n")           
    ##     cat("doing mclapply, cl = ", cl, "\n")
    ##     nsim.cl <- nsim %/% cl
    ##     ref <- unlist(mclapply(1:cl, function(i) {get_fun(largeModel, smallModel, nsim=nsim.cl)}, mc.cores=cl))
    ## } else
    ##     if (inherits(cl, "cluster")){
    ##         cat("doing clusterCall, nclusters = ", length(cl), "\n")
    ##         nsim.cl <- nsim %/% length(cl)
    ##         clusterSetRNGStream(cl)
    ##         ref <- unlist(clusterCall(cl, fun=get_fun,
    ##                                   largeModel, smallModel, nsim=nsim.cl))                
    ##     }
    ## else
    ##     stop("Invalid 'cl'\n")





















## #' @rdname pb-refdist
## PBrefdist.lm <- function(largeModel, smallModel, nsim=1000, seed=NULL, cl=NULL, details=0){

##   ##cat(".....PBrefdist.lm\n")
##   t0 <- proc.time()
##   .is.cluster <- !is.null(cl) && inherits(cl, "cluster")

##   if (!.is.cluster){
##     ref <- .lm_refDist(largeModel, smallModel, nsim, seed=seed)
##   } else {
##     nsim2 <- round(nsim/length(cl))
##     if (details>=1)
##       cat(sprintf("* Using %i clusters and %i samples per cluster\n", length(cl), nsim2))
##     clusterExport(cl, ls(envir=.GlobalEnv), envir = .GlobalEnv)
##     clusterSetRNGStream(cl)
##     ref <- unlist(clusterCall(cl, .lm_refDist, largeModel, smallModel, nsim2))
##   }

##   ref <- ref[ref>0]
##   ctime <- (proc.time()-t0)[3]
##   attr(ref,"ctime") <- ctime
##   if (details>0)
##     cat(sprintf("Reference distribution with %i samples; computing time: %5.2f secs. \n",
##                 length(ref), ctime))

##   ref
## }


## #' @rdname pb-refdist
## PBrefdist.lmerMod <- function(largeModel, smallModel, nsim=1000, seed=NULL, cl=NULL, details=0){


##     t0 <- proc.time()
##     if (getME(smallModel, "is_REML"))
##         smallModel <- update(smallModel, REML=FALSE)
##     if (getME(largeModel, "is_REML"))
##         largeModel <- update(largeModel, REML=FALSE)
    
##     .is.cluster <- !is.null(cl) && inherits(cl, "cluster")

##     if (!.is.cluster){
##         ref <- .get_refdist_merMod(largeModel, smallModel, nsim=nsim, seed=seed)
##     } else {
##         nsim.cl <- nsim %/% length(cl)
##         clusterSetRNGStream(cl)
##         ref <- unlist(clusterCall(cl, fun=.get_refdist_merMod,
##                                   largeModel, smallModel, nsim=nsim.cl) )
##     }


    
##     LRTstat     <- getLRT(largeModel, smallModel)
##     ctime <- (proc.time()-t0)[3]
##     attr(ref, "ctime")   <- ctime
##     attr(ref, "stat")    <- LRTstat
##     attr(ref, "samples") <- c(nsim=nsim, npos=sum(ref > 0),
##                               n.extreme=sum(ref > LRTstat["tobs"]),
##                               pPB=(1 + sum(ref > LRTstat["tobs"])) / (1 + sum(ref > 0)))
##     if (details>0)
##         cat(sprintf("Reference distribution with %5i samples; computing time: %5.2f secs. \n",
##                     length(ref), ctime))
    
##     ref
## }


## #' @rdname pb-refdist
## PBrefdist.mer <- PBrefdist.merMod













