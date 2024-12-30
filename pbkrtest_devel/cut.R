

##
## Modular implementation
##

## .get_GI_parms <- function( object ){
  ## GGamma <- VarCorr(object)
  ## parmList <- lapply(GGamma, function(Lii){  Lii[ lower.tri( Lii, diag=TRUE ) ] })
  ## parmList <- c( parmList, sigma( object )^2 )
  ## parmList
## }

## .get_GI_matrices <- function( object ){

  ## SS     <- .shgetME( object )
  ## Zt <- getME( object, "Zt" )

  ## G  <- NULL
  ## G  <- vector("list", SS$n.RT+1)

  ## for (ss in 1:SS$n.RT) {
    ## ZZ    <- .shget_Zt_group( ss, Zt, SS$Gp )
    ## n.lev <- SS$n.lev.by.RT2[ ss ] ## ; cat(sprintf("n.lev=%i\n", n.lev))
    ## Ig    <- sparseMatrix(1:n.lev, 1:n.lev, x=1)
    ## UU <- vector("list", SS$n.parm.by.RT)
    ## for (rr in 1:SS$n.parm.by.RT[ ss ]) {
      ## ii.jj <- .index2UpperTriEntry( rr, SS$n.comp.by.RT[ ss ] )
      ## ii.jj <- unique(ii.jj)
      ## if (length(ii.jj)==1){
        ## EE <- sparseMatrix(ii.jj, ii.jj, x=1, dims=rep(SS$n.comp.by.RT[ ss ], 2))
      ## } else {
        ## EE <- sparseMatrix(ii.jj, ii.jj[2:1], dims=rep(SS$n.comp.by.RT[ ss ], 2))
      ## }
      ## EE <- Ig %x% EE  ## Kronecker product
      ## UU[[ rr ]] <- t(ZZ) %*% EE %*% ZZ
    ## }
    ## G[[ ss ]] <- UU
  ## }
  ## n.obs <- nrow(getME(object,'X'))
  ## G[[ length( G ) ]] <- sparseMatrix(1:n.obs, 1:n.obs, x=1 )
  ## G
## }




## #' @export
## get_SigmaG.mer  <- function(object, details=0) {
##   LMM_Sigma_G( object, details )
## }

## ##############################################################################
##
## LMM_Sigma_G: Returns VAR(Y) = Sigma and the G matrices
##
## ##############################################################################

## LMM_Sigma_G  <- function(object, details=0) { 

##     DB     <- details > 0 ## For debugging only
    
##     if (!.is.lmm(object))
##         stop("'object' is not Gaussian linear mixed model")
    
##     GGamma <- VarCorr(object)  
##     ## Indexing of the covariance matrix;
##     ## this is somewhat technical and tedious
##     Nindex <- .get_indices(object)
    
##     ## number of random effects in each groupFac; note: residual error excluded!
##     n.groupFac <- Nindex$n.groupFac
    
##     ## the number of random effects for each grouping factor
##     nn.groupFacLevels <- Nindex$nn.groupFacLevels
    
##     ## size of the symmetric variance Gamma_i for reach groupFac
##     nn.GGamma <- Nindex$nn.GGamma
    
##     ## number of variance parameters of each GGamma_i  
##     mm.GGamma   <-  Nindex$mm.GGamma
    
##     ## not sure what this is...
##     group.index <- Nindex$group.index
    
##     ## writing the covariance parameters for the random effects into a vector: 
##     ggamma <- NULL
##     for ( ii in 1:(n.groupFac) ) {
##         Lii <- GGamma[[ii]]
##         nu  <- ncol(Lii)
##         ## Lii[lower.tri(Lii,diag=TRUE)= Lii[1,1],Lii[1,2],Lii[1,3]..Lii[1,nu],
##         ##                               Lii[2,2], Lii[2,3] ...
##         ggamma<-c(ggamma,Lii[lower.tri(Lii,diag=TRUE)])
##     }
    
##     ## extend ggamma by the residuals variance such that everything random is included
##     ggamma   <- c( ggamma, sigma( object )^2 )
##     n.ggamma <- length(ggamma)
    
##     ## Find G_r:
##     Zt <- getME( object, "Zt" )
    
##     t0 <- proc.time()
##     G  <- NULL
##     ##cat(sprintf("n.groupFac=%i\n", n.groupFac))
##     for (ss in 1:n.groupFac) {
##         ZZ <- .get_Zt_group(ss, Zt, object)
##         ##cat("ZZ\n"); print(ZZ)
        
##         n.levels <- nn.groupFacLevels[ss]
##         ##cat(sprintf("n.levels=%i\n", n.levels))
        
##         Ig <- sparseMatrix(1:n.levels, 1:n.levels, x=1)
##         ##print(Ig)
##         for (rr in 1:mm.GGamma[ss]) {
##             ii.jj <- .indexVec2Symmat(rr,nn.GGamma[ss])
##             ##cat("ii.jj:"); print(ii.jj)
##             ii.jj <- unique(ii.jj)
            
##             if (length(ii.jj)==1){
##                 EE <- sparseMatrix(ii.jj, ii.jj, x=1, dims=rep(nn.GGamma[ss],2))
##             } else {
##                 EE <- sparseMatrix(ii.jj, ii.jj[2:1], dims=rep(nn.GGamma[ss],2))
##             }
##             ##cat("EE:\n");print(EE)
            
##             EE <- Ig %x% EE  ## Kronecker product
##             G  <- c( G, list( t(ZZ) %*% EE %*% ZZ ) )
##         }
##     }
    
##     ## Extend by the indentity for the residual
##     nobs <- nrow(getME(object,'X'))
##     G    <- c( G, list(sparseMatrix(1:nobs, 1:nobs, x=1 )) ) 
    
    
##     if(DB){cat(sprintf("Finding G  %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()}
    
##     Sigma <- ggamma[1] * G[[1]]
##     for (ii in 2:n.ggamma) {
##         Sigma <- Sigma + ggamma[ii] * G[[ii]]
##     }
    
##     if(DB){cat(sprintf("Finding Sigma:    %10.5f\n", (proc.time()-t0)[1] ));
##         t0 <- proc.time()}
    
##     SigmaG <- list(Sigma=Sigma, G=G, n.ggamma=n.ggamma)
##     SigmaG
## }  

## .get_indices <-function(object) {

##   ## ff = number of random effects terms (..|F1) + (..|F1) are group factors!
##   ## without the residual variance output: list of several indices

##   ## we need  the number of random-term factors 
##   Gp <- getME(object,"Gp")

##   ff <- length(Gp)-1 
##   gg <- sapply(getME(object,"flist"), function(x)length(levels(x)))

##   qq <- .get.RT.dim.by.RT( object ) ##;  cat("qq:\n"); print(qq)
  
##   ## number of variance parameters of each GGamma_i
##   ss <- qq * (qq+1) / 2

##   ## numb of random effects per level of random-term-factor
##   nn.groupFac <- diff(Gp)  
##   ##cat("nn.groupFac:\n"); print(nn.groupFac)
  
##   ## number  of levels for each  random-term-factor; residual error here excluded!
##   nn.groupFacLevels <- nn.groupFac / qq

##   ## this is  the number of random term factors, should possible get a more approriate name
##   list(n.groupFac           = ff, 
##        nn.groupFacLevelsNew = gg,                # length of different grouping factors
##        nn.groupFacLevels    = nn.groupFacLevels, # vector of the numb. levels for each random-term-factor
##        nn.GGamma            = qq,
##        mm.GGamma            = ss,
##        group.index          = Gp)
## }

## .get_Zt_group <- function(ii.group, Zt, object) {

##   ## ii.group : the index number of a grouping factor
##   ## Zt       : the transpose of the random factors design matrix Z
##   ## object   : A mer or lmerMod model
##   ##output :  submatrix of Zt belongig to grouping factor ii.group

##   Nindex            <- .get_indices(object)
##   nn.groupFacLevels <- Nindex$nn.groupFacLevels
##   nn.GGamma         <- Nindex$nn.GGamma
##   group.index       <- Nindex$group.index
##   .cc               <- class(object)

## ##   cat(".get_Zt_group\n");
## ##   print(group.index)
## ##   print(ii.group)
  
##   zIndex.sub <-
##     if (.cc %in% "mer") {
##       Nindex$group.index[ii.group]+
##         1+c(0:(nn.GGamma[ii.group]-1))*nn.groupFacLevels[ii.group] +
##           rep(0:(nn.groupFacLevels[ii.group]-1),each=nn.GGamma[ii.group]) 
##     } else {
##       if (.cc %in% "lmerMod" ) {
##         c((group.index[ii.group]+1) : group.index[ii.group+1])
##       }
##     }
##   ZZ <- Zt[ zIndex.sub , ]
##   return(ZZ)
## }
















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













