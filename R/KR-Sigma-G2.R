## ##############################################################################
##
## LMM_Sigma_G: Returns VAR(Y) = Sigma and the G matrices
##
## Re-implemented in Banff, Canada, August 2013 by Søren Højsgaard
##
## ##############################################################################

#' @export
get_SigmaG <- function(object, details=0) {
  UseMethod("get_SigmaG")
}

#' @export
get_SigmaG.lmerMod  <- function(object, details=0) {
  .get_SigmaG( object, details )
}

.get_SigmaG  <- function(object, details=0) {

  DB     <- details > 0 ## For debugging only
  if (!.is.lmm(object))
    stop("'object' is not Gaussian linear mixed model")

  GGamma <- VarCorr(object)
  SS     <- .shgetME( object )

  ## Put covariance parameters for the random effects into a vector:
  ## Fixme: It is a bit ugly to throw everything into one long vector here; a list would be more elegant
  ggamma <- NULL
  for ( ii in 1:( SS$n.RT )) {
    Lii    <- GGamma[[ii]]
    ggamma <- c(ggamma, Lii[ lower.tri( Lii, diag=TRUE ) ] )
  }
  ggamma   <- c( ggamma, sigma( object )^2 ) ## Extend ggamma by the residuals variance
  n.ggamma <- length(ggamma)

  ## Find G_r:
  G  <- NULL
  Zt <- getME( object, "Zt" )
  for (ss in 1:SS$n.RT) {
    ZZ    <- .shget_Zt_group( ss, Zt, SS$Gp )
    n.lev <- SS$n.lev.by.RT2[ ss ] ## ; cat(sprintf("n.lev=%i\n", n.lev))
    Ig    <- sparseMatrix(1:n.lev, 1:n.lev, x=1)
    for (rr in 1:SS$n.parm.by.RT[ ss ]) {
      ## This is takes care of the case where there is random regression and several matrices have to be constructed.
      ## FIXME: I am not sure this is correct if there is a random quadratic term. The '2' below looks suspicious.
      ii.jj <- .index2UpperTriEntry( rr, SS$n.comp.by.RT[ ss ] ) ##; cat("ii.jj:"); print(ii.jj)
      ii.jj <- unique(ii.jj)
      if (length(ii.jj)==1){
        EE <- sparseMatrix(ii.jj, ii.jj, x=1, dims=rep(SS$n.comp.by.RT[ ss ], 2))
      } else {
        EE <- sparseMatrix(ii.jj, ii.jj[2:1], dims=rep(SS$n.comp.by.RT[ ss ], 2))
      }
      EE <- Ig %x% EE  ## Kronecker product
      G  <- c( G, list( t(ZZ) %*% EE %*% ZZ ) )
    }
  }

  ## Extend by the indentity for the residual
  n.obs <- nrow(getME(object,'X'))
  G    <- c( G, list(sparseMatrix(1:n.obs, 1:n.obs, x=1 )) )


  Sigma <- ggamma[1] * G[[1]]
  for (ii in 2:n.ggamma) {
    Sigma <- Sigma + ggamma[ii] * G[[ii]]
  }

  SigmaG <- list(Sigma=Sigma, G=G, n.ggamma=n.ggamma)
  SigmaG
}


.shgetME <- function( object ){
  Gp           <- getME( object, "Gp" )
  n.RT         <- length( Gp ) - 1  ## Number of random terms ( i.e. of (|)'s )
  n.lev.by.RT  <- sapply(getME(object, "flist"), function(x) length(levels(x)))
  n.comp.by.RT <- .get.RT.dim.by.RT( object )
  n.parm.by.RT <- (n.comp.by.RT + 1) * n.comp.by.RT / 2
  n.RE.by.RT   <- diff( Gp )

  n.lev.by.RT2 <- n.RE.by.RT / n.comp.by.RT ## Same as n.lev.by.RT2 ???

  list(Gp           = Gp,           ## group.index
       n.RT         = n.RT,         ## n.groupFac
       n.lev.by.RT  = n.lev.by.RT,  ## nn.groupFacLevelsNew
       n.comp.by.RT = n.comp.by.RT, ## nn.GGamma
       n.parm.by.RT = n.parm.by.RT, ## mm.GGamma
       n.RE.by.RT   = n.RE.by.RT,   ## ... Not returned before
       n.lev.by.RT2 = n.lev.by.RT2, ## nn.groupFacLevels
       n_rtrms      = getME( object, "n_rtrms")
       )
}

.getME.all <- function(obj) {
   nmME <- eval(formals(getME)$name)
   sapply(nmME, function(nm) try(getME(obj, nm)),
          simplify=FALSE)

}

## Alternative to .get_Zt_group
.shget_Zt_group <- function( ii.group, Zt, Gp, ... ){
  zIndex.sub <-  (Gp[ii.group]+1) : Gp[ii.group+1]
  ZZ <- Zt[ zIndex.sub , ]
  return(ZZ)
}


##
## Modular implementation
##

.get_GI_parms <- function( object ){
  GGamma <- VarCorr(object)
  parmList <- lapply(GGamma, function(Lii){  Lii[ lower.tri( Lii, diag=TRUE ) ] })
  parmList <- c( parmList, sigma( object )^2 )
  parmList
}

.get_GI_matrices <- function( object ){

  SS     <- .shgetME( object )
  Zt <- getME( object, "Zt" )

  G  <- NULL
  G  <- vector("list", SS$n.RT+1)

  for (ss in 1:SS$n.RT) {
    ZZ    <- .shget_Zt_group( ss, Zt, SS$Gp )
    n.lev <- SS$n.lev.by.RT2[ ss ] ## ; cat(sprintf("n.lev=%i\n", n.lev))
    Ig    <- sparseMatrix(1:n.lev, 1:n.lev, x=1)
    UU <- vector("list", SS$n.parm.by.RT)
    for (rr in 1:SS$n.parm.by.RT[ ss ]) {
      ii.jj <- .index2UpperTriEntry( rr, SS$n.comp.by.RT[ ss ] )
      ii.jj <- unique(ii.jj)
      if (length(ii.jj)==1){
        EE <- sparseMatrix(ii.jj, ii.jj, x=1, dims=rep(SS$n.comp.by.RT[ ss ], 2))
      } else {
        EE <- sparseMatrix(ii.jj, ii.jj[2:1], dims=rep(SS$n.comp.by.RT[ ss ], 2))
      }
      EE <- Ig %x% EE  ## Kronecker product
      UU[[ rr ]] <- t(ZZ) %*% EE %*% ZZ
    }
    G[[ ss ]] <- UU
  }
  n.obs <- nrow(getME(object,'X'))
  G[[ length( G ) ]] <- sparseMatrix(1:n.obs, 1:n.obs, x=1 )
  G
}




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













