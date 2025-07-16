################################################################################
#'
#' @title Adjusted covariance matrix for linear mixed models according
#'     to Kenward and Roger
#' @description Kenward and Roger (1997) describe an improved small
#'     sample approximation to the covariance matrix estimate of the
#'     fixed parameters in a linear mixed model.
#' @name kr-vcovAdj
#'
################################################################################
## Implemented in Banff, august 2013; Søren Højsgaard

#' @aliases vcovAdj vcovAdj.lmerMod vcovAdj_internal vcovAdj0 vcovAdj2
#'     vcovAdj.mer LMM_Sigma_G get_SigmaG get_SigmaG.lmerMod get_SigmaG.mer
#'
#' @param object An \code{lmer} model
#' @param details If larger than 0 some timing details are printed.
#' @return
#'
#' \item{phiA}{the estimated covariance matrix, this has attributed P, a
#'     list of matrices used in \code{KR_adjust} and the estimated matrix W of
#'     the variances of the covariance parameters of the random effects}
#' 
#' \item{SigmaG}{list: Sigma: the covariance matrix of Y; G: the G matrices that
#' sum up to Sigma; `n.ggamma`: the number (called M in the article) of G
#' matrices) }
#'
#' @note If $N$ is the number of observations, then the \code{vcovAdj()}
#'     function involves inversion of an $N x N$ matrix, so the computations can
#'     be relatively slow.
#' @author Ulrich Halekoh \email{uhalekoh@@health.sdu.dk}, Søren Højsgaard
#'     \email{sorenh@@math.aau.dk}
#' 
#' @seealso \code{\link{getKR}}, \code{\link{KRmodcomp}}, \code{\link[lme4]{lmer}},
#'     \code{\link{PBmodcomp}}, \code{\link{vcovAdj}}
#' 
#' @references Ulrich Halekoh, Søren Højsgaard (2014)., A Kenward-Roger
#'     Approximation and Parametric Bootstrap Methods for Tests in Linear Mixed
#'     Models - The R Package pbkrtest., Journal of Statistical Software,
#'     58(10), 1-30., \url{https://www.jstatsoft.org/v59/i09/}
#' 
#' Kenward, M. G. and Roger, J. H. (1997), \emph{Small Sample Inference for
#' Fixed Effects from Restricted Maximum Likelihood}, Biometrics 53: 983-997.
#' 
#' @keywords inference models
#' @examples
#' 
#' fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy, REML=TRUE)
#' class(fm1)
#'
#' set.seed(123)
#' sleepstudy2 <- sleepstudy[sample(nrow(sleepstudy), size=120), ]
#'
#' fm2 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy2, REML=TRUE)
#' 
#' ## Here the adjusted and unadjusted covariance matrices are identical,
#' ## but that is not generally the case:
#' 
#' v1 <- vcov(fm1)
#' v1a <- vcovAdj(fm1, details=0)
#' v1a / v1
#'
#' v2 <- vcov(fm2)
#' v2a <- vcovAdj(fm2, details=0)
#' v2a / v2
#' 
#' # For comparison, an alternative estimate of the
#' # variance-covariance matrix is based on parametric bootstrap (and
#' # this is easily parallelized):
#' 
#' @export vcovAdj
#' 
#' @rdname kr-vcovAdj
vcovAdj <- function(object, details=0){
  UseMethod("vcovAdj")
}

#' @method vcovAdj lmerMod
#' @rdname kr-vcovAdj
#' @export vcovAdj.lmerMod
vcovAdj.lmerMod <- function(object, details=0){
    if (!(getME(object, "is_REML"))) {
        object <- update(object, . ~ ., REML = TRUE)
    }
    Phi      <- vcov(object)
    SigmaG   <- get_SigmaG(object, details)
    X        <- getME(object, "X")
    vcovAdj_internal(Phi, SigmaG, X, details=details)
}

## Needed to avoid emmeans choking.

#' @export
vcovAdj.lmerMod <- vcovAdj.lmerMod

## Dette er en kopi af '2015' udgaven
vcovAdj_internal <- function(Phi, SigmaG, X, details=0) {

    details = 0
    DB <- details > 0 ## debugging only
    t0 <- proc.time()

    if (DB){
        cat("vcovAdj16_internal\n")
        cat(sprintf("dim(X) : %s\n", toString(dim(X))))
        print(class(X))
        cat(sprintf("dim(Sigma) : %s\n", toString(dim(SigmaG$Sigma))))
        print(class(SigmaG$Sigma))
    }
    
    
    SigmaInv <- chol2inv( chol( forceSymmetric(SigmaG$Sigma) ) )
    ## SS <<- SigmaG$Sigma
    ## November 2022: The line below causes random errors
    ## SigmaInv <- chol2inv( chol( forceSymmetric(as(SigmaG$Sigma, "matrix"))))
    ##SigmaInv <- as(SigmaInv, "dpoMatrix")

    if(DB){
        cat(sprintf("Finding SigmaInv:    time: %10.5f\n", (proc.time()-t0)[1] ));
        t0 <- proc.time()
    }

    #mat <<- list(SigmaG=SigmaG, SigmaInv=SigmaInv, X=X)
    
    t0 <- proc.time()
    ## Finding, TT, HH, 00
    n.ggamma <- SigmaG$n.ggamma
    TT       <- SigmaInv %*% X
    HH       <- OO <- vector("list", n.ggamma)
    for (ii in 1:n.ggamma) {
        #.tmp <- SigmaG$G[[ii]] %*% SigmaInv
        #HH[[ ii ]] <- .tmp
        #OO[[ ii ]] <- .tmp %*% X
        HH[[ ii ]] <- SigmaG$G[[ii]] %*% SigmaInv
        OO[[ ii ]] <- HH[[ ii ]] %*% X       
    }
    if(DB){cat(sprintf("Finding TT, HH, OO:  time: %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()}
    
    ## Finding PP, QQ
    PP <- QQ <- NULL
    for (rr in 1:n.ggamma) {
        OrTrans <- t( OO[[ rr ]] )
        PP <- c(PP, list(forceSymmetric( -1 * OrTrans %*%  TT)))
        for (ss in rr:n.ggamma) {
            QQ <- c(QQ, list(OrTrans %*% SigmaInv %*% OO[[ss]] ))
        }}
    if(DB){cat(sprintf("Finding PP, QQ:      time: %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()}


    ##stat15 <<- list(HH=HH, OO=OO, PP=PP, Phi=Phi, QQ=QQ)
    
    Ktrace <- matrix( NA, nrow=n.ggamma, ncol=n.ggamma )
    for (rr in 1:n.ggamma) {
        HrTrans <- t( HH[[rr]] )
        for (ss in rr:n.ggamma){
            Ktrace[rr,ss] <- Ktrace[ss,rr]<- sum( HrTrans * HH[[ss]] )
        }}
    if(DB) {cat(sprintf("Finding Ktrace:      time: %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()}

    ## Finding information matrix
    IE2 <- matrix( NA, nrow=n.ggamma, ncol=n.ggamma )
    for (ii in 1:n.ggamma) {
        Phi.P.ii <- Phi %*% PP[[ii]]
        for (jj in c(ii:n.ggamma)) {
            www <- .indexSymmat2vec( ii, jj, n.ggamma )
            IE2[ii,jj]<- IE2[jj,ii] <- Ktrace[ii,jj] -
                2 * sum(Phi * QQ[[ www ]]) + sum( Phi.P.ii * ( PP[[jj]] %*% Phi))
        }}
    if(DB){cat(sprintf("Finding IE2:         time: %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()}

    
    eigenIE2 <- eigen(IE2, only.values=TRUE)$values
    condi    <- min(abs(eigenIE2))

    WW <- if (condi > 1e-10) forceSymmetric(2 * solve(IE2)) else forceSymmetric(2 * ginv(IE2))

    ## print("vcovAdj")
    UU <- matrix(0, nrow=ncol(X), ncol=ncol(X))
    ## print(UU)
    for (ii in 1:(n.ggamma - 1)) {
        for (jj in c((ii + 1):n.ggamma)) {
            www <- .indexSymmat2vec( ii, jj, n.ggamma )
            UU <- UU + WW[ii, jj] * (QQ[[ www ]] - PP[[ii]] %*% Phi %*% PP[[jj]])
        }}
    ## print(UU)

    UU <- UU + t(UU)
    ## UU <<- UU
    for (ii in 1:n.ggamma) {
        www <- .indexSymmat2vec( ii, ii, n.ggamma )
        UU <- UU + WW[ii, ii] * (QQ[[ www ]] - PP[[ii]] %*% Phi %*% PP[[ii]])
    }
    ## print(UU)
    GGAMMA <-  Phi %*% UU %*% Phi
    PhiA   <-  Phi + 2 * GGAMMA
    attr(PhiA, "P")     <-PP
    attr(PhiA, "W")     <-WW
    attr(PhiA, "condi") <- condi
    PhiA
}


## ' \dontrun{
## ' nsim <- 100
## '
## ' sim1 <- simulate(fm1, nsim)
## ' B1 <- lapply(sim1, function(y){
## '   fixef(refit(fm1, newresp=y))
## ' })
## '
## ' sim2 <- simulate(fm2, nsim)
## ' B2 <- lapply(sim2, function(y){
## '   fixef(refit(fm2, newresp=y))
## ' })
## ' 
## ' B1 <- do.call(rbind, B1)
## ' B2 <- do.call(rbind, B2)
## ' 
## ' v13 <- cov.wt(B1)$cov
## ' v23 <- cov.wt(B2)$cov
## '
## ' v13 / v1
## ' v23 / v2
## '
## ' v13 / v1a
## ' v23 / v2a
## ' }
## ' 
