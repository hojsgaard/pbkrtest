##########################################################
###
### Bartlett corrected LRT
###
##########################################################

#' @title Model comparison using parametric bootstrap methods.
#' 
#' @description Model comparison of nested models using parametric bootstrap
#'     methods.  Implemented for some commonly applied model types.
#'
#' @name pb-modcomp
#' 
#' @details The model \code{object} must be fitted with maximum likelihood
#'     (i.e. with \code{REML=FALSE}). If the object is fitted with restricted
#'     maximum likelihood (i.e. with \code{REML=TRUE}) then the model is
#'     refitted with \code{REML=FALSE} before the p-values are calculated. Put
#'     differently, the user needs not worry about this issue.
#' 
#' Under the fitted hypothesis (i.e. under the fitted small model) \code{nsim}
#' samples of the likelihood ratio test statistic (LRT) are generetated.
#' 
#' Then p-values are calculated as follows:
#' 
#' LRT: Assuming that LRT has a chi-square distribution.
#' 
#' PBtest: The fraction of simulated LRT-values that are larger or equal to the
#' observed LRT value.
#' 
#' Bartlett: A Bartlett correction is of LRT is calculated from the mean of the
#' simulated LRT-values
#' 
#' Gamma: The reference distribution of LRT is assumed to be a gamma
#' distribution with mean and variance determined as the sample mean and sample
#' variance of the simulated LRT-values.
#' 
#' F: The LRT divided by the number of degrees of freedom is assumed to be
#' F-distributed, where the denominator degrees of freedom are determined by
#' matching the first moment of the reference distribution.
#' 
#' @aliases PBmodcomp PBmodcomp.lm PBmodcomp.merMod getLRT getLRT.lm
#'     getLRT.merMod plot.XXmodcomp PBmodcomp.mer getLRT.mer
#' @param largeModel A model object. Can be a linear mixed effects model or
#'     generalized linear mixed effects model (as fitted with \code{lmer()} and
#'     \code{glmer()} function in the \pkg{lme4} package) or a linear normal
#'     model or a generalized linear model. The \code{largeModel} must be larger
#'     than \code{smallModel} (see below).
#' @param smallModel A model of the same type as \code{largeModel} or a
#'     restriction matrix.
#' @param nsim The number of simulations to form the reference distribution.
#' @param ref Vector containing samples from the reference distribution. If
#'     NULL, this vector will be generated using PBrefdist().
#' @param seed A seed that will be passed to the simulation of new datasets.
#' @param cl A vector identifying a cluster; used for calculating the reference
#'     distribution using several cores. See examples below.
#' @param details The amount of output produced. Mainly relevant for debugging
#'     purposes.
#' @note It can happen that some values of the LRT statistic in the reference
#'     distribution are negative. When this happens one will see that the number
#'     of used samples (those where the LRT is positive) are reported (this
#'     number is smaller than the requested number of samples).
#' 
#' In theory one can not have a negative value of the LRT statistic but in
#' practice on can: We speculate that the reason is as follows: We simulate data
#' under the small model and fit both the small and the large model to the
#' simulated data. Therefore the large model represents - by definition - an
#' overfit; the model has superfluous parameters in it. Therefore the fit of the
#' two models will for some simulated datasets be very similar resulting in
#' similar values of the log-likelihood. There is no guarantee that the the
#' log-likelihood for the large model in practice always will be larger than for
#' the small (convergence problems and other numerical issues can play a role
#' here).
#' 
#' To look further into the problem, one can use the \code{PBrefdist()} function
#' for simulating the reference distribution (this reference distribution can be
#' provided as input to \code{PBmodcomp()}). Inspection sometimes reveals that
#' while many values are negative, they are numerically very small. In this case
#' one may try to replace the negative values by a small positive value and then
#' invoke \code{PBmodcomp()} to get some idea about how strong influence there
#' is on the resulting p-values. (The p-values get smaller this way compared to
#' the case when only the originally positive values are used).
#' 
#' @author Søren Højsgaard \email{sorenh@@math.aau.dk}
#' 
#' @seealso \code{\link{KRmodcomp}}, \code{\link{PBrefdist}}
#' 
#' @references Ulrich Halekoh, Søren Højsgaard (2014)., A Kenward-Roger
#'     Approximation and Parametric Bootstrap Methods for Tests in Linear Mixed
#'     Models - The R Package pbkrtest., Journal of Statistical Software,
#'     58(10), 1-30., \url{http://www.jstatsoft.org/v59/i09/}
#' @keywords models inference
#' @examples
#' 
#' data(beets, package="pbkrtest")
#' head(beets)
#' 
#' ## Linear mixed effects model:
#' sug   <- lmer(sugpct ~ block + sow + harvest + (1|block:harvest), data=beets, REML=FALSE)
#' sug.h <- update(sug, .~. -harvest)
#' sug.s <- update(sug, .~. -sow)
#' 
#' anova(sug, sug.h)
#' PBmodcomp(sug, sug.h, nsim=50)
#' anova(sug, sug.h)
#' PBmodcomp(sug, sug.s, nsim=50)
#' 
#' ## Linear normal model:
#' sug <- lm(sugpct ~ block + sow + harvest, data=beets)
#' sug.h <- update(sug, .~. -harvest)
#' sug.s <- update(sug, .~. -sow)
#' 
#' anova(sug, sug.h)
#' PBmodcomp(sug, sug.h, nsim=50)
#' anova(sug, sug.s)
#' PBmodcomp(sug, sug.s, nsim=50)
#' 
#' ## Generalized linear model
#' counts    <- c(18,17,15,20,10,20,25,13,12)
#' outcome   <- gl(3,1,9)
#' treatment <- gl(3,3)
#' d.AD      <- data.frame(treatment, outcome, counts)
#' head(d.AD)
#' glm.D93   <- glm(counts ~ outcome + treatment, family = poisson())
#' glm.D93.o <- update(glm.D93, .~. -outcome)
#' glm.D93.t <- update(glm.D93, .~. -treatment)
#' 
#' anova(glm.D93, glm.D93.o, test="Chisq")
#' PBmodcomp(glm.D93, glm.D93.o, nsim=50)
#' anova(glm.D93, glm.D93.t, test="Chisq")
#' PBmodcomp(glm.D93, glm.D93.t, nsim=50)
#' 
#' ## Generalized linear mixed model (it takes a while to fit these)
#' \dontrun{
#' (gm1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
#'               data = cbpp, family = binomial))
#' (gm2 <- update(gm1, .~.-period))
#' anova(gm1, gm2)
#' PBmodcomp(gm1, gm2)
#' }
#' 
#' 
#' \dontrun{
#' (fmLarge <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
#' ## removing Days
#' (fmSmall <- lmer(Reaction ~ 1 + (Days|Subject), sleepstudy))
#' anova(fmLarge, fmSmall)
#' PBmodcomp(fmLarge, fmSmall)
#' 
#' ## The same test using a restriction matrix
#' L <- cbind(0,1)
#' PBmodcomp(fmLarge, L)
#' 
#' ## Vanilla
#' PBmodcomp(beet0, beet_no.harv, nsim=1000)
#' 
#' ## Simulate reference distribution separately:
#' refdist <- PBrefdist(beet0, beet_no.harv, nsim=1000)
#' PBmodcomp(beet0, beet_no.harv, ref=refdist)
#' 
#' ## Do computations with multiple processors:
#' ## Number of cores:
#' (nc <- detectCores())
#' ## Create clusters
#' cl <- makeCluster(rep("localhost", nc))
#' 
#' ## Then do:
#' PBmodcomp(beet0, beet_no.harv, cl=cl)
#' 
#' ## Or in two steps:
#' refdist <- PBrefdist(beet0, beet_no.harv, nsim=1000, cl=cl)
#' PBmodcomp(beet0, beet_no.harv, ref=refdist)
#' 
#' ## It is recommended to stop the clusters before quitting R:
#' stopCluster(cl)
#' }
#' 
#' 
#' @export PBmodcomp

#' @rdname pb-modcomp
PBmodcomp <- function(largeModel, smallModel, nsim=1000, ref=NULL, seed=NULL, cl=NULL, details=0){
  UseMethod("PBmodcomp")
}

#' @rdname pb-modcomp
PBmodcomp.merMod <-
PBmodcomp.mer <-
    function(largeModel, smallModel, nsim=1000, ref=NULL, seed=NULL, cl=NULL, details=0){
        
        ##cat("PBmodcomp.lmerMod\n")
        f.large <- formula(largeModel)
        attributes(f.large) <- NULL
        
        if (inherits(smallModel, c("Matrix", "matrix"))){
            f.small <- smallModel
            smallModel <- restrictionMatrix2model(largeModel, smallModel)
        } else {
            f.small <- formula(smallModel)
            attributes(f.small) <- NULL
        }
        
        if (is.null(ref)){
            ref <- PBrefdist(largeModel, smallModel, nsim=nsim, seed=seed, cl=cl, details=details)
        }
        
        ## samples <- attr(ref, "samples")
        ## if (!is.null(samples)){
        ##     nsim <- samples['nsim']
        ##     npos <- samples['npos']
        ## } else {
        ##     nsim <- length(ref)
        ##     npos <- sum(ref>0)
        ## }
        
        LRTstat     <- getLRT(largeModel, smallModel)
        ans         <- .finalizePB(LRTstat, ref)
        .padPB( ans, LRTstat, ref, f.large, f.small)
    }

.padPB <- function(ans, LRTstat, ref, f.large, f.small){
    ans$LRTstat <- LRTstat
    ans$ref     <- ref
    ans$f.large <- f.large
    ans$f.small <- f.small
    ans
}

#' @rdname pb-modcomp
PBmodcomp.lm <- function(largeModel, smallModel, nsim=1000, ref=NULL, seed=NULL, cl=NULL, details=0){

  ok.fam <- c("binomial", "gaussian", "Gamma", "inverse.gaussian", "poisson")
  f.large <- formula(largeModel)
  attributes(f.large) <- NULL

  if (inherits(smallModel, c("Matrix", "matrix"))){
    f.small <- smallModel
    smallModel <- restrictionMatrix2model(largeModel, smallModel)
  } else {
    f.small <- formula(smallModel)
    attributes(f.small) <- NULL
  }

  if (!all.equal((fam.l <- family(largeModel)), (fam.s <- family(smallModel))))
    stop("Models do not have identical identical family\n")
  if (!(fam.l$family %in% ok.fam)){
    stop(sprintf("family must be of type %s", toString(ok.fam)))
  }

  if (is.null(ref)){
    ref <- PBrefdist(largeModel, smallModel, nsim=nsim, seed=seed, cl=cl, details=details)
  }

  LRTstat     <- getLRT(largeModel, smallModel)
  ans         <- .finalizePB(LRTstat, ref)
    .padPB( ans, LRTstat, ref, f.large, f.small)    
}

.finalizePB <- function(LRTstat, ref){

  tobs <- unname(LRTstat[1])
  ndf  <- unname(LRTstat[2])

  refpos <- ref[ref>0]
  nsim <- length(ref)
  npos <- length(refpos)

  ##cat(sprintf("EE=%f VV=%f\n", EE, VV))
  p.chi <- 1 - pchisq(tobs, df=ndf)
  ## Direct computation of tail probability

  n.extreme <- sum(tobs < refpos)
  p.PB  <- (1+n.extreme) / (1+npos)

  test = list(
    LRT      = c(stat=tobs,    df=ndf,    p.value=p.chi),
    PBtest   = c(stat=tobs,    df=NA,     p.value=p.PB))

  test  <- as.data.frame(do.call(rbind, test))
  ans   <- list(test=test, type="X2test", samples=c(nsim=nsim, npos=npos), n.extreme=n.extreme,
                ctime=attr(ref,"ctime"))
  class(ans) <- c("PBmodcomp")
  ans
}

.summarizePB <- function(LRTstat, ref){

  tobs <- unname(LRTstat[1])
  ndf  <- unname(LRTstat[2])

  refpos   <- ref[ref>0]
  nsim <- length(ref)
  npos <- length(refpos)


  EE      <- mean(refpos)
  VV      <- var(refpos)

  ##cat(sprintf("EE=%f VV=%f\n", EE, VV))
  p.chi <- 1-pchisq(tobs, df=ndf)

  ## Direct computation of tail probability
  n.extreme <- sum(tobs < refpos)
  ##p.PB  <- n.extreme / npos
  p.PB  <- (1+n.extreme) / (1+npos)

  p.PB.all  <- (1+n.extreme) / (1+nsim)


  se <- round(sqrt(p.PB*(1-p.PB)/npos),4)
  ci <- round(c(-1.96, 1.96)*se + p.PB,4)

  ## Kernel density estimate
  ##dd <- density(ref)
  ##p.KD <- sum(dd$y[dd$x>=tobs])/sum(dd$y)

  ## Bartlett correction - X2 distribution
  BCstat  <- ndf * tobs/EE
  ##cat(sprintf("BCval=%f\n", ndf/EE))
  p.BC    <- 1-pchisq(BCstat,df=ndf)

  ## Fit to gamma distribution
  scale   <- VV/EE
  shape   <- EE^2/VV
  p.Ga    <- 1-pgamma(tobs, shape=shape, scale=scale)

  ## Fit T/d to F-distribution (1. moment)

  ## FIXME: Think the formula is 2*EE/(EE-1)
  ##ddf  <- 2*EE/(EE-ndf)
  ddf  <- 2*EE/(EE-1)
  Fobs <- tobs/ndf
  if (ddf>0)
      p.FF <- 1-pf(Fobs, df1=ndf, df2=ddf)
  else
      p.FF <- NA

  ## Fit T/d to F-distribution (1. AND 2. moment)
  #' EE2   <- EE/ndf
  #' VV2   <- VV/ndf^2

  #' rho   <- VV2/(2*EE2^2)
  #' ddf2  <- 4 + (ndf+2)/(rho*ndf-1)
  #' lam2  <- (ddf/EE2*(ddf-2))
  #' Fobs2 <- lam2 * tobs/ndf
  #' if (ddf2>0)
  #'   p.FF2 <- 1-pf(Fobs2, df1=ndf, df2=ddf2)
  #' else
  #'   p.FF2 <- NA

  #' cat(sprintf("PB: EE=%f, ndf=%f VV=%f, ddf=%f\n", EE, ndf, VV, ddf))




  test = list(
    PBtest   = c(stat=tobs,    df=NA,  ddf=NA,   p.value=p.PB),
    Gamma    = c(stat=tobs,    df=NA,  ddf=NA,   p.value=p.Ga),
    Bartlett = c(stat=BCstat,  df=ndf, ddf=NA,   p.value=p.BC),
    F        = c(stat=Fobs,    df=ndf, ddf=ddf,  p.value=p.FF),
    LRT      = c(stat=tobs,    df=ndf, ddf=NA,   p.value=p.chi)
    )
    ##          PBkd     = c(stat=tobs,    df=NA,  ddf=NA,   p.value=p.KD),

    ##F2       = c(stat=Fobs2,   df=ndf, ddf=ddf2, p.value=p.FF2),
                                        #,
    #PBtest.all   = c(stat=tobs,    df=NA,  ddf=NA,   p.value=p.PB.all),
    #Bartlett.all = c(stat=BCstat.all,  df=ndf, ddf=NA,   p.value=p.BC.all)
    ##F2       = c(stat=Fobs2,   df=ndf,  p.value=p.FF2, ddf=ddf2)


  test <- as.data.frame(do.call(rbind, test))
  ans <- list(test=test, type="X2test",
              moment = c(mean=EE, var=VV),
              samples= c(nsim=nsim, npos=npos),
              gamma  = c(scale=scale, shape=shape),
              ref    = ref,
              ci     = ci,
              se     = se,
              n.extreme = n.extreme,
              ctime  = attr(ref, "ctime")
              )
  class(ans) <- c("PBmodcomp")
  ans
}



##   rho   <- VV/(2*EE^2)
##   ddf2  <- (ndf*(4*rho+1) - 2)/(rho*ndf-1)
##   lam2  <- (ddf/(ddf-2))/(EE/ndf)
##   cat(sprintf("EE=%f, VV=%f, rho=%f, lam2=%f\n",
##               EE, VV, rho, lam2))

##   ddf2 <- 4 + (ndf+2)/(rho*ndf-1)

##   Fobs2 <- lam2 * tobs/ndf
##   if (ddf2>0)
##     p.FF2 <- 1-pf(Fobs2, df1=ndf, df2=ddf2)
##   else
##     p.FF2 <- NA


### ###########################################################
###
### Utilities
###
### ###########################################################

.PBcommon <- function(x){

    cat(sprintf("Parametric bootstrap test; "))
    if (!is.null((zz<- x$ctime))){
        cat(sprintf("time: %.2f sec; ", round(zz,2)))
    }
    if (!is.null((sam <- x$samples))){
        cat(sprintf("samples: %d extremes: %d;", sam[1], x$n.extreme))
    }
    cat("\n")
    
    
    if (!is.null((sam <- x$samples))){
        if (sam[2]<sam[1]){
            cat(sprintf("Requested samples: %d Used samples: %d Extremes: %d\n",
                        sam[1], sam[2], x$n.extreme))
        }
    }
    if(!is.null(x$f.large)){
        cat("large : "); print(x$f.large)
        cat("small : "); print(x$f.small)
    }
}



print.PBmodcomp <- function(x, ...){
  .PBcommon(x)
  tab <- x$test
  printCoefmat(tab, tst.ind=1, na.print='', has.Pvalue=TRUE)
  return(invisible(x))
}


summary.PBmodcomp <- function(object,...){
  ans <- .summarizePB(object$LRTstat, object$ref)
  ans$f.large <- object$f.large
  ans$f.small <- object$f.small
  class(ans) <- "summaryPB"
  ans
}

print.summaryPB <- function(x,...){
  .PBcommon(x)
  ans <- x$test
  printCoefmat(ans, tst.ind=1, na.print='', has.Pvalue=TRUE)
  cat("\n")

##   ci <- x$ci
##   cat(sprintf("95 pct CI for PBtest   : [%s]\n", toString(ci)))
##   mo <- x$moment
##   cat(sprintf("Reference distribution : mean=%f var=%f\n", mo[1], mo[2]))
##   ga <- x$gamma
##   cat(sprintf("Gamma approximation    : scale=%f shape=%f\n", ga[1], ga[2]))

  return(invisible(x))
}


plot.PBmodcomp <- function(x, ...){

  ref <-x$ref

  ndf  <- x$test$df[1]
  u    <-summary(x)
  ddf  <-u$test['F','ddf']

  EE   <- mean(ref)
  VV   <- var(ref)
  sc   <- var(ref)/mean(ref)
  sh   <- mean(ref)^2/var(ref)
  sc   <- VV/EE
  sh   <- EE^2/VV
  B    <- ndf/EE # if ref is the null distr, so should A*ref follow a chisq(df=ndf) distribution

  upper <- 0.20
  #tail.prob <- c(0.0001, 0.001, 0.01, 0.05, 0.10, 0.20, 0.5)
  tail.prob <-seq(0.001, upper, length.out = 1111)
  PBquant   <- quantile(ref,1-tail.prob) ## tail prob for PB dist

  pLR       <- pchisq(PBquant,df=ndf,           lower.tail=FALSE)
  pF        <- pf(PBquant/ndf,df1=ndf,df2=ddf,  lower.tail=FALSE)
  pGamma    <- pgamma(PBquant,scale=sc,shape=sh,lower.tail=FALSE)
  pBart     <- pchisq(B*PBquant,df=ndf,         lower.tail=FALSE)

  sym.vec <- c(2,4,5,6)
  lwd     <- 2
  plot(pLR~tail.prob,type='l', lwd=lwd, #log="xy",
       xlab='Nominal p-value',ylab='True p-value',
       xlim=c(1e-3, upper),ylim=c(1e-3, upper),
       col=sym.vec[1], lty=sym.vec[1])
  lines(pF~tail.prob,lwd=lwd,     col=sym.vec[2], lty=sym.vec[2])
  lines(pBart~tail.prob,lwd=lwd,  col=sym.vec[3], lty=sym.vec[3])
  lines(pGamma~tail.prob,lwd=lwd, col=sym.vec[4], lty=sym.vec[4])
  abline(c(0,1))

  ZF     <-bquote(paste("F(1,",.(round(ddf,2)),")"))
  Zgamma <-bquote(paste("gamma(scale=",.(round(sc,2)),", shape=", .(round(sh,2)),")" ))
  ZLRT   <-bquote(paste(chi[.(ndf)]^2))
  ZBart  <-bquote(paste("Bartlett scaled ", chi[.(ndf)]^2))

  legend(0.001,upper,legend=as.expression(c(ZLRT,ZF,ZBart,Zgamma)),
         lty=sym.vec,col=sym.vec,lwd=lwd)
}
as.data.frame.XXmodcomp <- function(x, row.names = NULL, optional = FALSE, ...){
    as.data.frame(do.call(rbind, x[-c(1:3)]))
}




## plot.XXmodcomp <- function(x, ...){

##   test <- x$test
##   tobs <- test$LRT['stat']
##   ref <- attr(x,"ref")
##   rr  <- range(ref)
##   xx  <- seq(rr[1],rr[2],0.1)
##   dd  <- density(ref)
##   sc  <- var(ref)/mean(ref)
##   sh  <- mean(ref)^2/var(ref)

##   hist(ref, prob=TRUE,nclass=20, main="Reference distribution")
##   abline(v=tobs)
##   lines(dd, lty=2, col=2, lwd=2)
##   lines(xx,dchisq(xx,df=test$LRT['df']), lty=3, col=3, lwd=2)
##   lines(xx,dgamma(xx,scale=sc, shape=sh), lty=4, col=4, lwd=2)
##   lines(xx,df(xx,df1=test$F['df'], df2=test$F['ddf']), lty=5, col=5, lwd=2)

##   smartlegend(x = 'right', y = 'top',
##               legend = c("kernel density", "chi-square", "gamma","F"),
##               col = 2:5, lty = 2:5)

## }




##   rho   <- VV/(2*EE^2)
##   ddf2  <- (ndf*(4*rho+1) - 2)/(rho*ndf-1)
##   lam2  <- (ddf/(ddf-2))/(EE/ndf)
##   cat(sprintf("EE=%f, VV=%f, rho=%f, lam2=%f\n",
##               EE, VV, rho, lam2))

##   ddf2 <- 4 + (ndf+2)/(rho*ndf-1)

##   Fobs2 <- lam2 * tobs/ndf
##   if (ddf2>0)
##     p.FF2 <- 1-pf(Fobs2, df1=ndf, df2=ddf2)
##   else
##     p.FF2 <- NA
