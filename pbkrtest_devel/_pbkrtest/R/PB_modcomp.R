#' @title Model comparison using parametric bootstrap methods.
#' 
#' @description Model comparison of nested models using parametric bootstrap
#'     methods.  Implemented for some commonly applied model types.
#' @concept model_comparison
#' @name pb__modcomp
#' 
#' @details
#'
#' The model \code{object} must be fitted with maximum likelihood
#'     (i.e. with \code{REML=FALSE}). If the object is fitted with
#'     restricted maximum likelihood (i.e. with \code{REML=TRUE}) then
#'     the model is refitted with \code{REML=FALSE} before the
#'     p-values are calculated. Put differently, the user needs not
#'     worry about this issue.
#' 
#' Under the fitted hypothesis (i.e. under the fitted small model) \code{nsim}
#' samples of the likelihood ratio test statistic (LRT) are generated.
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
#' @aliases PBmodcomp PBmodcomp.lm PBmodcomp.merMod plot.XXmodcomp
#'     PBmodcomp.mer getLRT.mer
#'
#' @inheritParams kr_modcomp
#' @param nsim The number of simulations to form the reference
#'     distribution.
#' @param ref Vector containing samples from the reference
#'     distribution. If NULL, this vector will be generated using
#'     `PBrefdist()`.
#' @param seed A seed that will be passed to the simulation of new
#'     datasets.
#' @param h For sequential computing for bootstrap p-values: The
#'     number of extreme cases needed to generate before the sampling
#'     process stops.
#' @param cl A vector identifying a cluster; used for calculating the
#'     reference distribution using several cores. See examples below.
#' @param details The amount of output produced. Mainly relevant for
#'     debugging purposes.
#' @note It can happen that some values of the LRT statistic in the
#'     reference distribution are negative. When this happens one will
#'     see that the number of used samples (those where the LRT is
#'     positive) are reported (this number is smaller than the
#'     requested number of samples).
#' 
#' In theory one can not have a negative value of the LRT statistic but in
#' practice on can: We speculate that the reason is as follows: We simulate data
#' under the small model and fit both the small and the large model to the
#' simulated data. Therefore the large model represents - by definition - an
#' over fit; the model has superfluous parameters in it. Therefore the fit of the
#' two models will for some simulated datasets be very similar resulting in
#' similar values of the log-likelihood. There is no guarantee that the the
#' log-likelihood for the large model in practice always will be larger than for
#' the small (convergence problems and other numerical issues can play a role
#' here).
#' 
#' To look further into the problem, one can use the `PBrefdist()` function
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
#'     58(10), 1-30., \url{https://www.jstatsoft.org/v59/i09/}
#' @keywords models inference
#' @examples
#'
#' \dontrun{ 
#' (fm0 <- lmer(Reaction ~ (Days|Subject), sleepstudy))
#' (fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
#' (fm2 <- lmer(Reaction ~ Days + I(Days^2) + (Days|Subject), sleepstudy))
#'
#' NSIM <- 50 ## Simulations in parametric bootstrap; default is 1000.
#' 
#' ## Test for no effect of Days in fm1, i.e. test fm0 under fm1
#' PBmodcomp(fm1, "Days", cl=1, nsim=NSIM)
#' PBmodcomp(fm1, ~.-Days, cl=1, nsim=NSIM)
#' L1 <- cbind(0, 1)
#' PBmodcomp(fm1, L1, cl=1, nsim=NSIM) 
#' PBmodcomp(fm1, fm0, cl=1, nsim=NSIM)
#' anova(fm1, fm0)
#'
#' ## Test for no effect of Days and Days-squared in fm2, i.e. test fm0 under fm2
#' PBmodcomp(fm2, "(Days+I(Days^2))", cl=1, nsim=NSIM)
#' PBmodcomp(fm2, ~. - Days - I(Days^2), cl=1, nsim=NSIM)
#' L2 <- rbind(c(0, 1, 0), c(0, 0, 1))
#' PBmodcomp(fm2, L2, cl=1, nsim=NSIM) ## FIXME
#'
#' PBmodcomp(fm2, fm0, cl=1, nsim=NSIM)
#' anova(fm2, fm0)
#'
#' ## Test for no effect of Days-squared in fm2, i.e. test fm1 under fm2
#' PBmodcomp(fm2, "I(Days^2)", cl=1, nsim=NSIM)
#' PBmodcomp(fm2, ~. - I(Days^2), cl=1, nsim=NSIM)
#' L3 <- rbind(c(0, 0, 1))
#' PBmodcomp(fm2, L3, cl=1, nsim=NSIM) 
#' PBmodcomp(fm2, fm1, cl=1, nsim=NSIM)
#' anova(fm2, fm1)
#' 
#' ## Linear normal model:
#' sug <- lm(sugpct ~ block + sow + harvest, data=beets)
#' sug.h <- update(sug, .~. -harvest)
#' sug.s <- update(sug, .~. -sow)
#' 
#' PBmodcomp(sug, "harvest", nsim=NSIM, cl=1)
#' PBmodcomp(sug, ~. - harvest, nsim=NSIM, cl=1)
#' PBmodcomp(sug, sug.h, nsim=NSIM, cl=1)
#' anova(sug, sug.h)
#' 
#' ## Generalized linear model
#' mm <- glm(ndead/ntotal ~ sex + log(dose), family=binomial, weight=ntotal, data=budworm)
#' mm0 <- update(mm, .~. -sex)
#'
#' ### Test for no effect of sex
#' PBmodcomp(mm, "sex", cl=1, nsim=NSIM)
#' PBmodcomp(mm, ~.-sex, cl=1, nsim=NSIM)
#' ## PBmodcomp(mm, cbind(0, 1, 0), nsim=NSIM): FIXME
#' PBmodcomp(mm, mm0, cl=1, nsim=NSIM)
#' anova(mm, mm0, test="Chisq")
#' }
#' 
#' ## Generalized linear mixed model (it takes a while to fit these)
#' 
#' \dontrun{
#' (gm1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
#'               data = cbpp, family = binomial))
#' (gm2 <- update(gm1, .~.-period))
#' 
#' PBmodcomp(gm1, "period", nsim=NSIM)
#' PBmodcomp(gm1, ~. -period, nsim=NSIM)
#' PBmodcomp(gm1, gm2, nsim=NSIM)
#' anova(gm1, gm2)
#' }
#' 
#' \dontrun{
#' ## Linear mixed effects model:
#' sug   <- lmer(sugpct ~ block + sow + harvest + (1|block:harvest),
#'               data=beets, REML=FALSE)
#' sug.h <- update(sug, .~. -harvest)
#' sug.s <- update(sug, .~. -sow)
#' 
#' anova(sug, sug.h)
#' PBmodcomp(sug, sug.h, nsim=NSIM, cl=1)
#' PBmodcomp(sug, "harvest", nsim=NSIM, cl=1)
#' 
#' anova(sug, sug.s)
#' PBmodcomp(sug, sug.s, nsim=NSIM, cl=1)
#' PBmodcomp(sug, "sow", nsim=NSIM, cl=1)
#'
#' ## Simulate reference distribution separately:
#' refdist <- PBrefdist(sug, sug.h, nsim=1000, cl=1)
#' refdist <- PBrefdist(sug, "harvest", nsim=1000, cl=1)
#' refdist <- PBrefdist(sug, ~.-harvest, nsim=1000, cl=1)
#' 
#' ## Do computations with multiple processors:
#' ## Number of cores:
#' 
#' (nc <- detectCores())
#' ## Create clusters
#' cl <- makeCluster(rep("localhost", nc))
#' 
#' ## Then do:
#' refdist <- PBrefdist(sug, sug.h, nsim=1000, cl=cl)
#' 
#' ## It is recommended to stop the clusters before quitting R:
#' stopCluster(cl)
#' }
#'
#' lm1 <- lm(dist~speed+I(speed^2), data=cars)
#' PBmodcomp(lm1, .~.-speed, cl=2)
#' PBmodcomp(lm1, .~.-I(speed^2), cl=2)
#' 
#' 
#' @export
#' @rdname pb__modcomp
PBmodcomp <- function(largeModel, smallModel, nsim=1000, ref=NULL, seed=NULL, cl=NULL, details=0){
  UseMethod("PBmodcomp")
}



## '
## ' @examples
## ' if (requireNamespace("nlme", quietly = TRUE)) {
## '   library(nlme)
## '
## '   # Load data
## '   data(sleepstudy)
## '
## '   # Create quadratic term explicitly
## '   sleepstudy$Days2 <- sleepstudy$Days^2
## '
## '   # Model 0: Random intercept and slope, no fixed effect for Days
## '   fm0 <- lme(Reaction ~ 1,
## '              random = ~ Days | Subject,
## '              data = sleepstudy,
## '              method = "REML")
## '
## '   # Model 1: Add fixed effect for Days
## '   fm1 <- lme(Reaction ~ Days,
## '              random = ~ Days | Subject,
## '              data = sleepstudy,
## '              method = "REML")
## '
## '   # Model 2: Add fixed quadratic effect
## '   fm2 <- lme(Reaction ~ Days + Days2,
## '              random = ~ Days | Subject,
## '              data = sleepstudy,
## '              method = "REML")
## '
## '   # Create quadratic term explicitly
## '   sleepstudy$Days2 <- sleepstudy$Days^2
## '
## '   # Model 0: Intercept only
## '   g0 <- gls(Reaction ~ 1,
## '             data = sleepstudy,
## '             method = "REML")
## '
## '   # Model 1: Add linear effect of Days
## '   g1 <- gls(Reaction ~ Days,
## '             data = sleepstudy,
## '             method = "REML")
## '
## '   # Model 2: Add quadratic term
## '   g2 <- gls(Reaction ~ Days + Days2,
## '             data = sleepstudy,
## '             method = "REML")
## ' }
## ' 
## ' PBmodcomp(g1, g0)
## '
## ' PBmodcomp(fm1, fm0)


#' @export
#' @rdname pb__modcomp
PBmodcomp.merMod <- function(largeModel, smallModel, nsim=1000, ref=NULL, seed=NULL, cl=NULL, details=0){

    M <- get_nested_model_info(largeModel, smallModel)

    ## Specific for object class:
    nr_data <- nrow(getData(M$largeModel))
    nr_fit  <- getME(largeModel, "n")

    ## Generic across object classes
        
    if (nr_data != nr_fit)
        stop("Number of rows in data and fit do not match; remove NAs from data before fitting\n")

    if (is.null(ref)){
        ref <- PBrefdist(M$largeModel, M$smallModel, nsim=nsim,
                         seed=seed, cl=cl, details=details)
    }
    
    LRTstat     <- getLRT(M$largeModel, M$smallModel)
    out         <- .finalizePB(LRTstat, ref)
    out <- .padPB(out, LRTstat, ref, M$formula.large, M$formula.small)
    return(out)
}


#' @export
#' @rdname pb__modcomp
PBmodcomp.lm <- function(largeModel, smallModel, nsim=1000, ref=NULL, seed=NULL, cl=NULL, details=0){


    M <- get_nested_model_info(largeModel, smallModel)

    ## Specific for object class:
    
    ok.fam <- c("binomial", "gaussian", "Gamma", "inverse.gaussian", "poisson")    
    fam.l <- family(M$largeModel)
    fam.s <- family(M$smallModel)
    
    if (!all.equal(fam.l, fam.s))
        stop("Models do not have identical identical family\n")

    if (!(fam.l$family %in% ok.fam))
        stop(sprintf("family must be of type %s", toString(ok.fam)))

    nr_data <- nrow(eval(M$largeModel$call$data))
    nr_fit  <- nrow(M$largeModel$model)

    ## Generic across object classes
    
    if (nr_data != nr_fit)
      stop("Number of rows in data and fit do not match; remove NAs from data before fitting\n")

        
    if (is.null(ref)){
        ref <- PBrefdist(M$largeModel, M$smallModel, nsim=nsim, seed=seed, cl=cl, details=details)
    }
    
    LRTstat     <- getLRT(M$largeModel, M$smallModel)
    out         <- .finalizePB(LRTstat, ref)
    out <- .padPB(out, LRTstat, ref, M$formula.large, M$formula.small)
    return(out)
}

#' @export
#' @rdname pb_modcomp
PBmodcomp.gls <- function(largeModel, smallModel, nsim=1000, ref=NULL, seed=NULL, cl=NULL, details=0){

    M <- get_nested_model_info(largeModel, smallModel)

    ## Specific for object class:
    nr_data <- nrow(getData(largeModel))
    nr_fit  <- largeModel$dims$N
    
    if (nr_data != nr_fit)
        stop("Number of rows in data and fit do not match; remove NAs from data before fitting\n")

    ## Generic across object classes
    
    if (is.null(ref)){
        ref <- PBrefdist(M$largeModel, M$smallModel, nsim=nsim,
                         seed=seed, cl=cl, details=details)
    }
    
    LRTstat     <- getLRT(largeModel, smallModel)
    out         <- .finalizePB(LRTstat, ref)
    out <- .padPB(out, LRTstat, ref, M$formula.large, M$formula.small)
    return(out)
}




PBcompute_p_values <- function(LRTstat, ref){

    tobs <- unname(LRTstat[1])
    ndf  <- unname(LRTstat[2])
##rr <<- ref
    refpos   <- ref[ref > 0]
    nsim <- length(ref)
    npos <- length(refpos)
    
    EE      <- mean(refpos)
    VV      <- var(refpos)
    
    ##cat(sprintf("EE=%f VV=%f\n", EE, VV))
    p.chi <- 1 - pchisq(tobs, df=ndf)
    
    ## Direct computation of tail probability
    n.extreme <- sum(tobs < refpos)
    ##p.PB  <- n.extreme / npos
    p.PB  <- (1 + n.extreme) / (1 + npos)
    
    p.PB.all  <- (1 + n.extreme) / (1 + nsim)    
    
    se <- round(sqrt(p.PB * (1 - p.PB) / npos), 4)
    ci <- round(c(-1.96, 1.96) * se + p.PB, 4)

    ## Kernel density estimate
    ##dd <- density(ref)
    ##p.KD <- sum(dd$y[dd$x>=tobs])/sum(dd$y)
    
    ## Bartlett correction - X2 distribution
    BCstat  <- ndf * tobs / EE
    ##cat(sprintf("BCval=%f\n", ndf/EE))
    p.BC    <- 1 - pchisq(BCstat,df=ndf)
    
    ## Fit to gamma distribution
    scale   <- VV / EE
    shape   <- EE^2 / VV
    p.Ga    <- 1 - pgamma(tobs, shape=shape, scale=scale)
    
    ## Fit T/d to F-distribution (1. moment)
    ## ddf  <- 2 * EE / (EE - 1)
    EE2 <- EE / ndf
    ddf  <- 2 * EE2 / (EE2 - 1)
    ## cat("EE:\n"); print(EE); print(ddf)
    
    Fobs <- tobs/ndf
    if (ddf > 2)
        p.FF <- 1 - pf(Fobs, df1=ndf, df2=ddf)
    else
        p.FF <- NA
    
    test = list(
        LRT      = c(stat=tobs,    df=ndf, ddf=NA,   p.value=p.chi),
        PBtest   = c(stat=tobs,    df=NA,  ddf=NA,   p.value=p.PB),
        PB_Ftest = c(stat=Fobs,    df=ndf, ddf=ddf,  p.value=p.FF),        
        Gamma    = c(stat=tobs,    df=NA,  ddf=NA,   p.value=p.Ga),
        Bartlett = c(stat=BCstat,  df=ndf, ddf=NA,   p.value=p.BC)
    )

    test <- as.data.frame(do.call(rbind, test))
    
    out <- list(
        test    =test,
        type    ="X2test",
        moment  = c(mean=EE, var=VV),
        samples = c(nsim=nsim, npos=npos),
        gamma   = c(scale=scale, shape=shape),
        ref     = ref,
        LRTstat = LRTstat,
        ci      = ci,
        se      = se,
        n.extreme = n.extreme,
        ctime  = attr(ref, "ctime")
    )
    out
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
  p.PB  <- (1 + n.extreme) / (1 + npos)

  test = list(
    LRT      = c(stat=tobs,    df=ndf,    p.value=p.chi),
    PBtest   = c(stat=tobs,    df=NA,     p.value=p.PB))

  test  <- as.data.frame(do.call(rbind, test))
  ans   <- list(test=test, type="X2test", samples=c(nsim=nsim, npos=npos), n.extreme=n.extreme,
                ctime=attr(ref,"ctime"))
  class(ans) <- c("PBmodcomp")
  ans
}


.padPB <- function(ans, LRTstat, ref, formula.large, formula.small){
    ans$LRTstat <- LRTstat
    ans$ref     <- ref
    ans$formula.large <- formula.large
    ans$formula.small <- formula.small
    ans
}


    
#' @rdname pb__modcomp
seqPBmodcomp <-
    function(largeModel, smallModel, h = 20, nsim = 1000, cl=1) {
        t.start <- proc.time()
        chunk.size <- 200
        nchunk <- nsim %/% chunk.size
        LRTstat <- getLRT(largeModel, smallModel)
        ref <- NULL
        for (ii in 1:nchunk) {
            ref <- c(ref, PBrefdist(largeModel, smallModel, nsim = chunk.size, cl=cl))
            n.extreme <- sum(ref > LRTstat["tobs"])
            if (n.extreme >= h)
                break
        }
        ans <- PBmodcomp(largeModel, smallModel, ref = ref)
        ans$ctime <- (proc.time() - t.start)[3]
        ans
    }




### dot-functions below here


.summarizePB <- function(LRTstat, ref){

  tobs <- unname(LRTstat[1])
  ndf  <- unname(LRTstat[2])

  refpos   <- ref[ref > 0]
  nsim <- length(ref)
  npos <- length(refpos)

  EE      <- mean(refpos)
  VV      <- var(refpos)

  ##cat(sprintf("EE=%f VV=%f\n", EE, VV))
  p.chi <- 1 - pchisq(tobs, df=ndf)

  ## Direct computation of tail probability
  n.extreme <- sum(tobs < refpos)
  ##p.PB  <- n.extreme / npos
  p.PB  <- (1 + n.extreme) / (1 + npos)

  p.PB.all  <- (1 + n.extreme) / (1 + nsim)


  se <- round(sqrt(p.PB * (1 - p.PB) / npos), 4)
  ci <- round(c(-1.96, 1.96) * se + p.PB, 4)

  ## Kernel density estimate
  ##dd <- density(ref)
  ##p.KD <- sum(dd$y[dd$x>=tobs])/sum(dd$y)

  ## Bartlett correction - X2 distribution
  BCstat  <- ndf * tobs / EE
  ##cat(sprintf("BCval=%f\n", ndf/EE))
  p.BC    <- 1 - pchisq(BCstat,df=ndf)

  ## Fit to gamma distribution
  scale   <- VV / EE
  shape   <- EE^2 / VV
  p.Ga    <- 1 - pgamma(tobs, shape=shape, scale=scale)

  ## Fit T/d to F-distribution (1. moment)

  ## FIXME: Think the formula is 2*EE/(EE-1)
  ##ddf  <- 2*EE/(EE-ndf)
  ddf  <- 2 * EE / (EE - 1)
  Fobs <- tobs/ndf
  if (ddf > 0)
      p.FF <- 1 - pf(Fobs, df1=ndf, df2=ddf)
  else
      p.FF <- NA

  ## Fit T/d to F-distribution (1. AND 2. moment)
  ## EE2   <- EE/ndf
  ## VV2   <- VV/ndf^2

  ## rho   <- VV2/(2*EE2^2)
  ## ddf2  <- 4 + (ndf+2)/(rho*ndf-1)
  ## lam2  <- (ddf/EE2*(ddf-2))
  ## Fobs2 <- lam2 * tobs/ndf
  ## if (ddf2>0)
  ##   p.FF2 <- 1-pf(Fobs2, df1=ndf, df2=ddf2)
  ## else
  ##   p.FF2 <- NA

  ## cat(sprintf("PB: EE=%f, ndf=%f VV=%f, ddf=%f\n", EE, ndf, VV, ddf))


    test = list(
        LRT      = c(stat=tobs,    df=ndf, ddf=NA,   p.value=p.chi),
        PBtest   = c(stat=tobs,    df=NA,  ddf=NA,   p.value=p.PB),
        Gamma    = c(stat=tobs,    df=NA,  ddf=NA,   p.value=p.Ga),
        Bartlett = c(stat=BCstat,  df=ndf, ddf=NA,   p.value=p.BC),
        F        = c(stat=Fobs,    df=ndf, ddf=ddf,  p.value=p.FF)
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

    cat(sprintf("Bootstrap test; "))
    if (!is.null((zz<- x$ctime))){
        cat(sprintf("time: %.2f sec;", round(zz,2)))
    }
    if (!is.null((sam <- x$samples))){
        cat(sprintf(" samples: %d; extremes: %d;", sam[1], x$n.extreme))
    }
    cat("\n")
    
    if (!is.null((sam <- x$samples))){
        if (sam[2] < sam[1]){
            cat(sprintf("Requested samples: %d Used samples: %d Extremes: %d\n",
                        sam[1], sam[2], x$n.extreme))
        }
    }
    
    if (!is.null(x$formula.large)){
        cat("large : "); print(x$formula.large)
    }
    
    if (!is.null(x$formula.small)) {
        if (inherits(x$formula.small, "formula")) {
            cat("small : ")
        }
        else if (inherits(x$formula.small, "matrix")){
            cat("small : \n"); 
            print(x$formula.small)            
        }    
    }
}

#' @export
summary.PBmodcomp <- function(object, ...){
  ans <- .summarizePB(object$LRTstat, object$ref)
  ans$formula.large <- object$formula.large
  ans$formula.small <- object$formula.small
  class(ans) <- "summary_PBmodcomp"
  ans
}

#' @export
print.PBmodcomp <- function(x, ...){
  .PBcommon(x)
  tab <- x$test
  printCoefmat(tab, tst.ind=1, na.print='', has.Pvalue=TRUE)
  return(invisible(x))
}


#' @export
print.summary_PBmodcomp <- function(x, ...){
  .PBcommon(x)
  tab <- x$test
  printCoefmat(tab, tst.ind=1, na.print='', has.Pvalue=TRUE)
  cat("\n")
  return(invisible(x))
}


#' @export
as.data.frame.XXmodcomp <- function(x, row.names = NULL, optional = FALSE, ...){
    as.data.frame(do.call(rbind, x[-c(1:3)]))
}


##   ci <- x$ci
##   cat(sprintf("95 pct CI for PBtest   : [%s]\n", toString(ci)))
##   mo <- x$moment
##   cat(sprintf("Reference distribution : mean=%f var=%f\n", mo[1], mo[2]))
##   ga <- x$gamma
##   cat(sprintf("Gamma approximation    : scale=%f shape=%f\n", ga[1], ga[2]))



#' @export
plot.PBmodcomp <- function(x, ...){

  ref <-x$ref

  ndf  <- x$test$df[1]
  u    <-summary(x)
  ddf  <-u$test['F','ddf']

  EE   <- mean(ref)
  VV   <- var(ref)
  sc   <- var(ref) / mean(ref)
  sh   <- mean(ref)^2 / var(ref)
  sc   <- VV / EE
  sh   <- EE^2 / VV
  B    <- ndf / EE # if ref is the null distr, so should A*ref follow a chisq(df=ndf) distribution

  upper <- 0.20
  #tail.prob <- c(0.0001, 0.001, 0.01, 0.05, 0.10, 0.20, 0.5)
  tail.prob <-seq(0.001, upper, length.out = 1111)
  PBquant   <- quantile(ref, 1 - tail.prob) ## tail prob for PB dist

  pLR       <- pchisq(PBquant, df=ndf,             lower.tail=FALSE)
  pF        <- pf(PBquant / ndf, df1=ndf, df2=ddf, lower.tail=FALSE)
  pGamma    <- pgamma(PBquant, scale=sc, shape=sh, lower.tail=FALSE)
  pBart     <- pchisq(B * PBquant, df=ndf,         lower.tail=FALSE)

  sym.vec <- c(2,4,5,6)
  lwd     <- 2
  plot(pLR~tail.prob,type='l', lwd=lwd, #log="xy",
       xlab='Nominal p-value', ylab='True p-value',
       xlim=c(1e-3, upper), ylim=c(1e-3, upper),
       col=sym.vec[1], lty=sym.vec[1])
  lines(pF~tail.prob,lwd=lwd,     col=sym.vec[2], lty=sym.vec[2])
  lines(pBart~tail.prob,lwd=lwd,  col=sym.vec[3], lty=sym.vec[3])
  lines(pGamma~tail.prob,lwd=lwd, col=sym.vec[4], lty=sym.vec[4])
  abline(c(0,1))

  ZF     <-bquote(paste("F(1,",.(round(ddf,2)),")"))
  Zgamma <-bquote(paste("gamma(scale=",.(round(sc,2)),", shape=", .(round(sh,2)),")" ))
  ZLRT   <-bquote(paste(chi[.(ndf)]^2))
  ZBart  <-bquote(paste("Bartlett scaled ", chi[.(ndf)]^2))

  legend(0.001,upper,legend=as.expression(c(ZLRT, ZF, ZBart, Zgamma)),
         lty=sym.vec,col=sym.vec,lwd=lwd)
}








## #' @export
## PBmodcomp.mer <- PBmodcomp.merMod




## #' @export
## #' @rdname pb__modcomp
## PBFmodcomp <- function(largeModel, smallModel,
##                        nsim=500, ref=NULL, seed=NULL, cl=NULL,
##                        details=0){
##     ## if (is.null(control$nsim)) control$nsim <- 1000
##     ans <- PBmodcomp(largeModel, smallModel,
##                      nsim=nsim, ref=ref, seed=seed, cl=cl,
##                      details=details)
##     ## heading <- attr(ans, "heading")
##     ## out <- attr(ans, "aux")$test["PB_Ftest",]
##     ## attr(out, "heading") <- heading
##     ## attr(out, "aux") <- attr(ans, "aux")

##     class(out) <- c("PBFmodcomp", "anova", "data.frame")    
##     out

## }



    ## PBmodcomp.lm
    ## if (is.character(smallModel))
    ##     smallModel <- doBy::formula_add_str(formula(largeModel), terms=smallModel, op="-")

    ## if (inherits(smallModel, "formula"))
    ##     smallModel  <- update(largeModel, smallModel)

    ## if (is.numeric(smallModel) && !is.matrix(smallModel))
    ##     smallModel <- matrix(smallModel, nrow=1)
    
    ## formula.large <- formula(largeModel)
    ## attributes(formula.large) <- NULL
    
    ## if (inherits(smallModel, c("Matrix", "matrix"))){
    ##     formula.small <- smallModel
    ##     smallModel <- restriction_matrix2model(largeModel, smallModel)
    ## } else {
    ##     formula.small <- formula(smallModel)
    ##     attributes(formula.small) <- NULL
    ## }

    ## PBmodcomp.merMod
    ## if (is.character(smallModel))
    ##     smallModel <- doBy::formula_add_str(formula(largeModel), terms=smallModel, op="-")

    ## if (inherits(smallModel, "formula"))
    ##     smallModel  <- update(largeModel, smallModel, control=lmerControl(check.conv.singular = "ignore"))

    ## if (is.numeric(smallModel) && !is.matrix(smallModel))
    ##     smallModel <- matrix(smallModel, nrow=1)
            
    ## if (inherits(smallModel, c("Matrix", "matrix"))){
    ##     formula.small <- smallModel
    ##     smallModel <- restriction_matrix2model(largeModel, smallModel, REML=FALSE)
    ## } else {
    ##     formula.small <- formula(smallModel)
    ##     attributes(formula.small) <- NULL
    ## }

    ## formula.large <- formula(largeModel)
    ## attributes(formula.large) <- NULL
    
    ## All computations are based on 'largeModel' and 'smallModel'
    ## which at this point are both model objects.
    ## -----------------------------------------------------------


    ## cat("ref\n"); print(ref)
    ## largeModel <<- largeModel
    ## smallModel <<- smallModel
    
    ## dd <- logLik(largeModel) - logLik(smallModel)
    ## cat("dd:\n"); print(dd)

    ## ll.small <- logLik(smallModel, REML=FALSE)
    ## ll.large <- logLik(largeModel, REML=FALSE)
    ## dd <- ll.large - ll.small
    ## cat("dd:\n"); print(dd)
