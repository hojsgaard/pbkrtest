#' @title Model comparison using parametric bootstrap methods.
#' 
#' @description Model comparison of nested models using parametric bootstrap
#'     methods.  Implemented for some commonly applied model types.
#' @concept model_comparison
#' @name pb_modcomp
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
#' @aliases PBmodcomp PBmodcomp.lm PBmodcomp.merMod getLRT getLRT.lm
#'     getLRT.merMod plot.XXmodcomp PBmodcomp.mer getLRT.mer
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
#' @param control list with named arguments
## #' @param ... Additional arguments, currently not used.
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
#' (lmer0 <- lmer(Reaction ~ (Days|Subject), sleepstudy))
#' (lmer1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
#' (lmer2 <- lmer(Reaction ~ Days + I(Days^2) + (Days|Subject), sleepstudy))
#'
#' NSIM <- 50 ## Simulations in parametric bootstrap; default is 1000.
#' 
#' ## Test for no effect of Days in lmer1, i.e. test lmer0 under lmer1
#' PBmodcomp(lmer1, "Days", control=list(nsim=NSIM))
#' PBmodcomp(lmer1, ~.-Days, control=list(nsim=NSIM))
#' L1 <- cbind(0, 1) 
#' ## PBmodcomp(lmer1, L1, control=list(nsim=NSIM)) ## FIXME
#' PBmodcomp(lmer1, lmer0, control=list(nsim=NSIM))
#' anova(lmer1, lmer0)
#'
#' ## Test for no effect of Days and Days-squared in lmer2, i.e. test lmer0 under lmer2
#' PBmodcomp(lmer2, "(Days+I(Days^2))", control=list(nsim=NSIM))
#' PBmodcomp(lmer2, ~. - Days - I(Days^2), control=list(nsim=NSIM))
#' L2 <- rbind(c(0, 1, 0), c(0, 0, 1))
#' ## PBmodcomp(lmer2, L2, control=list(nsim=NSIM)) ## FIXME
#' PBmodcomp(lmer2, lmer0, control=list(nsim=NSIM))
#' anova(lmer2, lmer0)
#'
#' 
#' ## Test for no effect of Days-squared in lmer2, i.e. test lmer1 under lmer2
#' PBmodcomp(lmer2, "I(Days^2)", control=list(nsim=NSIM))
#' PBmodcomp(lmer2, ~. - I(Days^2), control=list(nsim=NSIM))
#' L3 <- rbind(c(0, 0, 1))
#' ## PBmodcomp(lmer2, L3, control=list(nsim=NSIM)) ## FIXME
#' PBmodcomp(lmer2, lmer1, control=list(nsim=NSIM))
#' anova(lmer2, lmer1)
#'
#' 
#' ## Linear normal model:
#' sug <- lm(sugpct ~ block + sow + harvest, data=beets)
#' sug.h <- update(sug, .~. -harvest)
#' sug.s <- update(sug, .~. -sow)
#' 
#' PBmodcomp(sug, "harvest", control=list(nsim=NSIM))
#' PBmodcomp(sug, ~. - harvest, control=list(nsim=NSIM))
#' PBmodcomp(sug, sug.h, control=list(nsim=NSIM))
#' anova(sug, sug.h)
#' 
#' ## Generalized linear model
#' glm1 <- glm(ndead/ntotal ~ sex + log(dose), family=binomial,
#'           weight=ntotal, data=budworm)
#' glm10 <- update(glm1, .~. -sex)
#'
#' ### Test for no effect of sex
#' PBmodcomp(glm1, "sex", control=list(nsim=NSIM))
#' PBmodcomp(glm1, ~.-sex, control=list(nsim=NSIM))
#' ## PBmodcomp(glm1, cbind(0, 1, 0), control=list(nsim=NSIM)): FIXME
#' PBmodcomp(glm1, glm10, control=list(nsim=NSIM))
#' anova(glm1, glm10, test="Chisq")
#' }
#' 
#' ## Generalized linear mixed model (it takes a while to fit these)
#' 
#' \dontrun{
#' (gm1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
#'               data = cbpp, family = binomial))
#' (gm2 <- update(gm1, .~.-period))
#' 
#' PBmodcomp(gm1, "period", control=list(nsim=NSIM))
#' PBmodcomp(gm1, ~. -period, control=list(nsim=NSIM))
#' PBmodcomp(gm1, gm2, control=list(nsim=NSIM))
#' anova(gm1, gm2)
#' }
#' 
#' 
#' \dontrun{
#' ## Linear mixed effects model:
#' sug   <- lmer(sugpct ~ block + sow + harvest + (1|block:harvest),
#'               data=beets, REML=FALSE)
#' sug.h <- update(sug, .~. -harvest)
#' sug.s <- update(sug, .~. -sow)
#' 
#' anova(sug, sug.h)
#' PBmodcomp(sug, sug.h, control=list(nsim=NSIM))
#' PBmodcomp(sug, "harvest", control=list(nsim=NSIM))
#' 
#' anova(sug, sug.s)
#' PBmodcomp(sug, sug.s, control=list(nsim=NSIM))
#' PBmodcomp(sug, "sow", control=list(nsim=NSIM))
#'
#' ## Simulate reference distribution separately:
#' refdist <- PBrefdist(sug, sug.h, nsim=1000)
#' refdist <- PBrefdist(sug, "harvest", nsim=1000)
#' refdist <- PBrefdist(sug, ~.-harvest, nsim=1000)
#' 
#' ## Do computations with multiple processors:
#' ## Number of cores:
#' 
#' (nc <- detectCores())
#' ## Create clusters
#' cl <- makeCluster(rep("localhost", nc))
#' 
#' ## Then do:
#' refdist <- PBrefdist(sug, sug.h, control=list(nsim=1000, cl=cl))
#' 
#' ## It is recommended to stop the clusters before quitting R:
#' stopCluster(cl)
#' }
#' 
#' @export PBmodcomp


#' @export
#' @rdname pb_modcomp
PBFmodcomp <- function(largeModel, smallModel,
                       control=list(nsim=500, ref=NULL, seed=NULL, cl=NULL),
                       details=0){
    ans <- PBmodcomp(largeModel, smallModel, control=control,
                     details=details)
    heading <- attr(ans, "heading")
    out <- attr(ans, "aux")$test["PB_Ftest",]
    attr(out, "heading") <- heading
    attr(out, "aux") <- attr(ans, "aux")

    class(out) <- c("PBFmodcomp", "anova", "data.frame")    
    out

}

#' @export
#' @rdname pb_modcomp
PBmodcomp <- function(largeModel, smallModel,
                      control=list(nsim=1000, ref=NULL, seed=NULL, cl=NULL),
                      details=0){
  UseMethod("PBmodcomp")
}


handle_models <- function(largeModel, smallModel){


    if (is.character(smallModel))
        smallModel <- doBy::formula_add_str(formula(largeModel), terms=smallModel, op="-")
    
    if (inherits(smallModel, "formula"))
        smallModel  <- update(largeModel, smallModel)
    
    if (is.numeric(smallModel) && !is.matrix(smallModel))
        smallModel <- matrix(smallModel, nrow=1)
    
    if (inherits(smallModel, c("Matrix", "matrix"))){
        formula.small <- smallModel
        smallModel <- restriction_matrix2model(largeModel, smallModel, REML=FALSE)
    } else {
        formula.small <- formula(smallModel)
        attributes(formula.small) <- NULL
    }
    
    formula.large <- formula(largeModel)
    attributes(formula.large) <- NULL

    out <- list(formula.large=formula.large,
                formula.small=formula.small,
                largeModel=largeModel,
                smallModel=smallModel
                )

    return(out)

}




#' @export
#' @rdname pb_modcomp
PBmodcomp.merMod <- function(largeModel, smallModel,
                             control=list(nsim=1000, ref=NULL, seed=NULL, cl=NULL),
                             details=0){

    

    mmm <- handle_models(largeModel, smallModel)
    largeModel <- mmm$largeModel
    smallModel <- mmm$smallModel
    formula.large <- mmm$formula.large
    formula.small <- mmm$formula.small
    
    nr_data <- nrow(getData(largeModel))
    nr_fit  <- getME(largeModel, "n")
    
    if (nr_data != nr_fit)
        stop("Number of rows in data and fit do not match; remove NAs from data before fitting\n")

    ref <- control$ref
    if (is.null(ref)){
        ref <- PBrefdist(largeModel, smallModel, control=control,
                         details=details)
    }
    
    LRTstat     <- getLRT(largeModel, smallModel)
    ans         <- PBcompute_p_values(LRTstat, ref)

    ans$formula.large <- formula.large
    ans$formula.small <- formula.small

    out <- ans$test[2,, drop=FALSE]
    attr(out, "aux") <- ans

    attr(out, "heading") <- c(
        deparse(formula.large),
        deparse(formula.small))

    class(out) <- c("PBmodcomp", "anova", "data.frame")
    return(out)
}


#' @export
#' @rdname pb_modcomp
PBmodcomp.gls <- function(largeModel, smallModel,
                          control=list(nsim=1000, ref=NULL, seed=NULL, cl=NULL),
                          details=0){



    mmm <- handle_models(largeModel, smallModel)
    largeModel <- mmm$largeModel
    smallModel <- mmm$smallModel
    formula.large <- mmm$formula.large
    formula.small <- mmm$formula.small

    nr_data <- nrow(getData(largeModel))
    nr_fit  <- largeModel$dims$N
    
    if (nr_data != nr_fit)
        stop("Number of rows in data and fit do not match; remove NAs from data before fitting\n")

    ref <- control$ref    
    if (is.null(ref)){
        ref <- PBrefdist(largeModel, smallModel, control=control,
                         details=details)
    }
    
    LRTstat     <- getLRT(largeModel, smallModel)
    ans         <- PBcompute_p_values(LRTstat, ref)

    ans$formula.large <- formula.large
    ans$formula.small <- formula.small

    out <- ans$test[2,, drop=FALSE]
    attr(out, "aux") <- ans

    attr(out, "heading") <- c(
        deparse(formula.large),
        deparse(formula.small))

    class(out) <- c("PBmodcomp", "anova", "data.frame")
    return(out)
}


#' @export
#' @rdname pb_modcomp
PBmodcomp.lm <- function(largeModel, smallModel,
                         control=list(nsim=1000, ref=NULL, seed=NULL, cl=NULL),
                         details=0){

    ok.fam <- c("binomial", "gaussian", "Gamma", "inverse.gaussian", "poisson")



    mmm <- handle_models(largeModel, smallModel)
    largeModel <- mmm$largeModel
    smallModel <- mmm$smallModel
    formula.large <- mmm$formula.large
    formula.small <- mmm$formula.small
    
    
    if (!all.equal((fam.l <- family(largeModel)), (fam.s <- family(smallModel))))
        stop("Models do not have identical identical family\n")

    if (!(fam.l$family %in% ok.fam))
        stop(sprintf("family must be of type %s", toString(ok.fam)))

    ref <- control$ref
    if (is.null(ref)){
        ref <- PBrefdist(largeModel, smallModel, control=control,
                         details=details)
    } 

    nr_data <- nrow(eval(largeModel$call$data))
    nr_fit  <- nrow(largeModel$model)
    
    if (nr_data != nr_fit)
      stop("Number of rows in data and fit do not match; remove NAs from data before fitting\n")
    
        
    LRTstat     <- getLRT(largeModel, smallModel)
    ans         <- PBcompute_p_values(LRTstat, ref)

    
    ans$formula.large <- formula.large
    ans$formula.small <- formula.small

    out <- ans$test[2,]
    ## print(out) ##HHHHHHHHHHHHHH
    attr(out, "aux") <- ans

    attr(out, "heading") <- c(
        deparse(formula.large),
        deparse(formula.small))

    class(out) <- c("PBmodcomp", "anova", "data.frame")
    return(out)

}









### ###########################################################
###
### Utilities
###
### ###########################################################


#' @export
summary.PBmodcomp <- function(object, ...){

    out <- attr(object, "aux")$test[c("LRT", "PBtest", "F"),]
    
    attr(out, "aux") <- attr(object, "aux")
    attr(out, "heading") <- c(
        deparse(attr(object, "aux")$formula.large),
        deparse(attr(object, "aux")$formula.small))
    
    class(out) <- c("summary_PBmodcomp", "anova", "data.frame")
    out
}

#' @export
summary.PBFmodcomp <- function(object, ...){

    out <- attr(object, "aux")$test[c("LRT", "F"),]
    
    attr(out, "aux") <- attr(object, "aux")
    attr(out, "heading") <- c(
        deparse(attr(object, "aux")$formula.large),
        deparse(attr(object, "aux")$formula.small))
    
    class(out) <- c("summary_PBmodcomp", "anova", "data.frame")
    out
}

#' @export
print.PBmodcomp <- function(x, ...){

    ctime <- attr(x,"aux")$ctime
    n.extreme <- attr(x,"aux")$n.extreme
    n.pos <- attr(x,"aux")$samples[2]
    
    if (!is.null(heading <- attr(x, "heading"))){
        ss <- sprintf("Parametric bootstrap test; n.ext: %d, n.boot: %d, time: %.1f sec",
                      n.extreme, n.pos, ctime)
        heading <- c(ss, heading)
        cat(heading, sep = "\n")
    }
    
    printCoefmat(x[1,,drop=FALSE], tst.ind=1, na.print='', has.Pvalue=TRUE)
    return(invisible(x))
}

#' @export
print.PBFmodcomp <- function(x, ...){

    ctime <- attr(x,"aux")$ctime
    
    if (!is.null(heading <- attr(x, "heading"))){
        ss <- sprintf("Parametric bootstrap F-test; time: %.1f sec",
                      ctime)
        heading <- c(ss, heading)
        cat(heading, sep = "\n")
    }
    
    printCoefmat(x[1,,drop=FALSE], tst.ind=1, na.print='', has.Pvalue=TRUE)
    return(invisible(x))
}

#' @export
print.summary_PBmodcomp <- function(x, ...){

    ctime <- attr(x,"aux")$ctime
    n.extreme <- attr(x,"aux")$n.extreme
    n.pos <- attr(x,"aux")$samples[2]
    
    if (!is.null(heading <- attr(x, "heading"))){
        ss <- sprintf("Parametric bootstrap test; n.ext: %d, n.boot: %d, time: %.1f sec",
                      n.extreme, n.pos, ctime)

        heading <- c(ss, heading)
        cat(heading, sep = "\n")
    }
    
    printCoefmat(x, tst.ind=1, na.print='', has.Pvalue=TRUE)
    cat("\n")
    return(invisible(x))
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



PBheader <- function(x){

  print.default(names(x))
  aux <- attr(x, "aux")
    cat(sprintf("Bootstrap test...; "))
    if (!is.null((zz<- aux$ctime))){
        cat(sprintf("time: %.2f sec;", round(zz,2)))
    }
    
    if (!is.null((sam <- aux$samples))){
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
            print(x$formula.large)
        }
        else if (inherits(x$formula.small, "matrix")){
            cat("small : \n"); 
            print(x$formula.small)            
        }    
    }
}




## #' @export
## summary.PBmodcomp <- function(object, ...){
##   ans <- PBcompute_p_values(object$LRTstat, object$ref)
##   ans$formula.large <- object$formula.large
##   ans$formula.small <- object$formula.small
##   class(ans) <- "summary_PBmodcomp"
##   ans
## }



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





### dot-functions below here





#' @rdname pb_modcomp
#' @export
seqPBmodcomp <-
    function(largeModel, smallModel, control=list(nsim = 1000, h = 20, cl=NULL)) {
        t.start <- proc.time()
        chunk.size <- 100
        nsim <- control$nsim
        h <- control$h
        nchunk <- nsim %/% chunk.size
        LRTstat <- getLRT(largeModel, smallModel)
        ref <- NULL
        ctrl2 <- control
        ctrl2$nsim <- chunk.size
        for (ii in 1:nchunk) {
            ref <- c(ref, PBrefdist(largeModel, smallModel,
                                    control=ctrl2))
            n.extreme <- sum(ref > LRTstat["tobs"])
            if (n.extreme >= h)
                break
        }

        ## FIXME Needs update / check
        ans <- PBmodcomp(largeModel, smallModel, control=list(ref = ref))
        ##ans$ctime <- (proc.time() - t.start)[3]
        attr(ans, "aux")$ctime <- (proc.time() - t.start)[3]
        ans
    }


































#' #' @export
#' print.summary_PBmodcomp <- function(x, ...){
#'   PBheader(x)
#'   tab <- x$test
#'   printCoefmat(tab, tst.ind=1, na.print='', has.Pvalue=TRUE)
#'   cat("\n")
#'   return(invisible(x))
#' }


##   ci <- x$ci
##   cat(sprintf("95 pct CI for PBtest   : [%s]\n", toString(ci)))
##   mo <- x$moment
##   cat(sprintf("Reference distribution : mean=%f var=%f\n", mo[1], mo[2]))
##   ga <- x$gamma
##   cat(sprintf("Gamma approximation    : scale=%f shape=%f\n", ga[1], ga[2]))


    ## cat("ref\n"); print(ref)
    ## largeModel <<- largeModel
    ## smallModel <<- smallModel
    
    ## dd <- logLik(largeModel) - logLik(smallModel)
    ## cat("dd:\n"); print(dd)

    ## ll.small <- logLik(smallModel, REML=FALSE)
    ## ll.large <- logLik(largeModel, REML=FALSE)
    ## dd <- ll.large - ll.small
    ## cat("dd:\n"); print(dd)



## #' @export
## PBmodcomp.mer <- PBmodcomp.merMod






    ##          PBkd     = c(stat=tobs,    df=NA,  ddf=NA,   p.value=p.KD),

    ##F2       = c(stat=Fobs2,   df=ndf, ddf=ddf2, p.value=p.FF2),
                                        #,
    #PBtest.all   = c(stat=tobs,    df=NA,  ddf=NA,   p.value=p.PB.all),
    #Bartlett.all = c(stat=BCstat.all,  df=ndf, ddf=NA,   p.value=p.BC.all)
    ##F2       = c(stat=Fobs2,   df=ndf,  p.value=p.FF2, ddf=ddf2)



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


## .finalizePB <- function(LRTstat, ref){

##   tobs <- unname(LRTstat[1])
##   ndf  <- unname(LRTstat[2])

##   refpos <- ref[ref>0]
##   nsim <- length(ref)
##   npos <- length(refpos)

##   ##cat(sprintf("EE=%f VV=%f\n", EE, VV))
##   p.chi <- 1 - pchisq(tobs, df=ndf)
##   ## Direct computation of tail probability

##   n.extreme <- sum(tobs < refpos)
##   p.PB  <- (1+n.extreme) / (1+npos)

##   test = list(
##     LRT      = c(stat=tobs,    df=ndf,    p.value=p.chi),
##     PBtest   = c(stat=tobs,    df=NA,     p.value=p.PB))

##   test  <- as.data.frame(do.call(rbind, test))
##   ans   <- list(test=test, type="X2test", samples=c(nsim=nsim, npos=npos), n.extreme=n.extreme,
##                 ctime=attr(ref,"ctime"))
##   class(ans) <- c("PBmodcomp")
##   ans
## }


  ## ans <- list(test=test, type="X2test",
  ##             moment = c(mean=EE, var=VV),
  ##             samples= c(nsim=nsim, npos=npos),
  ##             gamma  = c(scale=scale, shape=shape),
  ##             ref    = ref,
  ##             ci     = ci,
  ##             se     = se,
  ##             n.extreme = n.extreme,
  ##             ctime  = attr(ref, "ctime")
  ##             )
    
  ##   class(ans) <- c("PBmodcomp")
  ##   ans



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






## .padPB <- function(ans, LRTstat, ref, formula.large, formula.small){
##     ans$LRTstat <- LRTstat
##     ans$ref     <- ref
##     ans$formula.large <- formula.large
##     ans$formula.small <- formula.small
##     ans
## }


## #' @export
## #' @rdname pb_modcomp
## PBmodcomp_ <- function(largeModel, smallModel, control=NULL){
##     the_call <- match.call()
    
##     if (!is.null(control)){
##         print(control)
##         if (!is.null((val <- control$nsim))){
##             the_call$nsim <- val
##         }    
##         if (!is.null((val <- control$cl))){
##             the_call$cl <- val
##         }    
##     }
##     the_call$control <- NULL
##     the_call[[1]] <- as.name("PBmodcomp")
##     print(the_call)
##     eval(the_call)
## }

## #' @export
## #' @rdname pb_modcomp
## PBFmodcomp_ <- function(largeModel, smallModel, control=NULL){
##     ans <- PBmodcomp_(largeModel, smallModel, control=control)
##     heading <- attr(ans, "heading")
##     out <- attr(ans, "aux")$test["PB_Ftest",]
##     attr(out, "heading") <- heading
##     attr(out, "aux") <- attr(ans, "aux")

##     class(out) <- c("PBFmodcomp", "anova", "data.frame")    
##     out

## }



    ## cat("PBmodcomp.gls\n")
    
    ## if (is.character(smallModel))
    ##     smallModel <- doBy::formula_add_str(formula(largeModel), terms=smallModel, op="-")

    ## if (inherits(smallModel, "formula"))
    ##     smallModel  <- update(largeModel, smallModel)

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


## lmer
    ## if (is.character(smallModel))
    ##     smallModel <- doBy::formula_add_str(formula(largeModel), terms=smallModel, op="-")

    ## if (inherits(smallModel, "formula"))
    ##     smallModel  <- update(largeModel, smallModel)

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


    ## lm
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


