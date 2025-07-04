#' @title Calculate reference distribution using parametric bootstrap
#'
#' @description Calculate reference distribution of likelihood ratio statistic
#'     in mixed effects models using parametric bootstrap
#'
#' @concept model_comparison
#' @name pb-refdist
#'
#' @details The model \code{object} must be fitted with maximum likelihood
#'     (i.e. with \code{REML=FALSE}). If the object is fitted with restricted
#'     maximum likelihood (i.e. with \code{REML=TRUE}) then the model is
#'     refitted with \code{REML=FALSE} before the p-values are calculated. Put
#'     differently, the user needs not worry about this issue.
#'
#'     The argument 'cl' (originally short for 'cluster') is used for
#'     controlling parallel computations. 'cl' can be NULL (default),
#'     positive integer or a list of clusters.
#'
#' Special care must be taken
#'     on Windows platforms (described below) but the general picture
#'     is this:
#'
#'     The recommended way of controlling cl is to specify the
#'     component \code{pbcl} in options() with
#'     e.g. \code{options("pbcl"=4)}.
#'
#'     If cl is NULL, the function will look at if the pbcl has been set
#'     in the options list with \code{getOption("pbcl")}
#'
#'     If cl=N then N cores will be used in the computations. If cl is
#'     NULL then the function will look for
#'
#'
#' @aliases PBrefdist PBrefdist.merMod PBrefdist.lm
#'
#' @param largeModel A linear mixed effects model as fitted with the
#'     \code{lmer()} function in the \pkg{lme4} package. This model muse be
#'     larger than \code{smallModel} (see below).
#' @param smallModel A linear mixed effects model as fitted with the
#'     \code{lmer()} function in the \pkg{lme4} package. This model muse be
#'     smaller than \code{largeModel} (see above).
#' @param nsim The number of simulations to form the reference distribution.
#' @param seed Seed for the random number generation.
#'
#' @param cl Used for controlling parallel computations. See sections
#'     'details' and 'examples' below.
#'
#' @param details The amount of output produced. Mainly relevant for debugging
#'     purposes.
#' @return A numeric vector
#' @author Søren Højsgaard \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{PBmodcomp}}, \code{\link{KRmodcomp}}
#' @references Ulrich Halekoh, Søren Højsgaard (2014)., A Kenward-Roger
#'     Approximation and Parametric Bootstrap Methods for Tests in Linear Mixed
#'     Models - The R Package pbkrtest., Journal of Statistical Software,
#'     58(10), 1-30., \url{https://www.jstatsoft.org/v59/i09/}
#'
#' @keywords models inference
#' @examples
#'
#' data(beets)
#' head(beets)
#' beet0 <- lmer(sugpct ~ block + sow + harvest + (1|block:harvest), data=beets, REML=FALSE)
#' beet_no.harv <- update(beet0, . ~ . -harvest)
#' rd <- PBrefdist(beet0, beet_no.harv, nsim=20, cl=1)
#' rd
#' \dontrun{
#' ## Note: Many more simulations must be made in practice.
#'
#' # Computations can be made in parallel using several processors:
#'
#' # 1: On OSs that fork processes (that is, not on windows):
#' # --------------------------------------------------------
#'
#' if (Sys.info()["sysname"] != "Windows"){
#'   N <- 2 ## Or N <- parallel::detectCores()
#'
#' # N cores used in all calls to function in a session
#'   options("mc.cores"=N)
#'   rd <- PBrefdist(beet0, beet_no.harv, nsim=20)
#'
#' # N cores used just in one specific call (when cl is set,
#' # options("mc.cores") is ignored):
#'   rd <- PBrefdist(beet0, beet_no.harv, nsim=20, cl=N)
#' }
#'
#' # In fact, on Windows, the approach above also work but only when setting the
#' # number of cores to 1 (so there is to parallel computing)
#'
#' # In all calls:
#' # options("mc.cores"=1)
#' # rd <- PBrefdist(beet0, beet_no.harv, nsim=20)
#' # Just once
#' # rd <- PBrefdist(beet0, beet_no.harv, nsim=20, cl=1)
#'
#' # 2. On all platforms (also on Windows) one can do
#' # ------------------------------------------------
#' library(parallel)
#' N <- 2 ## Or N  <- detectCores()
#' clus <- makeCluster(rep("localhost", N))
#'
#' # In all calls in a session
#' options("pb.cl"=clus)
#' rd <- PBrefdist(beet0, beet_no.harv, nsim=20)
#'
#' # Just once:
#' rd <- PBrefdist(beet0, beet_no.harv, nsim=20, cl=clus)
#' stopCluster(clus)
#' }


#' @rdname pb-refdist
#' @export
PBrefdist <- function(largeModel, smallModel, nsim=1000, seed=NULL, cl=NULL, details=0){
    UseMethod("PBrefdist")
}


#' @rdname pb-refdist
#' @export
PBrefdist.lm <- function(largeModel, smallModel, nsim=1000, seed=NULL, cl=NULL, details=0){
  
    ## Specific for object class:

    nr_data <- nrow(eval(largeModel$call$data))
    nr_fit  <- nrow(largeModel$model)
    
    ## Generic across object classes
    
    t0 <- proc.time()

    if (nr_data != nr_fit)
        stop("Number of rows in data and fit do not match; remove NAs from data before fitting\n")
  
    ref <- do_sampling(largeModel, smallModel, nsim, cl, details)
    LRTstat     <- getLRT(largeModel, smallModel)
    ref <- finalize_refdist(LRTstat, ref, nsim)
    
    if (details>0)
        cat(sprintf("Reference distribution with %i samples; computing time: %5.2f secs. \n",
                    length(ref), attr(ref, "ctime")))
    
    ref
}



#' @rdname pb-refdist
#' @export
PBrefdist.merMod <- function(largeModel, smallModel, nsim=1000, seed=NULL, cl=NULL, details=0){

    ## Specific for object class:

    ctrl <- lmerControl(

        optCtrl = list(maxfun = 1e5, tol = 0.01),
        check.conv.grad = "ignore",
        check.conv.singular = "ignore",
        check.conv.hess = "ignore"  # optional
    )
    
    
    if (getME(smallModel, "is_REML")) {
        smallModel <- eval(bquote(update(.(smallModel),
                                         REML    = FALSE,
                                         control = .(ctrl))))

    }
    
    if (getME(largeModel, "is_REML")){
        largeModel <- eval(bquote(update(.(largeModel),
                                         REML    = FALSE,
                                         control = .(ctrl))))

    } 
    
    nr_data <- nrow(getData(largeModel))
    nr_fit  <- getME(largeModel, "n")
    
    ## Generic across object classes
    t0 <- proc.time()        

    if (nr_data != nr_fit)
        stop("Number of rows in data and fit do not match; remove NAs from data before fitting\n")

    LRTstat     <- getLRT(largeModel, smallModel)    
    ref <- do_sampling(largeModel, smallModel, nsim, cl, details)    
    ref <- finalize_refdist(LRTstat, ref, nsim)
    
    if (details > 0)
        cat(sprintf("Reference distribution with %5i samples; computing time: %5.2f secs. \n",
                    length(ref), attr(ref, "ctime")))
    
    ref
}


#' @rdname pb-refdist
#' @export
PBrefdist.gls <- function(largeModel, smallModel, nsim=1000, seed=NULL, cl=NULL, details=0){

    ## Specific for object class:
    
    smallModel <- update(smallModel, method="ML")
    largeModel <- update(largeModel, method="ML")

    nr_data <- nrow(getData(largeModel))
    nr_fit  <- largeModel$dims$N

    
    ## Generic across object classes

    t0 <- proc.time()
    
    if (nr_data != nr_fit)
      stop("Number of rows in data and fit do not match; remove NAs from data before fitting\n")
    

    ref <- do_sampling(largeModel, smallModel, nsim, cl, details)
    LRTstat     <- getLRT(largeModel, smallModel)
    ref <- finalize_refdist(LRTstat, ref, nsim)
    
    if (details > 0)
        cat(sprintf("Reference distribution with %5i samples; computing time: %5.2f secs. \n",
                    length(ref), attr(ref, "ctime")))
    
    ref
}


finalize_refdist <- function(LRTstat, ref, nsim){
    attr(ref, "stat")    <- LRTstat
    attr(ref, "samples") <- c(nsim=nsim, npos=sum(ref > 0),
                              n.extreme=sum(ref > LRTstat["tobs"]),
                              pPB=(1 + sum(ref > LRTstat["tobs"])) / (1 + sum(ref > 0)))
    class(ref) <- "refdist"
    return(ref)
}


#' @export
print.refdist <- function(x, n=6L, ...){
    cat("values: \n")
    print(head(x, n=n))
    cat("attributes: \n")
    print(attributes(x)[1:4])
    invisible(x)
}


get_refdist <- function(lg){
    UseMethod("get_refdist")
}

get_refdist.merMod <- function(lg){
    .get_refdist_merMod
}

get_refdist.lm <- function(lg){
    .get_refdist_lm
}

.get_refdist_lm <- function(lg, sm, nsim=20, seed=NULL,
                            simdata=simulate(sm, nsim=nsim, seed=seed)){
    ##simdata <- simulate(sm, nsim, seed=seed)
    ee  <- new.env()
    ee$simdata <- simdata

    ff.lg <- update.formula(formula(lg), simdata[, ii] ~ .)
    ff.sm <- update.formula(formula(sm), simdata[, ii] ~ .)
    environment(ff.lg) <- environment(ff.sm) <- ee

    cl.lg <- getCall(lg)
    cl.sm <- getCall(sm)

    cl.lg$formula <- ff.lg
    cl.sm$formula <- ff.sm

    if (inherits(lg, "glm")){
        cl.lg$start <- coef(lg)
        cl.sm$start <- coef(sm)
    }

    ref <- rep.int(NA, nsim)
    for (ii in 1:nsim){
        ref[ii] <- 2 * (logLik(eval(cl.lg)) - logLik(eval(cl.sm)))
    }
    ref
}

.get_refdist_merMod <- function(lg, sm, nsim=20, seed=NULL,
                                simdata=simulate(sm, nsim=nsim, seed=seed)){

    refit_safe <- function(model, newresp, ctrl) {
        ff <- formula(model)
        dd <- getData(model)
        dd[[as.character(ff[[2]])]] <- newresp  # Replace response
        update(model, formula = ff, data = dd, REML = FALSE, control = ctrl)
    }

    ctrl <- lmerControl(
        optCtrl = list(tol = 0.1),
        check.conv.grad = "ignore",
        check.conv.hess = "ignore",
        check.conv.singular = "ignore"
    )

    unname(unlist(lapply(simdata, function(yyy) {
        sm2 <- suppressMessages(refit_safe(sm, yyy, ctrl))
        lg2 <- suppressMessages(refit_safe(lg, yyy, ctrl))
        2 * (logLik(lg2, REML = FALSE) - logLik(sm2, REML = FALSE))
    })))    
}







get_cl <- function(cl){

    .cat <- function(b, ...) {if (b) cat(...)}
    dd <- 2
    
    if (Sys.info()["sysname"] == "Windows"){
        ##cat("We are on windows; setting cl=1\n")
        cl <- 1
    }
    
    if (!is.null(cl)){
        if (inherits(cl, "cluster") || (is.numeric(cl) && length(cl) == 1 && cl >= 1)){
            .cat(dd>3, "valid 'cl' specified in call \n")
        } else
            stop("invalid 'cl' specified in call \n")
    } else {
        .cat(dd>3, "trying to retrieve 'cl' from options('pb.cl') ... \n")
        cl <- getOption("pb.cl")
        if (!is.null(cl)){
            if (!inherits(cl, "cluster"))
                stop("option 'cl' set but is not a list of clusters\n")
            .cat(dd>3,"  got 'cl' from options; length(cl) = ", length(cl), "\n")
        }
        
        if (is.null(cl)){
            .cat(dd>3, "trying to retrieve 'cl' from options('mc.cores')... \n")
            cl <- getOption("mc.cores")
            if (!is.null(cl))
                .cat(dd>3,"  got 'cl' from options(mc.cores); cl = ", cl, "\n")
        }
    }
    
    if (is.null(cl)){
        .cat(dd > 3, "cl can not be retrieved anywhere; setting cl=1\n")
        cl <- 1
    }

    return(cl)    
}

do_sampling <- function(largeModel, smallModel, nsim, cl, details=0){

    t0  <- proc.time()
    .cat <- function(b, ...) {if (b) cat(...)}
    dd <- details

    get_fun <- get_refdist(largeModel)

    cl <- get_cl(cl)
        
    if (is.numeric(cl)){
        if (!(length(cl) == 1 && cl >= 1))
            stop("Invalid numeric cl\n")

        .cat(dd>3, "doing mclapply, cl = ", cl, "\n")

        nsim.cl <- nsim %/% cl
        ref <- unlist(mclapply(1:cl,
                               function(i) {
                                   get_fun(largeModel, smallModel, nsim=nsim.cl)},
                               mc.cores=cl))


    } else
        if (inherits(cl, "cluster")){
            .cat(dd>3, "doing clusterCall, nclusters = ", length(cl), "\n")
            nsim.cl <- nsim %/% length(cl)
            clusterSetRNGStream(cl)
            ref <- unlist(clusterCall(cl, fun=get_fun,
                                      largeModel, smallModel, nsim=nsim.cl))
        }
    else stop("Invalid 'cl'\n")

    attr(ref, "cl")  <- cl
    attr(ref, "ctime") <- (proc.time() - t0)[3]
    ref

}




## ALTERNATIV
## safe_update_lmer <- function(model, REML = FALSE, control = lmerControl()) {
##   eval(bquote(update(.(model),
##                      REML = .(REML),
##                      control = .(control))))
## }
## smallModel <- safe_update_lmer(smallModel, REML = FALSE, control = ctrl)

    ## smallModel <- update(smallModel, REML = FALSE, control = ctrl)

    ## smallModel <- eval(bquote(update(.(smallModel),
    ##                                  REML    = FALSE,
    ##                                  control = .(ctrl))))

        ## smallModel <- update(smallModel, REML = FALSE, control = ctrl)
        ## smallModel <- update(smallModel, REML=FALSE,
        ## control=lmerControl(check.conv.singular = "ignore"))

        ## largeModel <- update(largeModel, REML = FALSE, control = ctrl)
        ## largeModel <- update(largeModel, REML=FALSE,
                             ## control=lmerControl(check.conv.singular = "ignore"))  



### ###########################################################
###
### Computing of reference distribution; possibly in parallel
###
### ###########################################################

