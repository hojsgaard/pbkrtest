## FIXME: comodex er et dumt navn

## ' @title Model comparison
## ' 
## ' @description Wrapper for functions KRmodcomp, SATmodcomp, PBmodcomp, X2modcomp
## ' @name comodex
## ' @param largeModel A model object
## ' @param smallModel A model object, a formula or a restriction matrix
## ' @param test A character string
## ' @param control A list controlling the model comparions.
## ' @param ... Additional arguments to be passed on to other methods
## ' @param details should details be printed
## ' @author Søren Højsgaard
## '
## ' @examples
## ' (lmm0 <- lmer(Reaction ~ (Days|Subject), sleepstudy))
## ' (lmm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
## ' (lmm2 <- lmer(Reaction ~ Days + I(Days^2) + (Days|Subject), sleepstudy))
## '
## ' lm1 <- lm(dist ~ speed + I(speed^2), data=cars)
## ' lm0 <- lm(dist ~ speed, data=cars)
## ' 
## ' comodex(lmm2, lmm1, test="x2")
## ' comodex(lmm2, lmm1, test="kr")
## ' comodex(lmm2, lmm1, test="sat")
## ' comodex(lmm2, lmm1, test="PB", control=list(nsim=50, cl=1))
## ' comodex(lmm2, .~. - I(Days^2))
## '
## ' comodex(lm1, lm0)
## ' comodex(lm1, lm0, test="pb", control=list(nsim=50, cl=1))
## ' 

## ' @export
## ' @rdname comodex


## ' @rdname comodex
## ' @export




#' Simulate reference distribution of LR statistic via parametric bootstrap
#'
#' Simulates the null distribution of the likelihood ratio test statistic by
#' parametric bootstrap. Supports serial, parallel (parallel package), and
#' future-based parallel execution.
#'
#' @param fit1 Fitted larger model object with `refit` and `logLik` methods.
#' @param fit0 Fitted smaller (nested) model.
#' @param nsim Number of bootstrap replications. Default is 1000.
#' @param seed RNG seed for reproducibility (optional).
#' @param engine Parallelization engine. One of `"future"` (default), `"serial"`, `"parallel"`.
#' @param nworkers Number of workers/cores to use. Default is 2.
#' @param progress Logical. Show progress bar if available. Default is FALSE.
#'
#' @return Numeric vector of simulated LR statistics of length \code{nsim}.
#' @examples
#'
#' #' @examples
#'
#' lm_fit1 <- lm(Reaction ~ Days, data = sleepstudy)
#' lm_fit0 <- update(lm_fit1, . ~ . - Days)
#' set.seed(42)
#' lr_sim_lm <- pb_refdist(lm_fit1, lm_fit0, nsim = 500, engine = "future", nworkers =2)
#' 
#' gls_fit1 <- gls(Reaction ~ Days, data = sleepstudy, method="ML")
#' gls_fit0 <- update(gls_fit1, . ~ . - Days)
#' set.seed(42)
#' lr_sim_gls <- pb_refdist(gls_fit1, gls_fit0, nsim = 500, engine = "future", nworkers = 2)
#' 
#' lme_fit1 <- lme(Reaction ~ Days, random = ~ 1 | Subject, data = sleepstudy, method="ML")
#' lme_fit0 <- update(lme_fit1, . ~ . - Days)
#' set.seed(42)
#' lr_sim_lme <- pb_refdist(lme_fit1, lme_fit0, nsim = 500, engine = "future", nworkers = 2)
#'
#' lmer_fit1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy, REML=FALSE)
#' lmer_fit0 <- update(lmer_fit1, . ~ . - Days)
#' set.seed(42)
#' lr_sim_lmer <- pb_refdist(lmer_fit1, lmer_fit0, nsim = 500, engine = "future", nworkers = 2)
#' 
#' par(mfrow=c(2,2))
#' hist(lr_sim_lm, main = "Parametric bootstrap (lm)", xlab = "LR statistic")
#' hist(lr_sim_gls, main = "Parametric bootstrap (gls)", xlab = "LR statistic")
#' hist(lr_sim_lme, main = "Parametric bootstrap (lme)", xlab = "LR statistic")
#' hist(lr_sim_lmer, main = "Parametric bootstrap (lmer)", xlab = "LR statistic")
#'
#' @export
pb_refdist <- function(
  fit1,
  fit0,
  nsim = 1000,
  seed = NULL,
  engine = c("future", "serial", "parallel"),
  nworkers = 2,
  progress = FALSE
) {
  engine <- match.arg(engine)
  if (!is.null(seed)) set.seed(seed)

  ## Simulate responses under null
  simdata <- simulate(fit0, nsim = nsim, seed = seed)

  ## Define worker function
  ll_diff_worker <- function(y) {
    # Load nlme if needed on worker
    if (!"package:nlme" %in% search()) {
      suppressPackageStartupMessages(library(nlme))
    }
    ll1 <- logLik(refit(fit1, newresp = y))
    ll0 <- logLik(refit(fit0, newresp = y))
    as.numeric(ll1 - ll0)
  }

  ## SERIAL
  if (engine == "serial") {
    if (progress) message("Running serial bootstrap on 1 core")
    res <- vapply(simdata, ll_diff_worker, numeric(1))
    return(unname(res))
  }

  ## PARALLEL (parallel package)
  if (engine == "parallel") {
    if (progress) message(sprintf("Running parallel bootstrap on %d workers (parallel package)", nworkers))
    if (!requireNamespace("parallel", quietly = TRUE)) {
      stop("Package 'parallel' is required for engine='parallel'. Please install it.")
    }

    if (.Platform$OS.type == "windows") {
      cl <- parallel::makeCluster(nworkers)
      on.exit(parallel::stopCluster(cl))
      parallel::clusterExport(
        cl,
        varlist = c("fit1", "fit0", "ll_diff_worker", "refit", "logLik"),
        envir = environment()
      )
      res <- unlist(parallel::parLapply(cl, simdata, ll_diff_worker))
    } else {
      res <- unlist(parallel::mclapply(simdata, ll_diff_worker, mc.cores = nworkers))
    }
    return(unname(res))
  }

  ## FUTURE (default)
  if (engine == "future") {
    if (progress) message(sprintf("Running parallel bootstrap on %d workers (future)", nworkers))
    if (!requireNamespace("future", quietly = TRUE)) {
      stop("Package 'future' is required for engine='future'. Please install it.")
    }
    if (!requireNamespace("furrr", quietly = TRUE)) {
      stop("Package 'furrr' is required for engine='future'. Please install it.")
    }

    future::plan(future::multisession, workers = nworkers)

    res <- furrr::future_map_dbl(
                      simdata,
                      ll_diff_worker,
                      .options = furrr::furrr_options(
                                            seed = TRUE,
                                            packages = c("stats", "nlme", "lme4")
                                        ),
                      .progress = progress
                  )
    
    return(unname(res))
  }

  stop("Invalid engine specified.")
}




#' Summarize parametric bootstrap results for LRT
#'
#' Calculates asymptotic p-value, bootstrap p-value with CI, Bartlett corrections
#' (mean and median-based) and F-approximations (mean and median-based).
#'
#' @param LRTstat Numeric vector: c(tobs, df) = observed test statistic and df.
#' @param ref Numeric vector of simulated bootstrap test statistics.
#' @param conf.level Confidence level for bootstrap p CI (default 0.95).
#'
#' @return List with results table, moments, samples info, CI, SE, etc.
#'
#' @examples
#'
#' sleepstudy$Days2 <- sleepstudy$Days^2
#' 
#' lm_fit1 <- lm(Reaction ~ Days, data = sleepstudy)
#' lm_fit0 <- update(lm_fit1, . ~ . - Days)
#' set.seed(42)
#' lrt_lm <- getLRT(lm_fit1, lm_fit0)
#' lr_sim_lm <- pb_refdist(lm_fit1, lm_fit0, nsim = 500, engine = "future", nworkers =2)
#' summarize_pb(lrt_lm, lr_sim_lm)
#' 
#' gls_fit1 <- gls(Reaction ~ Days, data = sleepstudy, method="ML")
#' gls_fit0 <- update(gls_fit1, . ~ . - Days)
#' set.seed(42)
#' lrt_gls <- getLRT(gls_fit1, gls_fit0)
#' lr_sim_gls <- pb_refdist(gls_fit1, gls_fit0, nsim = 500, engine = "future", nworkers = 2)
#' summarize_pb(lrt_gls, lr_sim_gls)
#' 
#' lme_fit1 <- lme(Reaction ~ Days, random = ~ 1 | Subject, data = sleepstudy, method="ML")
#' lme_fit0 <- update(lme_fit1, . ~ . - Days)
#' set.seed(42)
#' lrt_lme <- getLRT(lme_fit1, lme_fit0)
#' lr_sim_lme <- pb_refdist(lme_fit1, lme_fit0, nsim = 500, engine = "future", nworkers = 2)
#' summarize_pb(lrt_lme, lr_sim_lme)
#' 
#' lmer_fit1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy, REML=FALSE)
#' lmer_fit0 <- update(lmer_fit1, . ~ . - Days)
#' set.seed(42)
#' lrt_lmer <- getLRT(lmer_fit1, lmer_fit0)
#' lr_sim_lmer <- pb_refdist(lmer_fit1, lmer_fit0, nsim = 500, engine = "future", nworkers = 2)
#' summarize_pb(lrt_lmer, lr_sim_lmer)
#'

#' sleepstudy$Days2 <- sleepstudy$Days^2
#' 
#' lm_fit1 <- lm(Reaction ~ Days + Days2, data = sleepstudy)
#' lm_fit0 <- update(lm_fit1, . ~ . - Days2)
#' set.seed(42)
#' lrt_lm <- getLRT(lm_fit1, lm_fit0)
#' lr_sim_lm <- pb_refdist(lm_fit1, lm_fit0, nsim = 500, engine = "future", nworkers =2)
#' summarize_pb(lrt_lm, lr_sim_lm)
#' 
#' gls_fit1 <- gls(Reaction ~ Days + Days2, data = sleepstudy, method="ML")
#' gls_fit0 <- update(gls_fit1, . ~ . - Days2)
#' set.seed(42)
#' lrt_gls <- getLRT(gls_fit1, gls_fit0)
#' lr_sim_gls <- pb_refdist(gls_fit1, gls_fit0, nsim = 500, engine = "future", nworkers = 2)
#' summarize_pb(lrt_gls, lr_sim_gls)
#' 
#' lme_fit1 <- lme(Reaction ~ Days + Days2, random = ~ 1 | Subject, data = sleepstudy, method="ML")
#' lme_fit0 <- update(lme_fit1, . ~ . - Days2)
#' set.seed(42)
#' lrt_lme <- getLRT(lme_fit1, lme_fit0)
#' lr_sim_lme <- pb_refdist(lme_fit1, lme_fit0, nsim = 500, engine = "future", nworkers = 2)
#' summarize_pb(lrt_lme, lr_sim_lme)
#' 
#' lmer_fit1 <- lmer(Reaction ~ Days + Days2 + (1 | Subject), data = sleepstudy, REML=FALSE)
#' lmer_fit0 <- update(lmer_fit1, . ~ . - Days2)
#' set.seed(42)
#' lrt_lmer <- getLRT(lmer_fit1, lmer_fit0)
#' lr_sim_lmer <- pb_refdist(lmer_fit1, lmer_fit0, nsim = 500, engine = "future", nworkers = 2)
#' summarize_pb(lrt_lmer, lr_sim_lmer) |> summary()
#'
#' lr_sim_lmer <- pb_refdist_sequential(lmer_fit1, lmer_fit0, nsim = 100, engine = "future", nworkers = 2)
#' summarize_pb(lrt_lmer, lr_sim_lmer)
#'
#' |> summary()
#'
#' 




#' Sequential parametric bootstrap with early stopping
#'
#' Simulate bootstrap distribution until reaching h extremes or maximum simulations.
#'
#' @param fit1 Larger model
#' @param fit0 Smaller model (null)
#' @param tobs Observed test statistic
#' @param h Number of desired extreme hits
#' @param nsim_max Maximum number of simulations
#' @param batch_size Number of simulations per batch
#' @param engine "serial", "parallel", or "future"
#' @param nworkers Number of workers for parallel
#' @param verbose Print progress
#'
#' @return List with simulated values, number of hits, p-value, SE, etc.
#' @export
pb_refdist_sequential <- function(
  fit1,
  fit0,
  h = 20,
  nsim_max = 1000,
  batch_size = 50,
  engine = c("serial", "parallel", "future"),
  nworkers = 2,
  verbose = TRUE
) {
  engine <- match.arg(engine)
  total_sims <- 0
  hits <- 0
  ref <- numeric(0)

  LRT <- getLRT(fit1, fit0)
  tobs <- LRT["tobs"]
  
  if (verbose) {
    cat(sprintf("Starting sequential bootstrap: target %d hits or max %d simulations\n", h, nsim_max))
  }
  
  while (hits < h && total_sims < nsim_max) {
    n_batch <- min(batch_size, nsim_max - total_sims)
    
    if (verbose) cat(sprintf("Simulating batch of %d...\n", n_batch))
    
    # Generate new responses
    simdata <- simulate(fit0, nsim = n_batch)
    
    # Define worker
    worker_fun <- function(x) {
      ll1 <- logLik(refit(fit1, newresp = x))
      ll0 <- logLik(refit(fit0, newresp = x))
      as.numeric(ll1 - ll0)
    }
    
    # Evaluate
    if (engine == "serial") {
      res <- sapply(simdata, worker_fun)
    } else if (engine == "parallel") {
      cl <- parallel::makeCluster(nworkers)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::clusterExport(cl, varlist = c("worker_fun", "fit1", "fit0", "refit"), envir = environment())
      res <- unlist(parallel::parLapply(cl, simdata, worker_fun))
    } else if (engine == "future") {
      requireNamespace("furrr")
      future::plan(future::multisession, workers = nworkers)
      res <- unlist(furrr::future_map(
        simdata,
        function(x) {
          worker_fun(x)
        },
        .options = furrr::furrr_options(seed = TRUE)
      ))
    } else {
      stop("Invalid engine")
    }
    
    ref <- c(ref, res)
    batch_hits <- sum(res >= tobs)
    hits <- hits + batch_hits
    total_sims <- total_sims + n_batch
    
    if (verbose) {
      cat(sprintf("Batch done. Hits in batch: %d | Total hits: %d | Total sims: %d\n", 
                  batch_hits, hits, total_sims))
    }
  }
  
  # Bias-corrected estimate
  p_hat <- (hits + 1) / (total_sims + 1)
  se_p  <- sqrt(p_hat * (1 - p_hat) / (total_sims + 1))
  
  result <- list(
    ref = ref,
    hits = hits,
    total_sims = total_sims,
    p.value = p_hat,
    se = se_p,
    tobs = tobs,
    h_target = h,
    nsim_max = nsim_max
  )
  class(result) <- "PBseq"
  return(result)
}

































#'
#'
#'
#' 
#' @export
summarize_pb <- function(LRTstat, ref, conf.level = 0.95) {
  
  # Extract observed
  tobs <- unname(LRTstat[1])
  ndf  <- unname(LRTstat[2])
  
  refpos <- ref[ref > 0]
  nsim   <- length(ref)
  npos   <- length(refpos)
  
  # Moments
  EE     <- mean(refpos)
  VV     <- var(refpos)
  Med    <- median(refpos)
  
  # Asymptotic p
  p.chi <- 1 - pchisq(tobs, df = ndf)
  
  # Bootstrap p-value (bias-corrected)
  n.extreme <- sum(tobs < refpos)
  p.PB      <- (1 + n.extreme) / (1 + npos)
  p.PB.all  <- (1 + n.extreme) / (1 + nsim)

  # Percentile interpolation
    ecdf_ref <- ecdf(ref)
    p.ECDF <- 1 - ecdf_ref(tobs)
    
  se <- sqrt(p.PB * (1 - p.PB) / npos)
  z <- qnorm(1 - (1 - conf.level) / 2)
  ci <- c(p.PB - z * se, p.PB + z * se)
  ci <- pmax(0, pmin(1, round(ci, 4)))
  
  # Bartlett correction
  BC_mean   <- ndf * tobs / EE
  p.BCmean  <- 1 - pchisq(BC_mean, df = ndf)
  
  BC_median <- ndf * tobs / Med
  p.BCmed   <- 1 - pchisq(BC_median, df = ndf)
  
  # F approximation
  Fobs <- tobs / ndf
  
  # Mean-based ddf
  ddf_mean <- if (!is.na(EE) && EE > ndf) 2 * EE / (EE - ndf) else NA
  p.Fmean  <- if (!is.na(ddf_mean) && ddf_mean > 0) 1 - pf(Fobs, df1 = ndf, df2 = ddf_mean) else NA
  
  # Median-based ddf via optimize
  find_ddf_median <- function(df1, med) {
    objective <- function(df2) abs(qf(0.5, df1, df2) - med)
    optimize(objective, interval = c(2.01, 500))$minimum
  }
  ddf_med <- tryCatch(find_ddf_median(ndf, Med), error = function(e) NA)
  p.Fmed  <- if (!is.na(ddf_med) && ddf_med > 0) 1 - pf(Fobs, df1 = ndf, df2 = ddf_med) else NA
  
  # Collect results
  tests <- list(
    Asymptotic       = c(stat = tobs,     df = ndf, ddf = NA,        p.value = p.chi),
    Bootstrap        = c(stat = tobs,     df = NA,  ddf = NA,        p.value = p.PB),
    Percentile       = c(stat = tobs,     df = NA,  ddf = NA,        p.value = p.ECDF),
    Bartlett_mean    = c(stat = BC_mean,  df = ndf, ddf = NA,        p.value = p.BCmean),
    Bartlett_median  = c(stat = BC_median,df = ndf, ddf = NA,        p.value = p.BCmed),
    F_mean           = c(stat = Fobs,     df = ndf, ddf = ddf_mean,  p.value = p.Fmean),
    F_median         = c(stat = Fobs,     df = ndf, ddf = ddf_med,   p.value = p.Fmed)
  )
  
  test_df <- as.data.frame(do.call(rbind, tests))
  
  ans <- list(
    test    = test_df,
    type    = "X2test",
    moment  = c(mean = EE, var = VV, median = Med),
    samples = c(nsim = nsim, npos = npos),
    ref     = ref,
    ci      = ci,
    se      = se,
    n.extreme = n.extreme,
    p.ECDF = p.ECDF
  )
  
  class(ans) <- c("PBmodcomp2")
  ans
}

#' @export
summary.PBmodcomp2 <- function(object, ...) {
  cat("\nParametric bootstrap test summary\n")
  cat("---------------------------------\n")
  cat("Type           :", object$type, "\n")
  cat("Simulations    :", object$samples["nsim"], "\n")
  cat("Positive samples:", object$samples["npos"], "\n")
  cat(sprintf("Bootstrap extremes: %d\n", object$n.extreme))
  cat("\n")
  cat("Moments of bootstrap distribution:\n")
  print(round(object$moment, 3))
  cat("\n")
  cat("Bootstrap p-value (bias-corrected):", 
      round(object$test["Bootstrap", "p.value"], 4), "\n")
  cat("Percentile-interpolated p-value   :", 
      round(object$test["Percentile", "p.value"], 4), "\n")
  cat("Asymptotic p-value                :", 
      round(object$test["Asymptotic", "p.value"], 4), "\n")
  cat("\n")
  cat("All test approximations:\n")
  print(round(object$test, 4))
  cat("\n")
  invisible(object)
}

















































## #' @method vcovAdj mer
## #' @rdname kr-vcovAdj
## #' @export
## vcovAdj.mer <- vcovAdj.lmerMod



## .vcovAdj_internal <- function(Phi, SigmaG, X, details=0){

##     ##cat("vcovAdj_internal\n")
##     ##SG<<-SigmaG
##     DB <- details > 0 ## debugging only

##     #print("HHHHHHHHHHHHHHH")
##     #print(system.time({chol( forceSymmetric(SigmaG$Sigma) )}))
##     #print(system.time({chol2inv( chol( forceSymmetric(SigmaG$Sigma) ) )}))

##     ## print("HHHHHHHHHHHHHHH")
##     ## Sig <- forceSymmetric( SigmaG$Sigma )
##     ## print("HHHHHHHHHHHHHHH")
##     ## print(system.time({Sig.chol <- chol( Sig )}))
##     ## print(system.time({chol2inv( Sig.chol )}))

##     t0 <- proc.time()
##     ## print("HHHHHHHHHHHHHHH")
##     SigmaInv <- chol2inv( chol( forceSymmetric(SigmaG$Sigma) ) )
##     ## print("DONE --- HHHHHHHHHHHHHHH")

##     if(DB){
##         cat(sprintf("Finding SigmaInv: %10.5f\n", (proc.time()-t0)[1] ));
##         t0 <- proc.time()
##     }
##     ##print("iiiiiiiiiiiii")

##     t0 <- proc.time()
##     ## Finding, TT, HH, 00
##     n.ggamma <- SigmaG$n.ggamma
##     TT       <- SigmaInv %*% X
##     HH       <- OO <- vector("list", n.ggamma)
##     for (ii in 1:n.ggamma) {
##         .tmp <- SigmaG$G[[ii]] %*% SigmaInv
##         HH[[ ii ]] <- .tmp
##         OO[[ ii ]] <- .tmp %*% X
##     }
##     if(DB){cat(sprintf("Finding TT,HH,OO  %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()}
##     ## if(DB){
##     ##     cat("HH:\n"); print(HH); HH <<- HH
##     ##     cat("OO:\n"); print(OO); OO <<- OO
##     ## }

##     ## Finding PP, QQ
##     PP <- QQ <- NULL
##     for (rr in 1:n.ggamma) {
##         OrTrans <- t( OO[[ rr ]] )
##         PP <- c(PP, list(forceSymmetric( -1 * OrTrans %*%  TT)))
##         for (ss in rr:n.ggamma) {
##             QQ <- c(QQ,list(OrTrans %*% SigmaInv %*% OO[[ss]] ))
##         }}
##     if(DB){cat(sprintf("Finding PP,QQ:    %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()}
##     ## if(DB){
##     ##     cat("PP:\n"); print(PP); PP2 <<- PP
##     ##     cat("QP:\n"); print(QQ); QQ2 <<- QQ
##     ## }

##     Ktrace <- matrix( NA, nrow=n.ggamma, ncol=n.ggamma )
##     for (rr in 1:n.ggamma) {
##         HrTrans <- t( HH[[rr]] )
##         for (ss in rr:n.ggamma){
##             Ktrace[rr,ss] <- Ktrace[ss,rr]<- sum( HrTrans * HH[[ss]] )
##         }}
##     if(DB){cat(sprintf("Finding Ktrace:   %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()}

##     ## Finding information matrix
##     IE2 <- matrix( NA, nrow=n.ggamma, ncol=n.ggamma )
##     for (ii in 1:n.ggamma) {
##         Phi.P.ii <- Phi %*% PP[[ii]]
##         for (jj in c(ii:n.ggamma)) {
##             www <- .indexSymmat2vec( ii, jj, n.ggamma )
##             IE2[ii,jj]<- IE2[jj,ii] <- Ktrace[ii,jj] -
##                 2 * sum(Phi*QQ[[ www ]]) + sum( Phi.P.ii * ( PP[[jj]] %*% Phi))
##         }}
##     if(DB){cat(sprintf("Finding IE2:      %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()}

##     eigenIE2 <- eigen(IE2,only.values=TRUE)$values
##     condi    <- min(abs(eigenIE2))

##     WW <- if(condi>1e-10) forceSymmetric(2* solve(IE2)) else forceSymmetric(2* ginv(IE2))

##     ## print("vcovAdj")
##     UU <- matrix(0, nrow=ncol(X), ncol=ncol(X))
##     ## print(UU)
##     for (ii in 1:(n.ggamma-1)) {
##         for (jj in c((ii+1):n.ggamma)) {
##             www <- .indexSymmat2vec( ii, jj, n.ggamma )
##             UU <- UU + WW[ii,jj] * (QQ[[ www ]] - PP[[ii]] %*% Phi %*% PP[[jj]])
##         }}
##     ## print(UU)

##     UU <- UU + t(UU)
##     ## UU <<- UU
##     for (ii in 1:n.ggamma) {
##         www <- .indexSymmat2vec( ii, ii, n.ggamma )
##         UU<- UU +   WW[ii,ii] * (QQ[[ www ]] - PP[[ii]] %*% Phi %*% PP[[ii]])
##     }
##     ## print(UU)
##     GGAMMA <-  Phi %*% UU %*% Phi
##     PhiA   <-  Phi + 2 * GGAMMA
##     attr(PhiA, "P")     <-PP
##     attr(PhiA, "W")     <-WW
##     attr(PhiA, "condi") <- condi
##     PhiA
## }


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













