
#' Parametric Bootstrap Reference Distribution
#'
#' Compute a parametric bootstrap reference distribution for a likelihood ratio statistic,
#' comparing a large (alternative) and a small (null) model. The distribution can be used
#' to estimate a bootstrap p-value. 
#'
#' @description
#' - `pb_refdist()` simulates a *fixed* number of bootstrap replicates.
#' - `pb_refdist_sequential()` runs batches of simulations until a target number of extreme
#'   hits is reached (or a maximum number of simulations is hit), useful for efficient estimation
#'   of small p-values.
#'
#' Both functions return objects of class `"PBrefdist"` with methods for `summary()`, `plot()`, and
#' `as.data.frame()`.
#'
#' @param fit1 The larger (alternative) model, e.g. `lm`, `gls`, `lme`, `lmer`.
#' @param fit0 The smaller (null) model, nested in `fit1`.
#' @param nsim Number of simulations (for `pb_refdist`) or maximum number of simulations (`pb_refdist_sequential`).
#' @param h Number of extreme hits to target (for `pb_refdist_sequential`).
#' @param engine Computation engine: `"serial"`, `"parallel"`, or `"future"`.
#' @param nworkers Number of workers for parallel/future backend.
#' @param batch_size Number of simulations per batch
#' @param verbose Logical; if `TRUE`, print progress messages. Default is `FALSE`.
#'
#' @return An object of class `"PBrefdist"` containing the observed statistic, the bootstrap replicates,
#'         degrees of freedom, asymptotic p-value, and optionally the number of hits and standard error.
#'
#' @details
#' The sequential version is useful when one wants to control Monte Carlo error by targeting
#' a fixed number of extreme values exceeding the observed test statistic.
#'
#' @note
#' **Best Practice:** Always fit your models with the `data=` argument.
#' This ensures all covariates used in the model formula are stored with the model object,
#' enabling reliable simulation and refitting for bootstrap analysis,
#' including on parallel workers. Without `data=`, refitting may fail in parallel contexts
#' and reproducibility is compromised.
#' 
#' The returned object can be passed to `summary()`, `plot()`, and `as.data.frame()`.
#' @note
#' **Best Practice:** Always fit your models with the `data=` argument.
#' This ensures all covariates used in the model formula are stored with the model object,
#' enabling reliable simulation and refitting for bootstrap analysis,
#' including on parallel workers. Without `data=`, refitting may fail in parallel contexts
#' and reproducibility is compromised.
#'
#' @note
#' The function automatically ensures that the models have their required data embedded.
#' This guarantees that parametric bootstrap simulations can be run in parallel workers
#' without errors about missing variables, even if the original dataset was modified
#' or removed from the global environment after fitting.
#' 
#' @examples
#' if (requireNamespace("lme4") && requireNamespace("nlme")) {
#'   data(sleepstudy, package = "lme4")
#'   sleepstudy$Days2 <- sleepstudy$Days^2
#'
#'   # LM example
#'   lm_fit1 <- lm(Reaction ~ Days + Days2, data = sleepstudy)
#'   lm_fit0 <- update(lm_fit1, . ~ . - Days2)
#'   set.seed(42)
#'   res_lm <- pb_refdist(lm_fit1, lm_fit0, nsim = 200)
#'   summary(res_lm)
#'   plot(res_lm, show.chisq=TRUE)
#'
#'   # GLS example
#'   gls_fit1 <- nlme::gls(Reaction ~ Days + Days2, data = sleepstudy, method="ML")
#'   gls_fit0 <- update(gls_fit1, . ~ . - Days2)
#'   set.seed(42)
#'   res_gls <- pb_refdist(gls_fit1, gls_fit0, nsim = 200)
#'   summary(res_gls)
#'   plot(res_gls, show.chisq=TRUE)
#'
#'   # LME example
#'   lme_fit1 <- nlme::lme(Reaction ~ Days + Days2, random = ~ 1 | Subject,
#'                         data = sleepstudy, method="ML")
#'   lme_fit0 <- update(lme_fit1, . ~ . - Days2)
#'   set.seed(42)
#'   res_lme <- pb_refdist(lme_fit1, lme_fit0, nsim = 200)
#'   summary(res_lme)
#'   plot(res_lme, show.chisq=TRUE)
#'
#'   # LMER example (lme4)
#'   lmer_fit1 <- lme4::lmer(Reaction ~ Days + Days2 + (1 | Subject),
#'                           data = sleepstudy, REML=FALSE)
#'   lmer_fit0 <- update(lmer_fit1, . ~ . - Days2)
#'   set.seed(42)
#'   res_lmer <- pb_refdist(lmer_fit1, lmer_fit0, nsim = 200)
#'   summary(res_lmer)
#'   plot(res_lmer, show.chisq=TRUE)
#'
#'   # Sequential example
#'   set.seed(42)
#'   res_seq <- pb_refdist_sequential(lmer_fit1, lmer_fit0, h = 20, nsim = 100)
#'   summary(res_seq)
#'   plot(res_seq, show.chisq=TRUE)
#' }
#'
#' @export
pb_refdist <- function(
                       fit1,
                       fit0,
                       nsim = 1000,
                       engine = c("serial", "parallel", "future"),
                       nworkers = 2,
                       verbose = FALSE
                       ) {
    t0 <- proc.time()
    engine <- match.arg(engine)
    
    
    check_model_has_data(fit1)
    check_model_has_data(fit0)
    
    fit1 <- fix_model_data(fit1)
    fit0 <- fix_model_data(fit0)
    
    data <- .get_model_data(fit1)

    if (verbose) cat("Computing observed likelihood ratio statistic via getLRT()...\n")
    LRT <- getLRT(fit1, fit0)
    
    if (verbose) {
        cat(sprintf("Simulating %d replicates under null (engine = %s)\n", nsim, engine))
    }
    
    simdata <- simulate(fit0, nsim = nsim)
    
    worker_fun <- function(x) {
        ll1 <- logLik(refit(fit1, newresp = x))
        ll0 <- logLik(refit(fit0, newresp = x))
        2 * as.numeric(ll1 - ll0)
    }
    
    
    if (engine == "serial") {
        ref <- sapply(simdata, worker_fun)
        
    } else if (engine == "parallel") {
        cl <- parallel::makeCluster(nworkers)
        on.exit(parallel::stopCluster(cl))
        parallel::clusterExport(cl, varlist = c("worker_fun", "fit1", "fit0", "refit"), envir = environment())
        ref <- unlist(parallel::parLapply(cl, simdata, worker_fun))
        
    } else if (engine == "future") {
        
        requireNamespace("furrr")
        requireNamespace("future")
        
        old_plan <- future::plan()
        ## on.exit(future::plan(old_plan), add = TRUE)      
        
        on.exit({
            future::plan(future::sequential)
        }, add = TRUE)
        
        
        
        future::plan(future::multisession, workers = nworkers)
        
        ref <- unlist(
            furrr::future_map(
                       simdata,
                       worker_fun,
                       .options = furrr::furrr_options(
                                             seed = TRUE,
                                             packages = c("pbkrtest", "nlme", "lme4")
                                         )
                   )
        )
    } else {
        stop("Invalid engine")
    }
    
    elapsed <- unname((proc.time() - t0))[3]    
    ans <- list(
        ref = unname(ref),
        LRT = LRT,
        nsim = nsim,
        ctime = elapsed
    )
    class(ans) <- "PBrefdist"
    return(ans)
}


#' @rdname pb_refdist
#' @export
pb_refdist_sequential <- function(
                                  fit1,
                                  fit0,
                                  h = 20,
                                  nsim = 1000,
                                  batch_size = 50,
                                  engine = c("serial", "parallel", "future"),
                                  nworkers = 2,
                                  verbose = FALSE
                                  ) {
    t0 <- proc.time()
    engine <- match.arg(engine)
    check_model_has_data(fit1)
    check_model_has_data(fit0)
    
    fit1 <- fix_model_data(fit1)
    fit0 <- fix_model_data(fit0)
    data <- .get_model_data(fit1)
    
    if (verbose) cat("Computing observed likelihood ratio statistic via getLRT()...\n")
    LRT <- getLRT(fit1, fit0)
    tobs <- LRT[1]
    
    if (is.infinite(h)) {
        if (verbose) cat("h = Inf detected. Falling back to fixed-nsim sampling.\n")
        return(pb_refdist(
            fit1 = fit1,
            fit0 = fit0,
            nsim = nsim,
            engine = engine,
            nworkers = nworkers,
            verbose = verbose
        ))
    }
    
    total_sims <- 0
    hits <- 0
    ref <- numeric(0)
    
    if (verbose) {
        cat(sprintf("Starting sequential bootstrap: target %d hits or max %d simulations\n", h, nsim))
    }
    
    while (hits < h && total_sims < nsim) {
        n_batch <- min(batch_size, nsim - total_sims)
        
        if (verbose) cat(sprintf("Simulating batch of %d...\n", n_batch))
        
        simdata <- simulate(fit0, nsim = n_batch)
        
        worker_fun <- function(x) {
            ll1 <- logLik(refit(fit1, newresp = x))
            ll0 <- logLik(refit(fit0, newresp = x))
            as.numeric(ll1 - ll0)
        }
        
        if (engine == "serial") {
            res <- sapply(simdata, worker_fun)
            
        } else if (engine == "parallel") {
            cl <- parallel::makeCluster(nworkers)
            on.exit(parallel::stopCluster(cl), add = TRUE)
            parallel::clusterExport(cl, varlist = c("worker_fun", "fit1", "fit0", "refit"), envir = environment())
            res <- unlist(parallel::parLapply(cl, simdata, worker_fun))
            
        } else if (engine == "future") {
            requireNamespace("furrr")
            requireNamespace("future")
            old_plan <- future::plan()
            ## on.exit(future::plan(old_plan), add = TRUE)
            
            on.exit({
                future::plan(future::sequential)
            }, add = TRUE)
            
            
            future::plan(future::multisession, workers = nworkers)
            
            res <- unlist(
                furrr::future_map(
                           simdata,
                           worker_fun,
                           .options = furrr::furrr_options(
                                                 seed = TRUE,
                                                 packages = c("pbkrtest", "nlme", "lme4")
                                             )
                       )
            )
        } else {
            stop("Invalid engine")
        }
        
        ref <- c(ref, unname(res))
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
    
    elapsed <- unname((proc.time() - t0))[3]
    result <- list(
        ref = ref,
        LRT = LRT,
        hits = hits,
        total_sims = total_sims,
        p.value = p_hat,
        se = se_p,
        h_target = h,
        nsim = nsim,
        ctime = elapsed
    )
    class(result) <- "PBrefdist"
    return(result)
}

















## #' @keywords internal
## fix_model_data <- function(model) {
##   if (inherits(model, "lm")) {
##     # lm har altid data internt, ingen grund til noget
##     return(model)
##   }
  
##   d <- tryCatch(getData(model), error = function(e) NULL)
##   if (is.null(d) || !is.data.frame(d)) {
##     stop(" Could not extract data from model. Make sure you used data= with all variables when fitting.")
##   }
  
##   # Refit model with its data, so formula can evaluate anywhere
##   update(model, data = d)
## }




## #' @keywords internal
## fix_model_data <- function(model) {
##   if (inherits(model, "lm")) {
##     return(model)
##   }
  
##   d <- tryCatch(getData(model), error = function(e) NULL)
##   if (is.null(d) || !is.data.frame(d)) {
##     stop(" Could not extract data from model. Make sure you used data= with all variables when fitting.")
##   }
  
##   # ️ Embed data in an environment to make it self-contained
##   env_data <- list2env(list(d = d), parent = emptyenv())
##   update(model, data = env_data)
## }


#' @keywords internal
fix_model_data <- function(model) {
  if (inherits(model, "lm")) return(model)
  
  d <- tryCatch(getData(model), error = function(e) NULL)
  if (is.null(d) || !is.data.frame(d)) {
    stop(" Could not extract data from model. Make sure you used data= with all variables when fitting.")
  }
  
  # ️ use do.call to force embedding the data.frame
  do.call(update, list(model, data = d))
}



.get_model_data <- function(fit) {
  if (inherits(fit, "gls") || inherits(fit, "lme")) {
    return(nlme::getData(fit))
  }
  if ("model" %in% names(fit)) {
    return(fit$model)
  }
  mf <- tryCatch(model.frame(fit), error=function(e) NULL)
  if (!is.null(mf)) return(mf)
  
  stop("Cannot extract data from model object; consider refitting with data= argument.")
}




## pb_refdist <- function(
##   fit1,
##   fit0,
##   nsim = 1000,
##   engine = c("serial", "parallel", "future"),
##   nworkers = 2,
##   verbose = FALSE
##   ) {
##     t0 <- proc.time()

##   engine <- match.arg(engine)
  
##   # Always compute observed statistic
##   if (verbose) cat("Computing observed likelihood ratio statistic via getLRT()...\n")
##   LRT <- getLRT(fit1, fit0)
##   tobs <- LRT[1]
  
##   if (verbose) {
##     cat(sprintf("Simulating %d replicates under null (engine = %s)\n", nsim, engine))
##   }
  
##   simdata <- simulate(fit0, nsim = nsim)
  
##   worker_fun <- function(x) {
##     ll1 <- logLik(refit(fit1, newresp = x))
##     ll0 <- logLik(refit(fit0, newresp = x))
##     as.numeric(ll1 - ll0)
##   }
  
##   if (engine == "serial") {
##     ref <- sapply(simdata, worker_fun)
##   } else if (engine == "parallel") {
##     cl <- parallel::makeCluster(nworkers)
##     on.exit(parallel::stopCluster(cl))
##     parallel::clusterExport(cl, varlist = c("worker_fun", "fit1", "fit0", "refit"), envir = environment())
##     ref <- unlist(parallel::parLapply(cl, simdata, worker_fun))
##   } else if (engine == "future") {
##     requireNamespace("furrr")
##     requireNamespace("future")
##     future::plan(future::multisession, workers = nworkers)
##     ref <- unlist(furrr::future_map(
##       simdata,
##       worker_fun,
##       .options = furrr::furrr_options(seed = TRUE)
##     ))
##   } else {
##     stop("Invalid engine")
##   }

## elapsed <- unname((proc.time() - t0))[3]    
##   ans <- list(
##       ref = unname(ref),
##       LRT = LRT,
##       nsim = nsim,
##       ctime=elapsed
##   )
##   class(ans) <- "PBrefdist"
##   return(ans)
## }



## #' @rdname pb_refdist
## #' @export
## pb_refdist_sequential <- function(
##   fit1,
##   fit0,
##   h = 20,
##   nsim_max = 1000,
##   batch_size = 50,
##   engine = c("serial", "parallel", "future"),
##   nworkers = 2,
##   verbose = FALSE
##   ) {
##     t0 <- proc.time()

##   engine <- match.arg(engine)
  
##   # Always compute observed statistic
##   if (verbose) cat("Computing observed likelihood ratio statistic via getLRT()...\n")
##   LRT <- getLRT(fit1, fit0)
##   tobs <- LRT[1]
  
  
##   if (is.infinite(h)) {
##     if (verbose) cat("h = Inf detected. Falling back to fixed-nsim sampling.\n")
##     return(pb_refdist(
##       fit1 = fit1,
##       fit0 = fit0,
##       nsim = nsim_max,
##       engine = engine,
##       nworkers = nworkers,
##       verbose = verbose
##     ))
##   }
  
##   total_sims <- 0
##   hits <- 0
##   ref <- numeric(0)
  
##   if (verbose) {
##     cat(sprintf("Starting sequential bootstrap: target %d hits or max %d simulations\n", h, nsim_max))
##   }
  
##   while (hits < h && total_sims < nsim_max) {
##     n_batch <- min(batch_size, nsim_max - total_sims)
    
##     if (verbose) cat(sprintf("Simulating batch of %d...\n", n_batch))
    
##     simdata <- simulate(fit0, nsim = n_batch)
    
##     worker_fun <- function(x) {
##       ll1 <- logLik(refit(fit1, newresp = x))
##       ll0 <- logLik(refit(fit0, newresp = x))
##       as.numeric(ll1 - ll0)
##     }
    
##     if (engine == "serial") {
##       res <- sapply(simdata, worker_fun)
##     } else if (engine == "parallel") {
##       cl <- parallel::makeCluster(nworkers)
##       on.exit(parallel::stopCluster(cl), add = TRUE)
##       parallel::clusterExport(cl, varlist = c("worker_fun", "fit1", "fit0", "refit"), envir = environment())
##       res <- unlist(parallel::parLapply(cl, simdata, worker_fun))
##     } else if (engine == "future") {
##       requireNamespace("furrr")
##       requireNamespace("future")
##       future::plan(future::multisession, workers = nworkers)
##       res <- unlist(furrr::future_map(
##         simdata,
##         worker_fun,
##         .options = furrr::furrr_options(seed = TRUE)
##       ))
##     } else {
##       stop("Invalid engine")
##     }
    
##     ref <- c(ref, unname(res))
##     batch_hits <- sum(res >= tobs)
##     hits <- hits + batch_hits
##     total_sims <- total_sims + n_batch
    
##     if (verbose) {
##       cat(sprintf("Batch done. Hits in batch: %d | Total hits: %d | Total sims: %d\n", 
##                   batch_hits, hits, total_sims))
##     }
##   }
  
##   # Bias-corrected estimate
##   p_hat <- (hits + 1) / (total_sims + 1)
##   se_p  <- sqrt(p_hat * (1 - p_hat) / (total_sims + 1))

##     elapsed <- unname((proc.time() - t0))[3]
##   result <- list(
##       ref = ref,
##       LRT = LRT,
##     hits = hits,
##     total_sims = total_sims,
##     p.value = p_hat,
##     se = se_p,
##     h_target = h,
##     nsim_max = nsim_max,
##     ctime=elapsed
##   )
##   class(result) <- "PBrefdist"
##   return(result)
## }






#' @export
print.PBrefdist <- function(x, ...) {
  cat("\nParametric Bootstrap Reference Distribution\n")
  cat("--------------------------------------------\n")
  cat("Observed statistic:", round(x$LRT["tobs"], 4), "\n")
  cat("Number of simulations:", 
      if (!is.null(x$nsim)) x$nsim else x$total_sims, "\n")
  cat("\n")
  invisible(x)
}

#' @export
summary.PBrefdist <- function(object, ...) {
  cat("\nParametric Bootstrap Reference Distribution\n")
  cat("--------------------------------------------\n")
  cat("Observed statistic:", round(object$LRT["tobs"], 4), "\n")
  
  ## nsim <- if (!is.null(object$nsim)) object$nsim else object$total_sims
  nsim <- if (!is.null(object$total_sims)) object$total_sims else object$nsim

  cat("Simulations       :", nsim, "\n")
  
  if (!is.null(object$hits)) {
    cat("Hits              :", object$hits, "\n")
    cat("Target hits       :", object$h_target, "\n")
    cat("Bias-corrected p  :", round(object$p.value, 4), "\n")
    cat("Standard Error    :", round(object$se, 4), "\n")
  } else {
    # For fixed-nsim runs (pb_refdist)
    extremes <- sum(object$ref >= object$LRT["tobs"])
    p.pb <- (1 + extremes) / (1 + nsim)
    se.pb <- sqrt(p.pb * (1 - p.pb) / (1 + nsim))
    cat("Bootstrap extremes:", extremes, "\n")
    cat("Bias-corrected p  :", round(p.pb, 4), "\n")
    cat("Standard Error    :", round(se.pb, 4), "\n")
  }
  
  cat("\n")
  invisible(object)
}


#' @method summary PBrefdist
#' @export
summary.PBrefdist <- function(object, ...) {
  cat("\nParametric Bootstrap Reference Distribution\n")
  cat("--------------------------------------------\n")
  
  cat("Observed statistic :", round(object$LRT[1], 4), "\n")
  cat("Degrees of freedom  :", round(object$LRT["df"], 2), "\n")
  cat("Asymptotic p-value  :", round(object$LRT["p.value"], 4), "\n")


  nsim <- if (!is.null(object$total_sims)) object$total_sims else object$nsim
  cat("Simulations used    :", nsim, "\n")
  
  if (!is.null(object$hits)) {
    cat("Hits (extremes)     :", object$hits, "\n")
    cat("Target hits         :", object$h_target, "\n")
    cat("Bias-corrected p    :", round(object$p.value, 4), "\n")
    cat("Standard Error      :", round(object$se, 4), "\n")
  } else {
    # For fixed-nsim runs
    extremes <- sum(object$ref >= object$LRT[1])
    p.pb <- (1 + extremes) / (1 + nsim)
    se.pb <- sqrt(p.pb * (1 - p.pb) / (1 + nsim))
    
    cat("Bootstrap extremes  :", extremes, "\n")
    cat("Bias-corrected p    :", round(p.pb, 4), "\n")
    cat("Standard Error      :", round(se.pb, 4), "\n")
  }
  
  cat("\n")
  invisible(object)
}


#' @method summary PBrefdist
#' @export
summary.PBrefdist <- function(object, ...) {
  cat("\nParametric Bootstrap Reference Distribution\n")
  cat("--------------------------------------------\n")
  
  # Observed statistic and asymptotic test
  cat("Observed statistic :", round(object$LRT[1], 4), "\n")
  
  if (!is.null(object$LRT["df"])) {
    cat("Degrees of freedom  :", round(object$LRT["df"], 2), "\n")
  }
  
  if (!is.null(object$LRT["p.value"])) {
    cat("Asymptotic p-value  :", round(object$LRT["p.value"], 4), "\n")
  }
  
  # Simulations used
  nsim <- if (!is.null(object$total_sims)) object$total_sims else object$nsim
  cat("Simulations used    :", nsim, "\n")
  
  # Sequential or fixed-nsim
  if (!is.null(object$hits)) {
    cat("Hits (extremes)     :", object$hits, "\n")
    cat("Target hits         :", object$h_target, "\n")
    cat("Bias-corrected p    :", round(object$p.value, 4), "\n")
    cat("Standard Error      :", round(object$se, 4), "\n")
  } else {
    # For fixed-nsim runs
    extremes <- sum(object$ref >= object$LRT[1])
    p.pb <- (1 + extremes) / (1 + nsim)
    se.pb <- sqrt(p.pb * (1 - p.pb) / (1 + nsim))
    
    cat("Bootstrap extremes  :", extremes, "\n")
    cat("Bias-corrected p    :", round(p.pb, 4), "\n")
    cat("Standard Error      :", round(se.pb, 4), "\n")
  }
  
  # Computing time if available
  if (!is.null(object$ctime)) {
    cat("Computing time (sec):", round(object$ctime, 2), "\n")
  }
  
  cat("\n")
  invisible(object)
}



#' @method as.data.frame PBrefdist
#' @export
as.data.frame.PBrefdist <- function(x, ...) {
  data.frame(
    simulated = x$ref,
    observed = x$LRT["tobs"]
  )
}


#' @method plot PBrefdist
#' @export
plot.PBrefdist <- function(x, ..., breaks = 30, main = NULL, col = "lightgray", show.chisq = FALSE) {
  # Check class
  if (!inherits(x, "PBrefdist")) {
    stop("plot.PBrefdist can only be used on objects of class 'PBrefdist'.")
  }

  # Extract observed statistic
  tobs <- x$LRT[1]
  
  # Create histogram
  h <- hist(x$ref, breaks = breaks, col = col, border = "white",
            main = if (is.null(main)) "Bootstrap Distribution of Test Statistic" else main,
            xlab = "Test Statistic", ...)
  
  abline(v = tobs, col = "red", lwd = 2)
  
  # Determine bootstrap p-value
  if (!is.null(x$p.value)) {
    p.boot <- x$p.value
  } else {
    extremes <- sum(x$ref >= tobs)
    nsim <- x$nsim
    p.boot <- (1 + extremes) / (1 + nsim)
  }
  
  # Asymptotic p-value
  p.asym <- if (!is.null(x$LRT["p.value"])) x$LRT["p.value"] else NA
  
  # Legend text items
  legend_items <- c(
    sprintf("Observed = %.3f", tobs),
    sprintf("Asymptotic p = %.3f", p.asym),
    sprintf("Bootstrap p = %.3f", p.boot)
  )
  
  # Optionally overlay chi-squared density
  if (show.chisq && !is.null(x$LRT["df"])) {
    df <- x$LRT["df"]
    
    # Scaling to histogram
    binwidth <- diff(h$mids[1:2])
    scale_factor <- length(x$ref) * binwidth
    xmin <- min(h$breaks)
    xmax <- max(h$breaks)
    
    curve(
      dchisq(uu_, df=df) * scale_factor,
      from = xmin,
      to = xmax,
      add = TRUE,
      col = "blue",
      lwd = 2,
      xname = "uu_"
    )
    
    legend("topright",
           legend = c(
             legend_items,
             "Observed statistic (red line)",
             sprintf("Chi-squared(df=%.0f) (blue curve)", df)
           ),
           col = c(rep(NA, length(legend_items)), "red", "blue"),
           lwd = c(rep(0, length(legend_items)), 2, 2),
           text.col = "black",
           bty = "n")
  } else {
    legend("topright",
           legend = c(
             legend_items,
             "Observed statistic (red line)"
           ),
           col = c(rep(NA, length(legend_items)), "red"),
           lwd = c(rep(0, length(legend_items)), 2),
           text.col = "black",
           bty = "n")
  }
  
  invisible(x)
}



utils::globalVariables("uu_")




## #' @title Extract data from model
## #' @description
## #' Safely extracts the data used to fit a model. Works with lm, lme4, nlme (gls/lme).
## #' Stops with clear error if model was not fitted with data=.
## #' @param fit A model object.
## #' @return A data.frame
## #' @keywords internal
## get_model_data <- function(fit) {
##   d <- NULL
  
##   if (inherits(fit, c("gls", "lme"))) {
##     d <- tryCatch(nlme::getData(fit), error = function(e) NULL)
##   } else if (inherits(fit, c("lmerMod", "glmerMod"))) {
##     d <- tryCatch(lme4::getData(fit), error = function(e) NULL)
##   } else if (inherits(fit, "lm")) {
##     d <- tryCatch(model.frame(fit), error = function(e) NULL)
##   } else {
##     stop(" Unsupported model type in get_model_data()")
##   }
  
##   if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) {
##     stop(
##       " Could not extract data from model object.\n",
##       " Please ensure your model was fitted with data= including all formula variables."
##     )
##   }
  
##   d
## }










## #' @export
## pb_refdist <- function(
##   fit1,
##   fit0,
##   nsim = 1000,
##   engine = c("serial", "parallel", "future"),
##   nworkers = 2,
##   verbose = FALSE
## ) {
##   t0 <- proc.time()
##   engine <- match.arg(engine)
  
##   if (verbose) cat("Computing observed likelihood ratio statistic via getLRT()...\n")
##   LRT <- getLRT(fit1, fit0)
##   tobs <- LRT[1]
  
##   if (verbose) {
##     cat(sprintf("Simulating %d replicates under null (engine = %s)\n", nsim, engine))
##   }
  
##   simdata <- simulate(fit0, nsim = nsim)
  
##   worker_fun <- function(x) {
##     ll1 <- logLik(refit(fit1, newresp = x))
##     ll0 <- logLik(refit(fit0, newresp = x))
##     as.numeric(ll1 - ll0)
##   }
  
##   if (engine == "serial") {
##     ref <- sapply(simdata, worker_fun)
    
##   } else if (engine == "parallel") {
##     cl <- parallel::makeCluster(nworkers)
##     on.exit(parallel::stopCluster(cl))
##     parallel::clusterExport(cl, varlist = c("worker_fun", "fit1", "fit0", "refit"), envir = environment())
##     ref <- unlist(parallel::parLapply(cl, simdata, worker_fun))
    
##   } else if (engine == "future") {
##     requireNamespace("furrr")
##     requireNamespace("future")
##     future::plan(future::multisession, workers = nworkers)
    
##     ref <- unlist(
##       furrr::future_map(
##         simdata,
##         worker_fun,
##         .options = furrr::furrr_options(seed = TRUE),
##         .packages = c("pbkrtest", "nlme", "lme4")
##       )
##     )
##   } else {
##     stop("Invalid engine")
##   }

##   elapsed <- unname((proc.time() - t0))[3]    
##   ans <- list(
##     ref = unname(ref),
##     LRT = LRT,
##     nsim = nsim,
##     ctime = elapsed
##   )
##   class(ans) <- "PBrefdist"
##   return(ans)
## }

## #' @export
## pb_refdist_sequential <- function(
##   fit1,
##   fit0,
##   h = 20,
##   nsim = 1000,
##   batch_size = 50,
##   engine = c("serial", "parallel", "future"),
##   nworkers = 2,
##   verbose = FALSE
## ) {
##   t0 <- proc.time()
##   engine <- match.arg(engine)
  
##   if (verbose) cat("Computing observed likelihood ratio statistic via getLRT()...\n")
##   LRT <- getLRT(fit1, fit0)
##   tobs <- LRT[1]
  
##   if (is.infinite(h)) {
##     if (verbose) cat("h = Inf detected. Falling back to fixed-nsim sampling.\n")
##     return(pb_refdist(
##       fit1 = fit1,
##       fit0 = fit0,
##       nsim = nsim,
##       engine = engine,
##       nworkers = nworkers,
##       verbose = verbose
##     ))
##   }
  
##   total_sims <- 0
##   hits <- 0
##   ref <- numeric(0)
  
##   if (verbose) {
##     cat(sprintf("Starting sequential bootstrap: target %d hits or max %d simulations\n", h, nsim))
##   }
  
##   while (hits < h && total_sims < nsim) {
##     n_batch <- min(batch_size, nsim - total_sims)
    
##     if (verbose) cat(sprintf("Simulating batch of %d...\n", n_batch))
    
##     simdata <- simulate(fit0, nsim = n_batch)
    
##     worker_fun <- function(x) {
##       ll1 <- logLik(refit(fit1, newresp = x))
##       ll0 <- logLik(refit(fit0, newresp = x))
##       as.numeric(ll1 - ll0)
##     }
    
##     if (engine == "serial") {
##       res <- sapply(simdata, worker_fun)
      
##     } else if (engine == "parallel") {
##       cl <- parallel::makeCluster(nworkers)
##       on.exit(parallel::stopCluster(cl), add = TRUE)
##       parallel::clusterExport(cl, varlist = c("worker_fun", "fit1", "fit0", "refit"), envir = environment())
##       res <- unlist(parallel::parLapply(cl, simdata, worker_fun))
      
##     } else if (engine == "future") {
##       requireNamespace("furrr")
##       requireNamespace("future")
##       future::plan(future::multisession, workers = nworkers)
      
##       res <- unlist(
##         furrr::future_map(
##           simdata,
##           worker_fun,
##           .options = furrr::furrr_options(seed = TRUE),
##           .packages = c("pbkrtest", "nlme", "lme4")
##         )
##       )
##     } else {
##       stop("Invalid engine")
##     }
    
##     ref <- c(ref, unname(res))
##     batch_hits <- sum(res >= tobs)
##     hits <- hits + batch_hits
##     total_sims <- total_sims + n_batch
    
##     if (verbose) {
##       cat(sprintf("Batch done. Hits in batch: %d | Total hits: %d | Total sims: %d\n", 
##                   batch_hits, hits, total_sims))
##     }
##   }
  
##   # Bias-corrected estimate
##   p_hat <- (hits + 1) / (total_sims + 1)
##   se_p  <- sqrt(p_hat * (1 - p_hat) / (total_sims + 1))
  
##   elapsed <- unname((proc.time() - t0))[3]
##   result <- list(
##     ref = ref,
##     LRT = LRT,
##     hits = hits,
##     total_sims = total_sims,
##     p.value = p_hat,
##     se = se_p,
##     h_target = h,
##     nsim_max = nsim,
##     ctime = elapsed
##   )
##   class(result) <- "PBrefdist"
##   return(result)
## }


## #' @export
## pb_refdist <- function(
##   fit1,
##   fit0,
##   nsim = 1000,
##   engine = c("serial", "parallel", "future"),
##   nworkers = 2,
##   verbose = FALSE
## ) {
##   t0 <- proc.time()
##   engine <- match.arg(engine)
  
##   if (verbose) cat("Computing observed likelihood ratio statistic via getLRT()...\n")
##   LRT <- getLRT(fit1, fit0)
##   tobs <- LRT[1]
  
##   if (verbose) {
##     cat(sprintf("Simulating %d replicates under null (engine = %s)\n", nsim, engine))
##   }
  
##   simdata <- simulate(fit0, nsim = nsim)
  
##   worker_fun <- function(x) {
##     ll1 <- logLik(refit(fit1, newresp = x))
##     ll0 <- logLik(refit(fit0, newresp = x))
##     as.numeric(ll1 - ll0)
##   }
  
##   if (engine == "serial") {
##     ref <- sapply(simdata, worker_fun)
    
##   } else if (engine == "parallel") {
##     cl <- parallel::makeCluster(nworkers)
##     on.exit(parallel::stopCluster(cl))
##     parallel::clusterExport(cl, varlist = c("worker_fun", "fit1", "fit0", "refit"), envir = environment())
##     ref <- unlist(parallel::parLapply(cl, simdata, worker_fun))
    
##   } else if (engine == "future") {
##     requireNamespace("furrr")
##     requireNamespace("future")
##     future::plan(future::multisession, workers = nworkers)
    
##     ## Ensure required packages are loaded on all workers
##     if (!is.null(future::getDefaultCluster())) {
##       parallel::clusterCall(future::getDefaultCluster(), function() {
##         library(pbkrtest)
##         library(nlme)
##         library(lme4)
##       })
##     }
    
##     ref <- unlist(
##       furrr::future_map(
##         simdata,
##         worker_fun,
##         .options = furrr::furrr_options(seed = TRUE)
##       )
##     )
##   } else {
##     stop("Invalid engine")
##   }

##   elapsed <- unname((proc.time() - t0))[3]    
##   ans <- list(
##     ref = unname(ref),
##     LRT = LRT,
##     nsim = nsim,
##     ctime = elapsed
##   )
##   class(ans) <- "PBrefdist"
##   return(ans)
## }

## #' @export
## pb_refdist_sequential <- function(
##   fit1,
##   fit0,
##   h = 20,
##   nsim = 1000,
##   batch_size = 50,
##   engine = c("serial", "parallel", "future"),
##   nworkers = 2,
##   verbose = FALSE
## ) {
##   t0 <- proc.time()
##   engine <- match.arg(engine)
  
##   if (verbose) cat("Computing observed likelihood ratio statistic via getLRT()...\n")
##   LRT <- getLRT(fit1, fit0)
##   tobs <- LRT[1]
  
##   if (is.infinite(h)) {
##     if (verbose) cat("h = Inf detected. Falling back to fixed-nsim sampling.\n")
##     return(pb_refdist(
##       fit1 = fit1,
##       fit0 = fit0,
##       nsim = nsim,
##       engine = engine,
##       nworkers = nworkers,
##       verbose = verbose
##     ))
##   }
  
##   total_sims <- 0
##   hits <- 0
##   ref <- numeric(0)
  
##   if (verbose) {
##     cat(sprintf("Starting sequential bootstrap: target %d hits or max %d simulations\n", h, nsim))
##   }
  
##   while (hits < h && total_sims < nsim) {
##     n_batch <- min(batch_size, nsim - total_sims)
    
##     if (verbose) cat(sprintf("Simulating batch of %d...\n", n_batch))
    
##     simdata <- simulate(fit0, nsim = n_batch)
    
##     worker_fun <- function(x) {
##       ll1 <- logLik(refit(fit1, newresp = x))
##       ll0 <- logLik(refit(fit0, newresp = x))
##       as.numeric(ll1 - ll0)
##     }
    
##     if (engine == "serial") {
##       res <- sapply(simdata, worker_fun)
      
##     } else if (engine == "parallel") {
##       cl <- parallel::makeCluster(nworkers)
##       on.exit(parallel::stopCluster(cl), add = TRUE)
##       parallel::clusterExport(cl, varlist = c("worker_fun", "fit1", "fit0", "refit"), envir = environment())
##       res <- unlist(parallel::parLapply(cl, simdata, worker_fun))
      
##     } else if (engine == "future") {
##       requireNamespace("furrr")
##       requireNamespace("future")
##       future::plan(future::multisession, workers = nworkers)
      
##       ## Ensure required packages are loaded on all workers
##       if (!is.null(future::getDefaultCluster())) {
##         parallel::clusterCall(future::getDefaultCluster(), function() {
##           library(pbkrtest)
##           library(nlme)
##           library(lme4)
##         })
##       }
      
##       res <- unlist(
##         furrr::future_map(
##           simdata,
##           worker_fun,
##           .options = furrr::furrr_options(seed = TRUE)
##         )
##       )
##     } else {
##       stop("Invalid engine")
##     }
    
##     ref <- c(ref, unname(res))
##     batch_hits <- sum(res >= tobs)
##     hits <- hits + batch_hits
##     total_sims <- total_sims + n_batch
    
##     if (verbose) {
##       cat(sprintf("Batch done. Hits in batch: %d | Total hits: %d | Total sims: %d\n", 
##                   batch_hits, hits, total_sims))
##     }
##   }
  
##   # Bias-corrected estimate
##   p_hat <- (hits + 1) / (total_sims + 1)
##   se_p  <- sqrt(p_hat * (1 - p_hat) / (total_sims + 1))
  
##   elapsed <- unname((proc.time() - t0))[3]
##   result <- list(
##     ref = ref,
##     LRT = LRT,
##     hits = hits,
##     total_sims = total_sims,
##     p.value = p_hat,
##     se = se_p,
##     h_target = h,
##     nsim_max = nsim,
##     ctime = elapsed
##   )
##   class(result) <- "PBrefdist"
##   return(result)
## }


## #' @export
## pb_refdist <- function(
##   fit1,
##   fit0,
##   nsim = 1000,
##   engine = c("serial", "parallel", "future"),
##   nworkers = 2,
##   verbose = FALSE
## ) {
##   t0 <- proc.time()
##   engine <- match.arg(engine)
  
##   if (verbose) cat("Computing observed likelihood ratio statistic via getLRT()...\n")
##   LRT <- getLRT(fit1, fit0)
  
##   if (verbose) {
##     cat(sprintf("Simulating %d replicates under null (engine = %s)\n", nsim, engine))
##   }
  
##   simdata <- simulate(fit0, nsim = nsim)
  
##   worker_fun <- function(x) {
##     ll1 <- logLik(refit(fit1, newresp = x))
##     ll0 <- logLik(refit(fit0, newresp = x))
##     as.numeric(ll1 - ll0)
##   }
  
##   if (engine == "serial") {
##     ref <- sapply(simdata, worker_fun)
    
##   } else if (engine == "parallel") {
##     cl <- parallel::makeCluster(nworkers)
##     on.exit(parallel::stopCluster(cl))
##     parallel::clusterExport(cl, varlist = c("worker_fun", "fit1", "fit0", "refit"), envir = environment())
##     ref <- unlist(parallel::parLapply(cl, simdata, worker_fun))
    
##   } else if (engine == "future") {
##     requireNamespace("furrr")
##     requireNamespace("future")
##     future::plan(future::multisession, workers = nworkers)
    
##     ## Ensure required packages are loaded on all workers
##     clust <- try(parallel::getDefaultCluster(), silent = TRUE)
##     if (!inherits(clust, "try-error") && !is.null(clust)) {
##       parallel::clusterCall(clust, function() {
##         library(pbkrtest)
##         library(nlme)
##         library(lme4)
##       })
##     }
    
##     ref <- unlist(
##       furrr::future_map(
##         simdata,
##         worker_fun,
##         .options = furrr::furrr_options(seed = TRUE)
##       )
##     )
##   } else {
##     stop("Invalid engine")
##   }

##   elapsed <- unname((proc.time() - t0))[3]    
##   ans <- list(
##     ref = unname(ref),
##     LRT = LRT,
##     nsim = nsim,
##     ctime = elapsed
##   )
##   class(ans) <- "PBrefdist"
##   return(ans)
## }


## #' @export
## pb_refdist_sequential <- function(
##   fit1,
##   fit0,
##   h = 20,
##   nsim = 1000,
##   batch_size = 50,
##   engine = c("serial", "parallel", "future"),
##   nworkers = 2,
##   verbose = FALSE
## ) {
##   t0 <- proc.time()
##   engine <- match.arg(engine)
  
##   if (verbose) cat("Computing observed likelihood ratio statistic via getLRT()...\n")
##   LRT <- getLRT(fit1, fit0)
##   tobs <- LRT[1]
  
##   if (is.infinite(h)) {
##     if (verbose) cat("h = Inf detected. Falling back to fixed-nsim sampling.\n")
##     return(pb_refdist(
##       fit1 = fit1,
##       fit0 = fit0,
##       nsim = nsim,
##       engine = engine,
##       nworkers = nworkers,
##       verbose = verbose
##     ))
##   }
  
##   total_sims <- 0
##   hits <- 0
##   ref <- numeric(0)
  
##   if (verbose) {
##     cat(sprintf("Starting sequential bootstrap: target %d hits or max %d simulations\n", h, nsim))
##   }
  
##   while (hits < h && total_sims < nsim) {
##     n_batch <- min(batch_size, nsim - total_sims)
    
##     if (verbose) cat(sprintf("Simulating batch of %d...\n", n_batch))
    
##     simdata <- simulate(fit0, nsim = n_batch)
    
##     worker_fun <- function(x) {
##       ll1 <- logLik(refit(fit1, newresp = x))
##       ll0 <- logLik(refit(fit0, newresp = x))
##       as.numeric(ll1 - ll0)
##     }
    
##     if (engine == "serial") {
##       res <- sapply(simdata, worker_fun)
      
##     } else if (engine == "parallel") {
##       cl <- parallel::makeCluster(nworkers)
##       on.exit(parallel::stopCluster(cl), add = TRUE)
##       parallel::clusterExport(cl, varlist = c("worker_fun", "fit1", "fit0", "refit"), envir = environment())
##       res <- unlist(parallel::parLapply(cl, simdata, worker_fun))
      
##     } else if (engine == "future") {
##       requireNamespace("furrr")
##       requireNamespace("future")
##       future::plan(future::multisession, workers = nworkers)
      
##       ## Ensure required packages are loaded on all workers
##       clust <- try(parallel::getDefaultCluster(), silent = TRUE)
##       if (!inherits(clust, "try-error") && !is.null(clust)) {
##         parallel::clusterCall(clust, function() {
##           library(pbkrtest)
##           library(nlme)
##           library(lme4)
##         })
##       }
      
##       res <- unlist(
##         furrr::future_map(
##           simdata,
##           worker_fun,
##           .options = furrr::furrr_options(seed = TRUE)
##         )
##       )
##     } else {
##       stop("Invalid engine")
##     }
    
##     ref <- c(ref, unname(res))
##     batch_hits <- sum(res >= tobs)
##     hits <- hits + batch_hits
##     total_sims <- total_sims + n_batch
    
##     if (verbose) {
##       cat(sprintf("Batch done. Hits in batch: %d | Total hits: %d | Total sims: %d\n", 
##                   batch_hits, hits, total_sims))
##     }
##   }
  
##   # Bias-corrected estimate
##   p_hat <- (hits + 1) / (total_sims + 1)
##   se_p  <- sqrt(p_hat * (1 - p_hat) / (total_sims + 1))
  
##   elapsed <- unname((proc.time() - t0))[3]
##   result <- list(
##     ref = ref,
##     LRT = LRT,
##     hits = hits,
##     total_sims = total_sims,
##     p.value = p_hat,
##     se = se_p,
##     h_target = h,
##     nsim_max = nsim,
##     ctime = elapsed
##   )
##   class(result) <- "PBrefdist"
##   return(result)
## }


