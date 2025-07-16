
#' Parametric Bootstrap Model Comparison
#'
#' Compare two nested models using parametric bootstrap simulation of the likelihood ratio statistic.
#' Supports models fitted via lm, lme4 (lmer/glmer), nlme (lme/gls), etc.
#'
#' The models should both be fitted by maximum likelihood (not REML). If REML was used,
#' the function will automatically refit with REML = FALSE where possible.
#'
#' @param fit1 The larger (alternative) model.
#' @param fit0 The smaller (null) model.
#' @param nsim Number of simulations. In fixed bootstrap: total number of simulations. In sequential bootstrap: maximum number of simulations allowed.
#' @param sequential Logical; if TRUE, use sequential bootstrap sampling to reach target number of extreme hits.
#' @param h Number of extreme hits to target in sequential sampling.
#' @param engine Parallelisation engine: "serial", "parallel", or "future".
#' @param nworkers Number of workers for parallel/future engine.
#' @param verbose Logical; if TRUE, print progress messages.

#' @note
#' **Best Practice:** Always fit your models with the `data=` argument.
#' This ensures all covariates used in the model formula are stored with the model object,
#' enabling reliable simulation and refitting for bootstrap analysis,
#' including on parallel workers. Without `data=`, refitting may fail in parallel contexts
#' and reproducibility is compromised.
#' 
#' @return An object of class \code{PBmodcomp}, with print(), summary(), and plot() methods.
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
#'   res_lm <- pb2_modcomp(lm_fit1, lm_fit0, nsim = 200)
#'   res_lm
#'   summary(res_lm)
#'   plot(res_lm, show.chisq=TRUE)
#'
#'   # GLS example
#'   gls_fit1 <- nlme::gls(Reaction ~ Days + Days2, data = sleepstudy, method = "ML")
#'   gls_fit0 <- update(gls_fit1, . ~ . - Days2)
#'   set.seed(42)
#'   res_gls <- pb2_modcomp(gls_fit1, gls_fit0, nsim = 200)
#'   res_gls
#'   summary(res_gls)
#'   plot(res_gls, show.chisq=TRUE)
#'
#'   # LME example
#'   lme_fit1 <- nlme::lme(Reaction ~ Days + Days2, random = ~ 1 | Subject,
#'                         data = sleepstudy, method = "ML")
#'   lme_fit0 <- update(lme_fit1, . ~ . - Days2)
#'   set.seed(42)
#'   res_lme <- pb2_modcomp(lme_fit1, lme_fit0, nsim = 200)
#'   res_lme
#'   summary(res_lme)
#'   plot(res_lme, show.chisq=TRUE)
#'
#'   # LMER example (lme4)
#'   lmer_fit1 <- lme4::lmer(Reaction ~ Days + Days2 + (1 | Subject),
#'                           data = sleepstudy, REML = FALSE)
#'   lmer_fit0 <- update(lmer_fit1, . ~ . - Days2)
#'   set.seed(42)
#'   res_lmer <- pb2_modcomp(lmer_fit1, lmer_fit0, nsim = 200)
#'   res_lmer
#'   summary(res_lmer)
#'   plot(res_lmer, show.chisq=TRUE)
#'
#'   # Sequential example
#'   set.seed(42)
#'   res_seq <- pb2_modcomp(lmer_fit1, lmer_fit0, sequential = TRUE, h = 20, nsim = 500)
#'   res_seq
#'   summary(res_seq)
#'   plot(res_seq, show.chisq=TRUE)
#' 
#' }
#' @export
pb2_modcomp <- function(fit1, fit0,
                        nsim = 1000,
                        sequential = FALSE,
                        h = 20,
                        engine = "serial",
                        nworkers = 2,
                        verbose = FALSE) {

  # Ensure ML fit if necessary
  fit1 <- ensure_ML(fit1)
  fit0 <- ensure_ML(fit0)

    check_model_has_data(fit1)
    check_model_has_data(fit0)
  # Get observed statistic
  LRTstat <- getLRT(fit1, fit0)

  # Simulate reference distribution
  if (sequential) {
    if (verbose) message("Using sequential bootstrap sampling")
    ref <- pb_refdist_sequential(
      fit1, fit0,
      h = h,
      nsim = nsim,
      engine = engine,
      nworkers = nworkers,
      verbose = verbose
    )
  } else {
    if (verbose) message("Using fixed-size bootstrap sampling")
    ref <- pb_refdist(
      fit1, fit0,
      nsim = nsim,
      engine = engine,
      nworkers = nworkers,
      verbose = verbose
    )
  }

  # Summarize results
  result <- summarize_pb(LRTstat, ref$ref)
  result$formula_large <- tryCatch(formula(fit1), error = function(e) NA)
  result$formula_small <- tryCatch(formula(fit0), error = function(e) NA)

  class(result) <- c("PBmodcomp2", class(result))
  result
}


#' @keywords internal
#' @noRd
ensure_ML <- function(fit) {
  if (inherits(fit, "lmerMod") || inherits(fit, "lmerModLmerTest")) {
    if (lme4::isREML(fit)) {
      fit <- update(fit, REML = FALSE)
    }
  }
  if (inherits(fit, "lme")) {
    if (!is.null(fit$method) && fit$method != "ML") {
      fit <- update(fit, method = "ML")
    }
  }
  fit
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
#' lr_sim_lm <- pb_refdist(lm_fit1, lm_fit0, nsim = 50, engine = "future", nworkers =2)
#' summarize_pb(lrt_lm, lr_sim_lm)
#' 
#' gls_fit1 <- gls(Reaction ~ Days + Days2, data = sleepstudy, method="ML")
#' gls_fit0 <- update(gls_fit1, . ~ . - Days2)
#' set.seed(42)
#' lrt_gls <- getLRT(gls_fit1, gls_fit0)
#' lr_sim_gls <- pb_refdist(gls_fit1, gls_fit0, nsim = 50, engine = "future", nworkers = 2)
#' summarize_pb(lrt_gls, lr_sim_gls)
#' 
#' lme_fit1 <- lme(Reaction ~ Days + Days2, random = ~ 1 | Subject, data = sleepstudy, method="ML")
#' lme_fit0 <- update(lme_fit1, . ~ . - Days2)
#' set.seed(42)
#' lrt_lme <- getLRT(lme_fit1, lme_fit0)
#' lr_sim_lme <- pb_refdist(lme_fit1, lme_fit0, nsim = 50, engine = "future", nworkers = 2)
#' summarize_pb(lrt_lme, lr_sim_lme)
#' 
#' lmer_fit1 <- lmer(Reaction ~ Days + Days2 + (1 | Subject), data = sleepstudy, REML=FALSE)
#' lmer_fit0 <- update(lmer_fit1, . ~ . - Days2)
#' set.seed(42)
#' lrt_lmer <- getLRT(lmer_fit1, lmer_fit0)
#' lr_sim_lmer <- pb_refdist(lmer_fit1, lmer_fit0, nsim = 50, engine = "future", nworkers = 2)
#' summarize_pb(lrt_lmer, lr_sim_lmer) |> summary()
#'
#' lr_sim_lmer <- pb_refdist_sequential(lmer_fit1, lmer_fit0, nsim = 50,
#'     engine = "future", nworkers = 2)
#' summarize_pb(lrt_lmer, lr_sim_lmer)

#' @export
summarize_pb <- function(LRTstat, ref, conf.level = 0.95) {

    if (inherits(ref, "PBrefdist")) {
        ref <- ref$ref
    }
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
print.PBmodcomp2 <- function(x, ...) {
  cat("\nParametric Bootstrap Model Comparison (Compact Summary)\n")
  cat("---------------------------------------------------------\n")
  cat(sprintf("Observed statistic : %.4f\n", x$test["Asymptotic", "stat"]))
  cat(sprintf("Degrees of freedom  : %.0f\n", x$test["Asymptotic", "df"]))
  cat(sprintf("Asymptotic p-value  : %.4f\n", x$test["Asymptotic", "p.value"]))
  cat(sprintf("Bootstrap p-value   : %.4f\n", x$test["Bootstrap", "p.value"]))
  cat(sprintf("Percentile p-value  : %.4f\n", x$p.ECDF))
  cat(sprintf("Simulations         : %d\n", x$samples["nsim"]))
  invisible(x)
}

#' @export
summary.PBmodcomp2 <- function(object, ...) {
  cat("\nParametric Bootstrap Model Comparison (Detailed Summary)\n")
  cat("---------------------------------------------------------\n")
  cat(sprintf("Observed statistic: %.4f\n", object$test["Asymptotic", "stat"]))
  cat(sprintf("Degrees of freedom (null): %.0f\n", object$test["Asymptotic", "df"]))
  
  cat("\nBootstrap settings:\n")
  cat(sprintf("  Total simulations : %d\n", object$samples["nsim"]))
  cat(sprintf("  Positive samples  : %d\n", object$samples["npos"]))
  cat(sprintf("  Extreme hits      : %d\n", object$n.extreme))
  
  cat("\nMoments of bootstrap distribution:\n")
  cat(sprintf("  Mean   : %.3f\n", object$moment["mean"]))
  cat(sprintf("  Variance: %.3f\n", object$moment["var"]))
  cat(sprintf("  Median : %.3f\n", object$moment["median"]))
  
  cat("\nBias-corrected bootstrap p-value:\n")
  cat(sprintf("  %.4f (SE = %.4f; CI = [%.4f, %.4f])\n",
               object$test["Bootstrap", "p.value"], object$se, object$ci[1], object$ci[2]))
  
  cat(sprintf("Percentile-interpolated p-value: %.4f\n", object$p.ECDF))
  cat(sprintf("Asymptotic (Chi^2) p-value     : %.4f\n", object$test["Asymptotic", "p.value"]))
  
  cat("\nAll test approximations:\n")
  print(object$test)
  
  invisible(object)
}

#' @export
plot.PBmodcomp2 <- function(x, show.chisq=FALSE, bins=30, ...) {

  h <- hist(x$ref, breaks = bins, main = "Bootstrap Reference Distribution",
            xlab = "Simulated LRT Statistics", col = "gray80", border = "white", ...)

  abline(v = x$test["Asymptotic", "stat"], col = "red", lwd = 2)
  
  legend_labels <- c(
    sprintf("Observed = %.3f", x$test["Asymptotic", "stat"]),
    sprintf("Bootstrap p = %.4f", x$test["Bootstrap", "p.value"]),
    sprintf("Asymptotic p = %.4f", x$test["Asymptotic", "p.value"])
  )
  legend_colors <- c("red", NA, NA)
  legend_lwd <- c(2, NA, NA)
  
  if (show.chisq && !is.na(x$test["Asymptotic", "df"])) {
    df <- x$test["Asymptotic", "df"]
    xx <- seq(min(h$breaks), max(h$breaks), length=200)
    lines(xx, dchisq(xx, df=df) * length(x$ref) * diff(h$mids[1:2]), col="blue", lwd=2)
    
    legend_labels <- c(legend_labels, sprintf("Chi^2(df=%.0f)", df))
    legend_colors <- c(legend_colors, "blue")
    legend_lwd <- c(legend_lwd, 2)
  }
  
  legend("topright",
         legend = legend_labels,
         col = legend_colors,
         lwd = legend_lwd,
         bty = "n",
         text.col = "black")

  invisible(x)
}


check_model_has_data <- function(fit) {
  data_ok <- FALSE
  
  if (inherits(fit, c("gls", "lme"))) {
    data_ok <- tryCatch({
      d <- nlme::getData(fit)
      is.data.frame(d) && nrow(d) > 0
    }, error = function(e) FALSE)
  } else {
    data_ok <- tryCatch({
      d <- model.frame(fit)
      is.data.frame(d) && nrow(d) > 0
    }, error = function(e) FALSE)
  }
  
  if (!data_ok) {
    stop(
      "  Error: The model must have been fitted with the data= argument, ",
      "so that all covariates are available for refitting.\n\n",
      " Please refit your model like this:\n",
      "  gls(..., data = your_data)\n",
      "  lme(..., data = your_data)\n",
      "  lmer(..., data = your_data)\n",
      "  lm(..., data = your_data)\n"
    )
  }
}
