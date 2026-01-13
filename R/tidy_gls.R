#' Tidy method for nlme::gls objects
#'
#' @param x A fitted model of class "gls" (from nlme).
#' @param conf.int Logical; include confidence intervals?
#' @param conf.level Confidence level for intervals.
#' @param exponentiate Logical; return exp(estimate) etc.?
#' @param ... Unused (for S3 compatibility).
#'
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @return data.frame with columns term, estimate, std.error, statistic, p.value
#'   and optionally conf.low, conf.high.
#' @export
tidy.gls <- function(x,
                     conf.int = FALSE,
                     conf.level = 0.95,
                     exponentiate = FALSE,
                     ...) {

  s <- summary(x)
  tt <- s$tTable
  if (is.null(tt)) 
    stop("summary(x)$tTable is NULL; cannot tidy gls object.")

  out <- data.frame(
    term      = rownames(tt),
    estimate  = tt[, "Value"],
    std.error = tt[, "Std.Error"],
    statistic = tt[, "t-value"],
    p.value   = tt[, "p-value"],
    row.names = NULL,
    check.names = FALSE
  )

  if (isTRUE(conf.int)) {
    alpha <- 1 - conf.level

    # nlme::summary.gls bruger typisk residual df (s$df)
    df <- s$df
    if (is.null(df) || !is.finite(df)) {
      # fallback: approx residual df
      df <- nobs(x) - length(stats::coef(x))
    }

    crit <- stats::qt(1 - alpha/2, df = df)
    out$conf.low  <- out$estimate - crit * out$std.error
    out$conf.high <- out$estimate + crit * out$std.error
  }

  if (isTRUE(exponentiate)) {
    out$estimate <- exp(out$estimate)
    if (isTRUE(conf.int)) {
      out$conf.low  <- exp(out$conf.low)
      out$conf.high <- exp(out$conf.high)
    }
  }

  out
}


#' Glance method for nlme::gls objects
#'
#' @param x A fitted model of class "gls" (from nlme).
#' @param ... Unused (for S3 compatibility).
#'
#' @return one-row data.frame with overall fit statistics.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @export
glance.gls <- function(x, ...) {

  ll <- as.numeric(stats::logLik(x))
  df_ll <- attr(stats::logLik(x), "df")

  # Residual "sigma" i gls (kan være NULL i nogle kanttilfælde)
  sigma <- tryCatch(x$sigma, error = function(e) NA_real_)
  if (is.null(sigma)) sigma <- NA_real_

  # Nobs: nlme har ofte nobs() metode, ellers fallback
  n <- tryCatch(stats::nobs(x), error = function(e) NA_integer_)
  if (is.na(n)) {
    n <- tryCatch(nrow(stats::model.frame(x)), error = function(e) NA_integer_)
  }

  # Residual df (typisk n - p)
  p <- length(stats::coef(x))
  df_residual <- if (is.finite(n)) n - p else NA_real_

  data.frame(
    nobs        = n,
    df.residual = df_residual,
    logLik      = ll,
    df.logLik   = df_ll,
    AIC         = stats::AIC(x),
    BIC         = stats::BIC(x),
    sigma       = sigma,
    row.names   = NULL,
    check.names = FALSE
  )
}

#' @export
tidy.summary.gls <- function(x, ...) {
  tt <- x$tTable
  if (is.null(tt)) stop("summary.gls mangler tTable.")

  data.frame(
    term      = rownames(tt),
    estimate  = tt[, "Value"],
    std.error = tt[, "Std.Error"],
    statistic = tt[, "t-value"],
    p.value   = tt[, "p-value"],
    row.names = NULL,
    check.names = FALSE
  )
}

