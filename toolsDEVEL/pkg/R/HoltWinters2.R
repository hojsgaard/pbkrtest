##
## Holt Winters linear filter modified so that it accounts for non-equidistant
## time points
##

hwsmooth <- function(x,y=NULL, alpha=0.5, beta=NULL, iter=1, l.start=NULL, b.start=NULL, fit=FALSE,
                     pstep=1, start=c(alpha=.1, beta=.1)){
  if (is.null(y)){
    y <- x
    x <- 1:length(y)
  }
  d.x     <- c(0, diff(x))
  first   <-  which(!is.na(y))[1]
  yuse    <- y
  yuse[1] <- yuse[first]
  a.tilde <- b.tilde <- rep(0, length(y))
  a.tilde[1] <- if (is.null(l.start)) yuse[1]  else l.start
  b.tilde[1] <- if (is.null(b.start)) 0        else b.start
  
  hw <- function(alpha, beta){
    a.w <- alpha^(1/d.x)
    b.w <- beta^(1/d.x)
    if (is.null(beta)){  ## No trend
      for (ii in 2:length(y)){
        if (!is.na(y[ii])){
          a.tilde[ii] <- a.w[ii]*yuse[ii] + (1-a.w[ii])*a.tilde[ii-1]
        } else {
          a.tilde[ii]  <- a.tilde[ii-1]
        }
      }
      y.pred    <- a.tilde[1:(length(y)-pstep)]
      pred.err  <- y[(pstep+1):length(y)] - y.pred
      SSE       <- var(na.omit(pred.err))
    } else {
      for (ii in 2:length(y)){
        if (!is.na(y[ii])){
          a.tilde[ii] <- a.w[ii]*yuse[ii] + (1-a.w[ii])*(a.tilde[ii-1] + b.tilde[ii-1]*d.x[ii])
          b.tilde[ii] <- b.w[ii]*(a.tilde[ii]-a.tilde[ii-1])/d.x[ii] + (1-b.w[ii])*b.tilde[ii-1]
        } else {
          a.tilde[ii]  <- a.tilde[ii-1]
          b.tilde[ii]  <- b.tilde[ii-1]
        }
      }
      ##y.pred    <- a.tilde[1:(length(y)-pstep)] + b.tilde[1:(length(y)-pstep)]*d.x[-(1:pstep)]
      y.pred    <- a.tilde[1:(length(y)-pstep)] +
        b.tilde[1:(length(y)-pstep)]*(x[-(1:pstep)]-x[1:(length(y)-pstep)])
      pred.err  <- y[(pstep+1):length(y)] - y.pred
      SSE       <- var(na.omit(pred.err))
    }
    return(list(SSE=SSE, a.tilde=a.tilde, b.tilde=b.tilde))
  }

  if (fit){
    if (is.null(beta)){
      error <- function(p) hw(p, beta)$SSE
      alpha <- optimize(error, lower = 0, upper = 1)$minimum
      ##cat("alpha (fitted):", alpha,"\n")
    } else {
      error <- function(p) hw(p[1], p[2])$SSE
      sol <- optim(c(alpha, beta),
                   error, method = "L-BFGS-B", lower = c(0, 0), 
                   upper = c(1, 1), control = list(trace=0))
      alpha <- sol$par[1]
      beta  <- sol$par[2]
      ##cat("alpha (fitted):", alpha,"beta (fitted)",beta,"\n")
    }
  }

  xxx    <- hw(alpha, beta)
  if (is.null(beta)){
    yhat   <- xxx$a.tilde
    fitted <- as.data.frame(cbind(yhat, level=xxx$a.tilde))
    coef = c(alpha=alpha)
  } else {
    yhat   <- xxx$a.tilde + xxx$b.tilde * d.x
    fitted <- as.data.frame(cbind(yhat, level=xxx$a.tilde, trend=xxx$b.tilde))
    coef = c(alpha=alpha, beta=beta)
  }
  ans    <- list(x=x, y=yhat, fitted=fitted, alpha=alpha, beta=beta, fit=fit)

  ans    <- c(ans, xxx)
  class(ans) <- "HoltWinters2"
  return(ans)
}


print.HoltWinters2 <- function(x, ...){
  if (is.null(x$beta))
    cat("Holt-Winters exponential smoothing (without trend)\n")
  else
    cat("Holt-Winters exponential smoothing (with trend)\n")

  cat("Smoothing parameters: (fitted):", x$fit ,"\n")
  cat(" alpha:", x$alpha, "\n")
  cat(" beta :", if(is.null(x$beta)) 0 else x$beta, "\n")
}



## HW2<-function (x, alpha = NULL, beta = NULL, gamma = NULL, seasonal = c("additive", 
##     "multiplicative"), start.periods = 3, l.start = NULL, b.start = NULL, 
##     s.start = NULL, optim.start = c(alpha = 0.3, beta = 0.1, 
##         gamma = 0.1), optim.control = list()) 
## {
##     x <- as.ts(x)
##     seasonal <- match.arg(seasonal)
##     f <- frequency(x)
##     if (!is.null(alpha) && alpha == 0) 
##         stop("cannot fit models without level ('alpha' must not be 0).")
##     if (!all(is.null(c(alpha, beta, gamma))) && any(c(alpha, 
##         beta, gamma) < 0 || c(alpha, beta, gamma) > 1)) 
##         stop("'alpha', 'beta' and 'gamma' must be within the unit interval.")
##     if ((is.null(gamma) || gamma > 0)) {
##         if (seasonal == "multiplicative" && any(x <= 0)) 
##             stop("data must be strictly non-negative for multiplicative Holt-Winters")
##         if (start.periods < 3) 
##             stop("need at least 3 periods to compute seasonal start values")
##     }
##     if (!is.null(gamma) && gamma == 0) {
##         expsmooth <- !is.null(beta) && (beta == 0)
##         if (is.null(l.start)) 
##             l.start <- if (expsmooth) 
##                 x[1]
##             else x[2]
##         if (is.null(b.start)) 
##             if (is.null(beta) || beta > 0) 
##                 b.start <- x[2] - x[1]
##         start.time <- 3 - expsmooth
##         s.start <- 0
##     }
##     else {
##         start.time <- f + 1
##         wind <- start.periods * f
##         st <- decompose(ts(x[1:wind], start = start(x), frequency = f), 
##             seasonal)
##         dat <- na.omit(st$trend)
##         m <- lm(dat ~ c(1:length(dat)))
##         if (is.null(l.start)) 
##             l.start <- as.vector(coef(m)[1])
##         if (is.null(b.start)) 
##             b.start <- as.vector(coef(m)[2])
##         if (is.null(s.start)) 
##             s.start <- st$figure
##     }
##     len <- length(x) - start.time + 1
##     hw <- function(alpha, beta, gamma) .C("HoltWinters", as.double(x), 
##         as.integer(length(x)), as.double(alpha), as.double(beta), 
##         as.double(gamma), as.integer(start.time), as.integer(!+(seasonal == 
##             "multiplicative")), as.integer(f), a = as.double(l.start), 
##         b = as.double(b.start), s = as.double(s.start), SSE = as.double(0), 
##         level = double(len + 1), trend = double(len + 1), seasonal = double(len + 
##             f), PACKAGE = "stats")
##     if (is.null(gamma)) {
##         if (is.null(alpha)) {
##             if (is.null(beta)) {
##                 error <- function(p) hw(p[1], p[2], p[3])$SSE
##                 sol <- optim(optim.start, error, method = "L-BFGS-B", 
##                   lower = c(0, 0, 0), upper = c(1, 1, 1), control = optim.control)
##                 alpha <- sol$par[1]
##                 beta <- sol$par[2]
##                 gamma <- sol$par[3]
##             }
##             else {
##                 error <- function(p) hw(p[1], beta, p[2])$SSE
##                 sol <- optim(c(optim.start["alpha"], optim.start["gamma"]), 
##                   error, method = "L-BFGS-B", lower = c(0, 0), 
##                   upper = c(1, 1), control = optim.control)
##                 alpha <- sol$par[1]
##                 gamma <- sol$par[2]
##             }
##         }
##         else {
##             if (is.null(beta)) {
##                 error <- function(p) hw(alpha, p[1], p[2])$SSE
##                 sol <- optim(c(optim.start["beta"], optim.start["gamma"]), 
##                   error, method = "L-BFGS-B", lower = c(0, 0), 
##                   upper = c(1, 1), control = optim.control)
##                 beta <- sol$par[1]
##                 gamma <- sol$par[2]
##             }
##             else {
##                 error <- function(p) hw(alpha, beta, p)$SSE
##                 gamma <- optimize(error, lower = 0, upper = 1)$minimum
##             }
##         }
##     }
##     else {
##         if (is.null(alpha)) {
##             if (is.null(beta)) {
##                 error <- function(p) hw(p[1], p[2], gamma)$SSE
##                 sol <- optim(c(optim.start["alpha"], optim.start["beta"]), 
##                   error, method = "L-BFGS-B", lower = c(0, 0), 
##                   upper = c(1, 1), control = optim.control)
##                 alpha <- sol$par[1]
##                 beta <- sol$par[2]
##             }
##             else {
##                 error <- function(p) hw(p, beta, gamma)$SSE
##                 alpha <- optimize(error, lower = 0, upper = 1)$minimum
##             }
##         }
##         else {
##             if (is.null(beta)) {
##                 error <- function(p) hw(alpha, p, gamma)$SSE
##                 beta <- optimize(error, lower = 0, upper = 1)$minimum
##             }
##         }
##     }

    
##     final.fit <- hw(alpha, beta, gamma)
##     fitted <- ts(cbind(xhat = final.fit$level[-len - 1], level = final.fit$level[-len - 
##         1], trend = if (beta > 0) 
##         final.fit$trend[-len - 1], season = if (gamma > 0) 
##         final.fit$seasonal[1:len]), start = start(lag(x, k = 1 - 
##         start.time)), frequency = frequency(x))
##     if (beta > 0) 
##         fitted[, 1] <- fitted[, 1] + fitted[, "trend"]
##     if (gamma > 0) 
##         fitted[, 1] <- if (seasonal == "multiplicative") 
##             fitted[, 1] * fitted[, "season"]
##         else fitted[, 1] + fitted[, "season"]
##     structure(list(fitted = fitted, x = x, alpha = alpha, beta = beta, 
##         gamma = gamma, coefficients = c(a = final.fit$level[len + 
##             1], b = if (beta > 0) final.fit$trend[len + 1], s = if (gamma > 
##             0) final.fit$seasonal[len + 1:f]), seasonal = seasonal, 
##         SSE = final.fit$SSE, call = match.call()), class = "HoltWinters")
## }

