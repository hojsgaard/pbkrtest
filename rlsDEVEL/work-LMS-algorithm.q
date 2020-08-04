
###
### Filtering synthetic data using LMS algorithm
###

tvar1 <- 1:105
mu1   <- 10 + 10*cos(2*pi*tvar1/21) + 5*sin(2*pi*tvar1/21)
tvar2 <- 106:200
mu2   <- rep(15, length(tvar2))
mu    <- c(mu1, mu2)
tvar  <- c(tvar1, tvar2)
yvar <- mu + rnorm(length(tvar))

plot(yvar~tvar, type='l')


covariate <- function(ttt, ...){
  c(1, cos(2*pi*ttt/23), sin(2*pi*ttt/23))
}

fun <- function(parm, ttt){
  sum(covariate(ttt)*parm)
}

parm0 <- c(0,5,5)
alpha <- 0.05
cparm <- parm0
res   <- list()
fit   <- list()
for (ii in seq_along(tvar)){
  ct    <- tvar[ii]
  cy    <- yvar[ii]
  xxx   <- covariate(ct)
  inner <- 0
  repeat{
    inner <- inner + 1
    cparm2 <- cparm + alpha * xxx * (cy-fun(cparm,ct))
    parm.diff <- max(abs((cparm2-cparm)/cparm))
    if ( parm.diff < 0.001){
      cat(sprintf("inner loops %i parm.diff %f\n", inner, parm.diff))
      break()
    }
    cparm <- cparm2
  }
  fit[[ii]] <- fun(cparm,ct)
  res[[ii]] <- cparm
}

res2 <- do.call(rbind, res)
fit2 <- unlist(fit)

plot(yvar~tvar)
matplot(tvar, cbind(res2,fit2,yvar),type="l")
abline(h=0)

############################################################################
############################################################################


