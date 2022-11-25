
##########################################################
###
### Likelihood ratio statistic
###
##########################################################

getLRT <- function(largeModel, smallModel){
  UseMethod("getLRT")
}

getLRT.merMod <-
    getLRT.mer <-
        function(largeModel, smallModel){
    ll.small <- logLik(update(smallModel, REML=FALSE))
    ll.large <- logLik(update(largeModel, REML=FALSE))
    tobs     <- 2 * (ll.large - ll.small)
    df11     <- attr(ll.large, "df") - attr(ll.small, "df")
    p.X2     <- 1 - pchisq(tobs, df11)
    c(tobs=tobs, df=df11, p.value=p.X2)
}


getLRT.lm <- function(largeModel, smallModel){
  ll.small <- logLik(smallModel)
  ll.large <- logLik(largeModel)
  tobs     <- 2 * (ll.large - ll.small)
  df11     <- attr(ll.large, "df") - attr(ll.small, "df")
  p.X2     <- 1 - pchisq(tobs, df11)
  c(tobs=tobs, df=df11, p.value=p.X2)
}


as.data.frame.PBmodcomp <- function(x, ...){
    out <- x$test
    attributes(out) <- c(attributes(out), x[-1])
    out
}

as.data.frame.summaryPB <- function(x, ...){
    out <- x$test
    attributes(out) <- c(attributes(out), x[-1])
    out
}






