#' ## How much do we gain with multicore
#' ### Søren Højsgaard
#' Date: 
date()

require(lme4)
require(pbkrtest)
library(devtools)
options(warn=-1)

## Linear mixed effects model:
data(beets, package="pbkrtest")
lg <- lmer(sugpct ~ block + sow + harvest + (1|block:harvest),
           data=beets, REML=FALSE)
sm <- update(lg, .~. -harvest)

load_all("pbkrtest")

#' ## Controlling parallel computing
NCL <- 4 # Number of clusters
DT  <- 1 # Details

options(mc.cores=NCL, pb.cl=NULL)
NSIM <- 100 # Number of simulations
ss <- PBrefdist(lg, sm, nsim=NSIM, details=DT)
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=1, details=DT)

NSIM <- 500 # Number of simulations
ss <- PBrefdist(lg, sm, nsim=NSIM, details=DT)
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=1, details=DT)

NSIM <- 1000 # Number of simulations
ss <- PBrefdist(lg, sm, nsim=NSIM, details=DT)
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=1, details=DT)

## NSIM <- 5000 # Number of simulations
## ss <- PBrefdist(lg, sm, nsim=NSIM, details=DT)
## ss <- PBrefdist(lg, sm, nsim=NSIM, cl=1, details=DT)


#' ## Increasing number of clusters

NSIM <- 1000 # Number of simulations

NCL <- 1 # Number of clusters
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=NCL, details=DT)

NCL <- 2 # Number of clusters
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=NCL, details=DT)

NCL <- 3 # Number of clusters
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=NCL, details=DT)

NCL <- 4 # Number of clusters
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=NCL, details=DT)

