#' ## Multicore facilities for pbkrtest
#' ### Søren Højsgaard
#' Date: 
date()

require(lme4)
require(pbkrtest)
library(devtools)
options(warn=-1)

## Linear mixed effects model:
data(beets, package="pbkrtest")
lg <- lmer(sugpct ~ block + sow + harvest +
               (1|block:harvest),
           data=beets, REML=FALSE)
sm <- update(lg, .~. -harvest)

#load_all("pbkrtest")

#' ## Controlling parallel computing
NSIM <- 500 # Number of simulations
NCL <- 3 # Number of clusters
DT  <- 1 # Details
#' Needed for parallel on windows
CLUS <- parallel::makeCluster(rep("localhost", 4))

#' ### Global settings
options(mc.cores=NULL, pb.cl=NULL)
ss <- PBrefdist(lg, sm, nsim=NSIM, details=DT)

options(mc.cores=NCL, pb.cl=NULL)
ss <- PBrefdist(lg, sm, nsim=NSIM, details=DT)

options(mc.cores=NULL, pb.cl=CLUS)
ss <- PBrefdist(lg, sm, nsim=NSIM, details=DT)

#' ### Local settings
options(mc.cores=NULL, pb.cl=NULL)
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=NULL, details=DT)
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=NULL, details=DT)

options(mc.cores=NULL, pb.cl=NULL)
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=NCL, details=DT)
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=CLUS, details=DT)
#ss <- PBrefdist(lg, sm, nsim=NSIM, cl="ouch", details=DT)

options(mc.cores=NCL, pb.cl=NULL)
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=NCL, details=DT)
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=CLUS, details=DT)

options(mc.cores=NULL, pb.cl=CLUS)
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=NCL, details=DT)
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=CLUS, details=DT)




