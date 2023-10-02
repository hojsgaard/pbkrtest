## Multicore facilities for pbkrtest
### Søren Højsgaard
Date: 


```r
date()
```

```
## [1] "Fri May 18 19:50:44 2018"
```

```r
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
```

## Controlling parallel computing


```r
NSIM <- 500 # Number of simulations
NCL <- 3 # Number of clusters
DT  <- 1 # Details
```

Needed for parallel on windows


```r
CLUS <- parallel::makeCluster(rep("localhost", 4))
```

### Global settings


```r
options(mc.cores=NULL, pb.cl=NULL)
ss <- PBrefdist(lg, sm, nsim=NSIM, details=DT)
```

```
## trying to retrieve 'cl' from options('pb.cl') ... 
## trying to retrieve 'cl' from options('mc.cores')... 
## cl can not be retrieved anywhere; setting cl=1
## doing mclapply, cl =  1 
## Reference distribution with   500 samples; computing time:  7.80 secs.
```

```r
options(mc.cores=NCL, pb.cl=NULL)
ss <- PBrefdist(lg, sm, nsim=NSIM, details=DT)
```

```
## trying to retrieve 'cl' from options('pb.cl') ... 
## trying to retrieve 'cl' from options('mc.cores')... 
##   got 'cl' from options(mc.cores); cl =  3 
## doing mclapply, cl =  3 
## Reference distribution with   498 samples; computing time:  4.16 secs.
```

```r
options(mc.cores=NULL, pb.cl=CLUS)
ss <- PBrefdist(lg, sm, nsim=NSIM, details=DT)
```

```
## trying to retrieve 'cl' from options('pb.cl') ... 
##   got 'cl' from options; length(cl) =  4 
## doing clusterCall, nclusters =  4 
## Reference distribution with   500 samples; computing time:  6.60 secs.
```

### Local settings


```r
options(mc.cores=NULL, pb.cl=NULL)
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=NULL, details=DT)
```

```
## trying to retrieve 'cl' from options('pb.cl') ... 
## trying to retrieve 'cl' from options('mc.cores')... 
## cl can not be retrieved anywhere; setting cl=1
## doing mclapply, cl =  1 
## Reference distribution with   500 samples; computing time:  8.00 secs.
```

```r
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=NULL, details=DT)
```

```
## trying to retrieve 'cl' from options('pb.cl') ... 
## trying to retrieve 'cl' from options('mc.cores')... 
## cl can not be retrieved anywhere; setting cl=1
## doing mclapply, cl =  1 
## Reference distribution with   500 samples; computing time:  7.72 secs.
```

```r
options(mc.cores=NULL, pb.cl=NULL)
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=NCL, details=DT)
```

```
## valid 'cl' specified in call 
## doing mclapply, cl =  3 
## Reference distribution with   498 samples; computing time:  4.07 secs.
```

```r
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=CLUS, details=DT)
```

```
## valid 'cl' specified in call 
## doing clusterCall, nclusters =  4 
## Reference distribution with   500 samples; computing time:  4.70 secs.
```

```r
#ss <- PBrefdist(lg, sm, nsim=NSIM, cl="ouch", details=DT)

options(mc.cores=NCL, pb.cl=NULL)
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=NCL, details=DT)
```

```
## valid 'cl' specified in call 
## doing mclapply, cl =  3 
## Reference distribution with   498 samples; computing time:  4.11 secs.
```

```r
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=CLUS, details=DT)
```

```
## valid 'cl' specified in call 
## doing clusterCall, nclusters =  4 
## Reference distribution with   500 samples; computing time:  4.30 secs.
```

```r
options(mc.cores=NULL, pb.cl=CLUS)
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=NCL, details=DT)
```

```
## valid 'cl' specified in call 
## doing mclapply, cl =  3 
## Reference distribution with   498 samples; computing time:  4.62 secs.
```

```r
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=CLUS, details=DT)
```

```
## valid 'cl' specified in call 
## doing clusterCall, nclusters =  4 
## Reference distribution with   500 samples; computing time:  4.21 secs.
```

