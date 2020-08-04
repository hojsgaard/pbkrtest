## How much do we gain with multicore
### Søren Højsgaard
Date: 


```r
date()
```

```
## [1] "Fri May 18 20:19:35 2018"
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

load_all("pbkrtest")
```

```
## Loading pbkrtest
```

## Controlling parallel computing


```r
NCL <- 4 # Number of clusters
DT  <- 1 # Details

options(mc.cores=NCL, pb.cl=NULL)
NSIM <- 100 # Number of simulations
ss <- PBrefdist(lg, sm, nsim=NSIM, details=DT)
```

```
## Reference distribution with   100 samples; computing time:  1.23 secs.
```

```r
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=1, details=DT)
```

```
## Reference distribution with   100 samples; computing time:  2.06 secs.
```

```r
NSIM <- 500 # Number of simulations
ss <- PBrefdist(lg, sm, nsim=NSIM, details=DT)
```

```
## Reference distribution with   500 samples; computing time:  5.29 secs.
```

```r
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=1, details=DT)
```

```
## Reference distribution with   500 samples; computing time:  9.31 secs.
```

```r
NSIM <- 1000 # Number of simulations
ss <- PBrefdist(lg, sm, nsim=NSIM, details=DT)
```

```
## Reference distribution with  1000 samples; computing time:  7.89 secs.
```

```r
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=1, details=DT)
```

```
## Reference distribution with  1000 samples; computing time: 19.83 secs.
```

```r
## NSIM <- 5000 # Number of simulations
## ss <- PBrefdist(lg, sm, nsim=NSIM, details=DT)
## ss <- PBrefdist(lg, sm, nsim=NSIM, cl=1, details=DT)
```

## Increasing number of clusters


```r
NSIM <- 1000 # Number of simulations

NCL <- 1 # Number of clusters
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=NCL, details=DT)
```

```
## Reference distribution with  1000 samples; computing time: 16.40 secs.
```

```r
NCL <- 2 # Number of clusters
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=NCL, details=DT)
```

```
## Reference distribution with  1000 samples; computing time: 16.13 secs.
```

```r
NCL <- 3 # Number of clusters
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=NCL, details=DT)
```

```
## Reference distribution with   999 samples; computing time:  9.75 secs.
```

```r
NCL <- 4 # Number of clusters
ss <- PBrefdist(lg, sm, nsim=NSIM, cl=NCL, details=DT)
```

```
## Reference distribution with  1000 samples; computing time: 11.30 secs.
```

