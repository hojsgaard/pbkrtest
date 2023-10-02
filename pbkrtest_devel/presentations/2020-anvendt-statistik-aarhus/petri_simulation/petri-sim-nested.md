## Simulate data from nested design

### e.g. data in connection with replicates on petri plate

Used in talks on `pbkrtest`

Date Fri Dec 27 22:35:51 2019




```r
library(lme4)
library(pbkrtest)
library(parallel)

## options("mc.cores"=50)
```

## Set simulation



```r
ngr <- 2 ## Number of groups
spg <- 3 ## Subjects per group
ops <- 3 ## Obs per subject
ns <- ngr * spg ## Number of subjects

trt <- factor(rep(1:ngr, each=spg*ops))
levels(trt) <- c("ctrl", "trt1")
trt
```

```
##  [1] ctrl ctrl ctrl ctrl ctrl ctrl ctrl ctrl ctrl trt1 trt1 trt1 trt1 trt1 trt1
## [16] trt1 trt1 trt1
## Levels: ctrl trt1
```

```r
subj <- factor(rep(1:(ngr*spg), each=ops))
levels(subj) <- paste0("subj", levels(subj))
subj
```

```
##  [1] subj1 subj1 subj1 subj2 subj2 subj2 subj3 subj3 subj3 subj4 subj4 subj4
## [13] subj5 subj5 subj5 subj6 subj6 subj6
## Levels: subj1 subj2 subj3 subj4 subj5 subj6
```

```r
makeW <- function(dim, rho){
    out <- matrix(rho, nrow=dim, ncol=dim)
    diag(out) <- 1
    out
}
```

## Simulate



```r
N <- 500   ## Number of simulated datasets
M <- 5     ## Trials in binomial experiment
rho <- .9  ## correlation
```

### Correlated N_2


```r
set.seed(1411)
V <- kronecker(diag(1, ns), makeW(ops, rho))
V
```

```
##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
##  [1,]  1.0  0.9  0.9  0.0  0.0  0.0  0.0  0.0  0.0   0.0   0.0   0.0   0.0
##  [2,]  0.9  1.0  0.9  0.0  0.0  0.0  0.0  0.0  0.0   0.0   0.0   0.0   0.0
##  [3,]  0.9  0.9  1.0  0.0  0.0  0.0  0.0  0.0  0.0   0.0   0.0   0.0   0.0
##  [4,]  0.0  0.0  0.0  1.0  0.9  0.9  0.0  0.0  0.0   0.0   0.0   0.0   0.0
##  [5,]  0.0  0.0  0.0  0.9  1.0  0.9  0.0  0.0  0.0   0.0   0.0   0.0   0.0
##  [6,]  0.0  0.0  0.0  0.9  0.9  1.0  0.0  0.0  0.0   0.0   0.0   0.0   0.0
##  [7,]  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.9  0.9   0.0   0.0   0.0   0.0
##  [8,]  0.0  0.0  0.0  0.0  0.0  0.0  0.9  1.0  0.9   0.0   0.0   0.0   0.0
##  [9,]  0.0  0.0  0.0  0.0  0.0  0.0  0.9  0.9  1.0   0.0   0.0   0.0   0.0
## [10,]  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0   1.0   0.9   0.9   0.0
## [11,]  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0   0.9   1.0   0.9   0.0
## [12,]  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0   0.9   0.9   1.0   0.0
## [13,]  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0   0.0   0.0   0.0   1.0
## [14,]  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0   0.0   0.0   0.0   0.9
## [15,]  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0   0.0   0.0   0.0   0.9
## [16,]  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0   0.0   0.0   0.0   0.0
## [17,]  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0   0.0   0.0   0.0   0.0
## [18,]  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0   0.0   0.0   0.0   0.0
##       [,14] [,15] [,16] [,17] [,18]
##  [1,]   0.0   0.0   0.0   0.0   0.0
##  [2,]   0.0   0.0   0.0   0.0   0.0
##  [3,]   0.0   0.0   0.0   0.0   0.0
##  [4,]   0.0   0.0   0.0   0.0   0.0
##  [5,]   0.0   0.0   0.0   0.0   0.0
##  [6,]   0.0   0.0   0.0   0.0   0.0
##  [7,]   0.0   0.0   0.0   0.0   0.0
##  [8,]   0.0   0.0   0.0   0.0   0.0
##  [9,]   0.0   0.0   0.0   0.0   0.0
## [10,]   0.0   0.0   0.0   0.0   0.0
## [11,]   0.0   0.0   0.0   0.0   0.0
## [12,]   0.0   0.0   0.0   0.0   0.0
## [13,]   0.9   0.9   0.0   0.0   0.0
## [14,]   1.0   0.9   0.0   0.0   0.0
## [15,]   0.9   1.0   0.0   0.0   0.0
## [16,]   0.0   0.0   1.0   0.9   0.9
## [17,]   0.0   0.0   0.9   1.0   0.9
## [18,]   0.0   0.0   0.9   0.9   1.0
```

```r
Yn <- t(MASS::mvrnorm(N, mu=rep(0, nrow(V)), Sigma=V)) # N cols
Yn <- round(Yn, 2)
```

### Correlated binomials


```r
set.seed(1411)
U <- rep(rnorm(ns, sd=2), each=ops)
p <- 1 / (1 + exp(U))
Y <- rbinom(N*length(p), M, p)
Yb <- matrix(Y, nc=N) # N cols
```

### Correlated poissons


```r
set.seed(1411)
U <- rep(rnorm(ns, sd=2), each=ops)
lambda <- exp(U)
Yp <- matrix(rpois(N * length(lambda), lambda), nc=N)
```

Save the lot


```r
design <- data.frame(grp=trt, subj=subj)
save(design, Yn, Yb, Yp, M, file="petri-dat-nested.RData")
```

