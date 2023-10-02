#' ## Simulate data from nested design
#'
#' ### e.g. data in connection with replicates on petri plate
#'
#' Used in talks on `pbkrtest`
#'
#' Date `r date()`
#'
#' 

library(lme4)
library(pbkrtest)
library(parallel)

## options("mc.cores"=50)

#' ## Set simulation
#' 

ngr <- 2 ## Number of groups
spg <- 3 ## Subjects per group
ops <- 3 ## Obs per subject
ns <- ngr * spg ## Number of subjects

trt <- factor(rep(1:ngr, each=spg*ops))
levels(trt) <- c("ctrl", "trt1")
trt

subj <- factor(rep(1:(ngr*spg), each=ops))
levels(subj) <- paste0("subj", levels(subj))
subj


makeW <- function(dim, rho){
    out <- matrix(rho, nrow=dim, ncol=dim)
    diag(out) <- 1
    out
}

#' ## Simulate
#' 

N <- 500   ## Number of simulated datasets
M <- 5     ## Trials in binomial experiment
rho <- .9  ## correlation

#' ### Correlated N_2
set.seed(1411)
V <- kronecker(diag(1, ns), makeW(ops, rho))
V
Yn <- t(MASS::mvrnorm(N, mu=rep(0, nrow(V)), Sigma=V)) # N cols
Yn <- round(Yn, 2)

#' ### Correlated binomials
set.seed(1411)
U <- rep(rnorm(ns, sd=2), each=ops)
p <- 1 / (1 + exp(U))
Y <- rbinom(N*length(p), M, p)
Yb <- matrix(Y, nc=N) # N cols

#' ### Correlated poissons
set.seed(1411)
U <- rep(rnorm(ns, sd=2), each=ops)
lambda <- exp(U)
Yp <- matrix(rpois(N * length(lambda), lambda), nc=N)

#' Save the lot
design <- data.frame(grp=trt, subj=subj)
save(design, Yn, Yb, Yp, M, file="petri-dat-nested.RData")



load("petri-dat-nested.RData")

dub <- cbind(y1=100*Yn[,1], y2=Yb[,2], design)
ii <-1:(nrow(dub)/2)
dub[ii,"y1"] <- dub[ii,"y1"] + 20
dub <- doBy::orderBy(~grp+subj+y1, data=dub)
dput(dub)
