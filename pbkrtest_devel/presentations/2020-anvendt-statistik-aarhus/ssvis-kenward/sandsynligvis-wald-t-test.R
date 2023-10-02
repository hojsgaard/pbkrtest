library(tidyverse)

ngr <- 2 ## Number of groups
spg <- 3 ## Subjects per group
ops <- 1 ## Obs per subject
ns <- ngr * spg ## Number of subjects
nspg <- spg * ops ## Number of subjects per group

trt <- factor(rep(1:ngr, each=nspg))
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

N <- 10000
M <- 5
rho <- .9

#' ### Correlated N_2
set.seed(1411)
V <- kronecker(diag(1, ns), makeW(ops, rho))
V
Yn <- t(MASS::mvrnorm(N, mu=rep(0, nrow(V)), Sigma=V)) # N cols

design <- data.frame(grp=trt, subj=subj)

do_test <- function(i){
    y1 <- Yn[,i]
    lg1 <- lm(y1 ~ grp, data=design)
    lg1 %>% summary %>% coef %>% as.data.frame -> tb1
    tb1$"Pr(>|X^2|)" = 1 - pchisq(tb1[,3]^2, df=1)
    as.numeric(tb1[2,4:5])
}

out <- sapply(1:N, do_test)

sum(out[1,] <= 0.01) / ncol(out)

r1 <- sapply(c(0.10, 0.05, 0.01), function(sl) out[1,] <= sl) %>% apply(2, sum)
r1/ncol(out) 

r2 <- sapply(c(0.10, 0.05, 0.01), function(sl) out[2,] <= sl) %>% apply(2, sum)
r2/ncol(out) 


