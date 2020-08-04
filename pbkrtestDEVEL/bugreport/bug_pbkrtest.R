
install.packages("pbkrtest")

require(devtools)
require(pbkrtest)
packageVersion("pbkrtest")

load_all("pbkrtest")
packageVersion("pbkrtest")

require(lme4)
load("fhch2010.rda")
fhch <- droplevels(fhch2010[ fhch2010$correct,])
fhch <- subset(fhch, id %in% unique(fhch$id)[c(1:5, 21:25)])
fhch$id <- factor(fhch$id)
##afex::set_sum_contrasts()
m1 <- lmer(log_rt ~ stimulus*density*frequency + 
              (density*frequency|id) + 
              (task|item), fhch, 
            control = lmerControl(optCtrl = list(maxfun = 1e6)))
m2 <- lmer(log_rt ~ (stimulus+density+frequency)^2 + 
              (density*frequency|id) + 
              (task|item), fhch, 
            control = lmerControl(optCtrl = list(maxfun = 1e6)))

devtools::load_all("pbkrtest")
KRmodcomp(m1, m2)

PBmodcomp(m1, m2)




load_all("pbkrtest")

load("SigmaG.RData")
library(parallel)

S <- SigmaG$Sigma
G <- SigmaG$G[1:5]
G1<- SigmaG[[1]]

microbenchmark::microbenchmark(
                    S %*% G1, crossprod(S, G1), times=4)

system.time( mclapply(G, function(g) g %*% S) )

m <- matrix(1:4, nr=2)
S <- m
G <- list(m, m, m)

library(foreach)
library(doParallel)

system.time(w <- for (i in 1:length(G)){G[[i]] %*% S})

system.time(w <- foreach(i=(1:length(G))) %do% {G[[i]] %*% S} )

system.time(v <- foo(G, S))




registerDoParallel()
foreach(i=(1:length(G))) %dopar% (G[[i]] %*% S)











foreach(i=1:10000) %do% sum(tanh(1:i))

















m <- as(SigmaG$Sigma, "matrix")
microbenchmark::microbenchmark(
                    chol2inv( chol( forceSymmetric(m))),
                    chol2inv( chol( m )),
                    times=2)

as(m, "dgCMatrix")



## Note: method with signature ‘sparseMatrix#ANY’ chosen for function ‘kronecker’,
# target signature ‘dgCMatrix#ngCMatrix’.
# "ANY#sparseMatrix" would also be valid
# Error in chol2inv(chol(forceSymmetric(SigmaG$Sigma))) %*% M : 
#   Cholmod error 'problem too large' at file ../Core/cholmod_sparse.c, line 92


## restart R:
devtools::install_url("https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-4.tar.gz")

require(lme4)
require(pbkrtest)
load("fhch2010.rda")
fhch <- droplevels(fhch2010[ fhch2010$correct,])

afex::set_sum_contrasts()
m1 <- lmer(log_rt ~ stimulus*density*frequency + 
             (density*frequency|id) + 
             (task|item), fhch, 
           control = lmerControl(optCtrl = list(maxfun = 1e6)))
m2 <- lmer(log_rt ~ (stimulus+density+frequency)^2 + 
             (density*frequency|id) + 
             (task|item), fhch, 
           control = lmerControl(optCtrl = list(maxfun = 1e6)))

KRmodcomp(m1, m2) # may require up to 64 GB of RAM
# Note: method with signature ‘sparseMatrix#ANY’ chosen for function ‘kronecker’,
# target signature ‘dgCMatrix#ngCMatrix’.
# "ANY#sparseMatrix" would also be valid
# F-test with Kenward-Roger approximation; computing time: 5636.57 sec.
# large : log_rt ~ stimulus * density * frequency + (density * frequency | 
#                                                      id) + (task | item)
# small : log_rt ~ (stimulus + density + frequency)^2 + (density * frequency | 
#                                                          id) + (task | item)
# stat      ndf      ddf F.scaling p.value
# Ftest   2.0918   1.0000 581.4186         1  0.1486

