sessionInfo()
data(Oats, package = "nlme")
library(lme4)
Oats.lmer <- lmer(log(yield) ~ Variety*factor(nitro) + (1|Block/Variety), data = Oats)
library(pbkrtest)
vcovAdj(Oats.lmer)



library(pbkrtest)
library(dplyr)
library(ggplot2)


A <- B <- matrix(rnorm(n^2), nr=n)
Rprof()
for (i in 1:3) A%*%B
Rprof(NULL)
summaryRprof()

n <- 10000
m <- 3000
X <- matrix(rnorm(n*m), nr=m)
X <- t(X)%*%X

m <- 3000
X <- rWishart(1, 2*m, diag(1,m))[,,1]

Rprof()
for (i in 1:10) chol2inv(chol(X))
Rprof(NULL)
summaryRprof()









data(milkman, package="doBy")
milkman <- tbl_df(milkman)
cowvec <- unique(milkman$cowno)
milkman <- subset(milkman, cowno %in% cowvec[1:8])
milkman$cowno <- factor(milkman$cowno)
fm1 <- lmer(my~dfc+log(dfc)+(1|cowno), data=milkman, na.action=na.exclude)

v1 <- vcov(fm1)
v2 <- vcovAdj(fm1)


Rprof()
for (i in 1:1) vcovAdj(fm1)
Rprof(NULL)
summaryRprof()


load_all("pbkrtest"); .vcovAdj15(fm1)
load_all("pbkrtest"); .vcovAdj16(fm1)

load_all("pbkrtest");
microbenchmark::microbenchmark(vcovAdj(fm1), vcovAdj15(fm1), vcovAdj16(fm1), times=5)


fm1 <- lmer(breaks ~ -1 + tension + (1|wool), data=warpbreaks)

fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)

if (getME(fm1, "is_REML")){
    fm.ml <- update( fm1, REML=FALSE )
} else {
    fm.ml <- fm1
}

nsim <- 100
sim <- simulate(fm.ml, nsim)
B <- lapply(sim, function(newy) fixef(refit(fm.ml, newresp=newy)))
B <- do.call(rbind, B)
v3 <- cov.wt(B)$cov

v2/v1
v3/v1

data(beets, package="pbkrtest")
head(beets)

## Linear mixed effects model:
sug   <- lmer(sugpct ~ block + sow + harvest + (1|block:harvest), data=beets, REML=FALSE)
sug.h <- update(sug, .~. -harvest)
sug.s <- update(sug, .~. -sow)


load_all("pbkrtest");
anova(sug, sug.h)
PBmodcomp(sug, sug.h, nsim=500)
anova(sug, sug.h)
PBmodcomp(sug, sug.s, nsim=500)


ref <- PBrefdist(sug, sug.h, 10000)

ref2 <- PBrefdist(sug, sug.h, 10000, cl=cl)



sourceCpp("fun-pbkr.cpp")
Sigma <- stat$SigmaG$Sigma
M <- stat$M
microbenchmark::microbenchmark(#solve(Sigma),
                               #solve(Sigma, M),
                               #solve(Sigma) %*% M,
                               #chol2inv(chol(Sigma)),
                               chol2inv(chol(Sigma)) %*% M,
    #spSolve1(Sigma, M),
                               spSolve2(Sigma, M),
    spSolve3(Sigma, M),
    #spSolve4(Sigma, M),
                               times=4)



SigmaG <- stat$SigmaG
X <- stat$X

Sigma <- stat$SigmaG$Sigma

II <- diag(1, nrow(Sigma))

AA <- II-Sigma

Sinv <- II+AA

P <- Sinv%*%Sigma







stat15$OO
stat16$OO
stat16$OOt

stat15$QQ
stat16$QQ



n <- 500
p <- 4
A <- B <- matrix(rnorm(n^2), nr=n)
X <- matrix(rnorm(n*p), nr=n)

microbenchmark::microbenchmark(A %*% B %*% X, A %*% (B %*% X), times=5)




#' data(dietox, package="doBy")
#' dietox <- tbl_df(dietox)
#' fm1 <- lmer( Weight ~ Time + Evit + Cu + (1|Pig), data=dietox)
#' vcov(fm1)


object <- fm1
SigmaG   <- get_SigmaG( object, 0)
X        <- getME(object,"X")
mat <- list(SigmaG=SigmaG, SigmaInv=solve(SigmaG$Sigma), X=X)
save(mat, file="mat.RData")

load("mat.RData")

Sigma <- mat$SigmaG$Sigma
n.ggamma <- mat$SigmaG$n.ggamma

foo <- function(){

M <- cbind(do.call(cbind, mat$SigmaG$G), mat$X)
##SinvM <- solve(Sigma, M)
    SinvM <- chol2inv(chol(SigmaG$Sigma)) %*% M
v   <- c(rep(1:length(mat$SigmaG$G), each=nrow(SinvM)), rep(length(mat$SigmaG$G)+1,
                                                            ncol(mat$X)))
idx <- lapply(unique(v), function(i) which(v==i))

# list of SinvG1, SinvG2,... SinvGr, SinvX
# Næsten H
SinvG <- lapply(idx, function(z) SinvM[,z])
SinvX <- SinvG[[length(SinvG)]] ## Kaldes TT andre steder
SinvG[length(SinvG)] <- NULL

}

Rprof()
for (i in 1:10) foo()
Rprof(NULL)
summaryRprof()




X1 <- solve(Sigma, M)
X2 <- chol2inv(chol(Sigma)) %*% M


OO <- lapply(1:n.ggamma, function(i) {
    #' G <- mat$SigmaG$G[[i]];
    #' G %*% SinvX
    mat$SigmaG$G[[i]] %*% SinvX
})

PP  <- QQ <- NULL
PP2 <- vector("list", n.ggamma)
QQ2 <- vector("list", n.ggamma * (n.ggamma + 1) / 2 )
index <- 1
for (r in 1:n.ggamma) {
    OOt <- t( OO[[ r ]] )
    PP  <- c(PP, list(forceSymmetric( -1 * OOt %*%  SinvX)))
    PP2[[r]] <- -1 * forceSymmetric( OOt %*%  SinvX) 
    for (s in r:n.ggamma) {
        index <- index + 1;
        QQ <- c(QQ, list( OOt %*% SinvG[[s]] ))
        QQ2[[index]] <- OOt %*% SinvG[[s]]
    }}


Ktrace <- matrix( NA, nrow=n.ggamma, ncol=n.ggamma )
for (r in 1:n.ggamma) {
    ##HrTrans <- t( HH[[r]] )
    ##HrTrans <- SinvG[[r]]
    for (s in r:n.ggamma){
        ##Ktrace[r,s] <- Ktrace[s,r] <- sum( HrTrans * t(SinvG[[s]]) )
        ##Ktrace[r,s] <- Ktrace[s,r] <- sum( SinvG[[r]] * t(SinvG[[s]]) )
        Ktrace[r,s] <- Ktrace[s,r] <- sum( SinvG[[r]] * SinvG[[s]] )
    }}

getTHO <- function(mat){
    #SigmaInv <- mat$SigmaInv
    Sigma <- mat$SigmaG$Sigma
    SigmaInv <- solve(Sigma)
    TT <- SigmaInv %*% mat$X
    n.ggamma <- mat$SigmaG$n.ggamma
    HH       <- OO <- vector("list", n.ggamma)     
    for (ii in 1:n.ggamma) {
        .tmp <- mat$SigmaG$G[[ii]] %*% SigmaInv
        HH[[ ii ]] <- .tmp
        OO[[ ii ]] <- .tmp %*% mat$X
    }
    PP <- QQ <- NULL
    for (rr in 1:n.ggamma) {
        OrTrans <- t( OO[[ rr ]] )
        PP <- c(PP, list(forceSymmetric( -1 * OrTrans %*%  TT)))
        for (ss in rr:n.ggamma) {
            QQ <- c(QQ,list(OrTrans %*% SigmaInv %*% OO[[ss]] ))
        }
    }    
    list(TT=TT, HH=HH, OO=OO, PP=PP, QQ=QQ)
}

getTHO2 <- function(mat){
    Sigma <- mat$SigmaG$Sigma
    n.ggamma <- mat$SigmaG$n.ggamma
    
    M <- cbind(do.call(cbind, mat$SigmaG$G), mat$X)
    ##tmp <<- list(Sigma=Sigma, M=M, n.ggamma=n.ggamma)
    SinvM <- solve(Sigma, M)
    
    v   <- c(rep(1:length(mat$SigmaG$G), each=nrow(SinvM)), rep(length(mat$SigmaG$G)+1, ncol(mat$X)))
    idx <- lapply(unique.default(v), function(i) which(v==i))
    
                                        # list of SinvG1, SinvG2,... SinvGr, SinvX
                                        # Næsten H
    SinvG <- lapply(idx, function(z) SinvM[,z])
    SinvX <- SinvG[[length(SinvG)]] ## Kaldes TT andre steder
    SinvG[length(SinvG)] <- NULL

    
    OO <- lapply(1:n.ggamma, function(i) {
        #' G <- mat$SigmaG$G[[i]];
        #' G %*% SinvX
        mat$SigmaG$G[[i]] %*% SinvX
    })

    #PP  <- QQ <- NULL
    PP2 <- vector("list", n.ggamma)
    QQ2 <- vector("list", n.ggamma * (n.ggamma + 1) / 2 )
    index <- 1
    for (r in 1:n.ggamma) {
        OOt <- t( OO[[ r ]] )
        #PP  <- c(PP, list(forceSymmetric( -1 * OOt %*%  SinvX)))
        PP2[[r]] <- -1 * forceSymmetric( OOt %*%  SinvX) 
        for (s in r:n.ggamma) {
            index <- index + 1;
            #QQ <- c(QQ, list( OOt %*% SinvG[[s]] ))
            QQ2[[index]] <- OOt %*% SinvG[[s]]
        }
    }
    list(OO=OO, PP=PP2, QQ=QQ2)
}

MM <- tmp$M
SS <- tmp$Sigma

SiM <- solve(SS, MM)
SSd <- as.matrix(SS)
MMd <- as.matrix(MM)

microbenchmark::microbenchmark( solve.default(SSd,MMd), solve(SS,MM), times=3) 

aa1 <- getTHO(mat)
aa1 <- getTHO2(mat)


Rprof()
for (i in 1:3) getTHO2(mat)
Rprof(NULL)
summaryRprof()

sourceCpp("fun-pbkr.cpp");

r1 <- solve(SS, MM)
r2 <- spSolve(SS,MM)
r3 <- spSolve2(SS,MM)

microbenchmark::microbenchmark(
    solve(SS, MM), spSolve(SS, MM), spSolve2(SS, MM), times=3 )

out <- pbkr(mat)


microbenchmark::microbenchmark( getTHO(mat), getTHO2(mat), pbkr(mat), times=3)

Sigma <- mat$SigmaG$Sigma
SigmaInv <- solve(Sigma)

sourceCpp("pbkr.cpp");
microbenchmark::microbenchmark( sparseInverse(Sigma), solve(Sigma), times=4)


A <- matrix(c(1,2,3,4,4,5,3,2,1),nr=3)

S <- A
S[upper.tri(S)]<- 0
R <- A-S
Si <- solve(S)
#' R  <- A-S
#' S  <- diag(diag(A))
SiR <- Si%*%R

X <- diag(1, nrow(A))

X2 <- SiR %*% X + Si
print(max(abs(X2%*%A)))
X <- X2


A <- mat$SigmaG$Sigma

m <- max(rowSums(A%*%t(A)))
a <- 2/m
I <- diag(1,nrow(A))
X <- a*t(A)
for(i in 1:100){
    X2 <- X + X%*%(I-A%*%X)
    print(max(abs(X2%*%A)))
    X <- X2
}



A <- Sigma
Ai <- solve(Sigma)

I <- diag(1,nrow(Sigma))
X <- diag(1/diag(Sigma))

X2 <- X + 0.1*X%*%(I-A%*%X)
max(abs(Ai-X2))
X <- X2

X2 <- X %*% (II - A %*% X)

library(Matrix)
A <- as(diag(10, 10), "dgCMatrix")

sourceCpp("arma.cpp")
spinv(A)
sparseInverse(A)


load("mat.RData")
s1 <- solve(mat$SigmaG$Sigma)
s2 <- sparseInverse(mat$SigmaG$Sigma)

s2 <- spinv(mat$SigmaG$Sigma)

microbenchmark::microbenchmark(solve(mat$SigmaG$Sigma), sparseInverse(mat$SigmaG$Sigma), times=3)


load_all("pbkrtest")
vcov(fm1)
vcovAdj15(fm1)

Rprof()
for (i in 1:5) vcovAdj(fm1)
Rprof(NULL)
summaryRprof()


load_all("pbkrtest")
lapply(mat, class)

class(mat$SigmaG$Sigma) ## dgCMatrix
class(mat$X) ## matrix
class(mat$SigmaG$G[[1]]) ## dgCmatrix

## Brug for
X <- mat$X
Sigma <- mat$SigmaG$Sigma
SigmaInv <- solve(Sigma)

Gii <- mat$SigmaG$G[[1]]
ZZ <- Gii %*% SigmaInv

siginv <- 1/diag(Sigma)[1]
ZZ2<-Gii / siginv
max(abs(ZZ-ZZ2))









































library(doBy)

load_all("doBy")

require(lme4)
class(warp.mm)


LSmeans(warp.mm, effect="tension")
 class(warp.mm)
 fixef(warp.mm)
 coef(summary(warp.mm))
 vcov(warp.mm)
 #if (requireNamespace("pbkrtest", quietly=TRUE ))
 #vcovAdj(warp.mm)





library(lmerTest)

library(lme4)
options("digits"=5)
data("beets", package = "pbkrtest")
sug  <- lmer(sugpct ~ block + sow + harvest + (1 | block:harvest), data = beets, REML = FALSE)
sugh <- lmer(sugpct ~ block + sow + (1 | block:harvest), data = beets, REML = FALSE)
sugs <- lmer(sugpct ~ block + harvest + (1 | block:harvest), data = beets, REML = FALSE)

anova(sug)

library(lmerTest)
anova(sug)
pbkrtest::KRmodcomp.lmerMod(sug, sugh)
pbkrtest::KRmodcomp.lmerMod(sug, sugs)

a<-pbkrtest::PBmodcomp.merMod(sug, sugh)











dat <- read.csv("LongDat1.txt", header=T)


fit1 <- lmer(zz ~ (X1i*obstime)+(X2i*obstime) + (1 + obstime | id),
             data=dat, weights = wz)
fit2 <- lmer(zz ~ (1 + obstime | id),
             data=dat, weights = wz)

KRmodcomp(fit1, fit2)

anova(fit1, fit2)

PBmodcomp(fit1, fit2)



fmLarge <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)

V0 <- vcov(fmLarge)
VA <- vcovAdj(fmLarge)
d  <- V0-VA
zapsmall(d)


options("digits"=5)
data("beets", package = "pbkrtest")
sug <- lmer(sugpct ~ block + sow + harvest + (1 | block:harvest), data = beets, REML = FALSE)
V0 <- vcov(sug)
VA <- vcovAdj(sug)
d  <- V0-VA
1000*zapsmall(d)

data(Orthodont, package="nlme")
m <- lmer(distance~Sex+age+(age|Subject), data=Orthodont)
V0 <- vcov(m)
VA <- vcovAdj(m)
d  <- V0-VA
zapsmall(d)










library(nlme)
mses <- with(MathAchieve, tapply(SES, School, mean))
mses[as.character(MathAchSchool$School[1:10])]  # for first 10 schools
Bryk <- as.data.frame(MathAchieve[, c("School", "SES", "MathAch")])
names(Bryk) <- tolower(names(Bryk))
sector <- MathAchSchool$Sector
names(sector) <- row.names(MathAchSchool)
Bryk <- within(Bryk,{
    meanses <- as.vector(mses[as.character(school)])
    cses <- ses - meanses
    sector <- sector[as.character(school)]
})

library(lme4)
bryk.lmer.1 <- lmer(mathach ~ meanses*cses + sector*cses + (cses | school),
    data=Bryk)
summary(bryk.lmer.1)



library(pbkrtest)

load_all("pbkrtest")
vcovAdj(bryk.lmer.1, details=10)

object <- bryk.lmer.1
details=0

SigmaG <- get_SigmaG( object, details=0)

Sig <- forceSymmetric(SigmaG$Sigma)
dim(Sig)
load_all("pbkrtest")
f<-foo(Sig)

Sig.chol <- Matrix:::chol( Sig )
class(Sig.chol)
SigmaInv <- chol2inv( Sig.chol )
class(SigmaInv)

dim(SigmaInv)



a<-forceSymmetric(SigmaG$Sigma)
a2<-chol(a)
a3<-chol2inv(a2)


vcovAdj(bryk.lmer.1, details=10)




Phi      <- vcov(object)
SigmaG   <- get_SigmaG( object, details=10)
X        <- getME(object,"X")



bryk.lmer.2 <- lmer(mathach ~ meanses*cses + sector*cses + (1 | school),
    data=Bryk)



anova(bryk.lmer.1, bryk.lmer.2, refit=FALSE)

system.time(print(vcovAdj(bryk.lmer.2, details=10)))











library(pbkrtest)
data("beets", package = "pbkrtest")
sug <- lm(sugpct ~ block + sow + harvest, data=beets)
sug.h <- update(sug, .~. - harvest)
sug.s <- update(sug, .~. - sow)


lg<-sug
sm<-sug.h
nsim<-20
seed=NULL

.lmRefDist <- function(lg, sm, nsim=20, seed=NULL){
    simdata <- simulate(sm, nsim, seed=seed)
    ee  <- new.env()
    ee$simdata <- simdata

    ff.lg <- update.formula(formula(lg),simdata[,ii]~.)
    ff.sm <- update.formula(formula(sm),simdata[,ii]~.)
    environment(ff.lg) <- environment(ff.sm) <- ee

    cl.lg <- getCall(lg)
    cl.sm <- getCall(sm)

    cl.lg$formula <- ff.lg
    cl.sm$formula <- ff.sm

    ref <- rep.int(NA, nsim)
    for (ii in 1:nsim){
        ref[ii] <- 2*(logLik(eval(cl.lg))-logLik(eval(cl.sm)))
    }
    ref
}

.getP <- function(lg, sm, refDist){
  stat <- 2*(logLik(lg)-logLik(sm))
  (1+sum(refDist>=stat))/(1+length(refDist))
}

ref <- .lmRefDist(sug, sug.h, nsim=1000)
.getP(sug, sug.h, ref)

ref <- .lmRefDist(sug, sug.s, nsim=1000)
.getP(sug, sug.s, ref)

qqplot(r.h,rchisq(1000,df=1)); abline(0,1)

m1 <- lm(dist~speed+I(speed^2), data=cars)
m2 <- lm(dist~speed, data=cars)
r <- .lmRefDist(m1, m2, nsim=1000)
.getP(m1, m2, r)

qqplot(r,rchisq(1000,df=1)); abline(0,1)
stat <- 2*(logLik(m1)-logLik(m2))
sum(r>=stat)/length(r)

counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
print(d.AD <- data.frame(treatment, outcome, counts))
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
anova(glm.D93)
summary(glm.D93)

glm.D93.2 <- update(glm.D93, .~. -outcome)
glm.D93.3 <- update(glm.D93, .~. -treatment)
anova(glm.D93, glm.D93.2, test="Chisq")
anova(glm.D93, glm.D93.3, test="Chisq")

PBmodcomp(glm.D93, glm.D93.2)
PBmodcomp(glm.D93, glm.D93.3)




(gm1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
              data = cbpp, family = binomial))
(gm2 <- update(gm1, .~.-period))
anova(gm1, gm2)
PBmodcomp(gm1, gm2)




glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
              data = cbpp, family = binomial))








PBmodcomp( sug, sug.h )
PBmodcomp( sug, sug.s )

load_all("pbkrtest")
m.h <- model2restrictionMatrix(sug, sug.h); m.h
m.s <- model2restrictionMatrix(sug, sug.s); m.s
## should we do zapsmall? Think so!

f.h <- restrictionMatrix2model(sug, m.h); f.h
f.s <- restrictionMatrix2model(sug, m.s); f.s

plot(fitted(f.h), fitted(sug.h))
plot(fitted(f.s), fitted(sug.s))

nullBasis <- function (X) {
    S <- svd( X )
    null.basis <- S$v[, S$d < 1e-06, drop = FALSE]
    null.basis
}

load_all("pbkrtest")
PBrefdist( sug, sug.h )

load_all("pbkrtest")
.lm_ref(sug, sug.h, 10)

load_all("pbkrtest")
debug(".lm_ref")
.lm_ref(sug, sug.h, 10)

fun(sug, sug.h, 10)
simulate(sug.h, 10)


#	cl.l <- lg
#	cl.s <- sm
#	cl.l$formula <- update.formula(formula(lg),simdata[,ii]~.)
#	cl.s$formula <- update.formula(formula(sm),simdata[,ii]~.)


    ref <- rep.int(NA,nsim)
    for (ii in 1:nsim){
   	    cl.l <- update(cl.l, ff.l)
		cl.s <- update(cl.s, ff.s)
        ref[ii] <- 2*(logLik(cl.l)-logLik(cl.s))
    }
	ref

















debug(PBmodcomp)
PBmodcomp( sug, sug.h )
undebug(PBmodcomp)


PBmodcomp( sug, sug.s )

PM <- function(X){X%*%solve(t(X)%*%X)%*%t(X)}

B <- model.matrix(sug)
A <- model.matrix(sug.h)

D  <- cbind( A, B )
d  <- rankMatrix( D ) - rankMatrix( B )
rA <- rankMatrix(A)
rB <- rankMatrix(B)
Q  <- qr.Q(qr( D ))
Q2 <- Q[,(rA+1):rB, drop=FALSE]
L2 <- t(Q2)  %*% B
L2 <- zapsmall( L2 )
L  <- t( qr.Q(qr( t( L2 ) )) )
L  <- zapsmall(L)

d <- rankMatrix(B)-rankMatrix(A)
PA  <- PM(A)
PB  <- PM(B)
IPA <- (diag(1, nrow(PA)) - PA)
#IPA <- (PB - PA)
QQ  <- qr(IPA)
QQ2 <- qr.Q(QQ)[,(rA+1):rB, drop=F]

dp <- Q2%*%t(Q2) - QQ2%*%t(QQ2)
max(abs(dp))

L3 <- t(QQ2) %*% B
L3 <- zapsmall(L3)

LL <- IPA %*% B
LL <- zapsmall(LL)
S <- svd(LL)
ii <- abs(S$d) > 1e-12
bn <- S$v[ii, ,drop=FALSE]
bn <- zapsmall(bn)

svd(IPA)



