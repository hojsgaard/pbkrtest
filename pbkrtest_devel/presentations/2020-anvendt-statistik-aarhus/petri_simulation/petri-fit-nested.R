library(lme4)
library(pbkrtest)
library(parallel)
#' ## Analyze data

rm(list=ls())
load("petri-dat-nested.RData")
source("seqPB.R")
ls()

grp <- design$grp
subj <- design$subj

cp <- function(rr)
    c(sum(rr <= 0.01), sum(rr <= 0.05), sum(rr <= 0.10)) / length(rr)

#' Ignore clustering; do F-test
norm_fun01 <- function(i){
    lg <- lm(Yn[,i] ~ grp)
    sm <- update(lg, .~. - grp)
    as.data.frame(anova(lg, sm, test="F"))[2,6]
}

#' Ignore clustering; do X2-test
norm_fun02 <- function(i){
    lg <- lm(Yn[,i] ~ grp)
    sm <- update(lg, .~. - grp)
    as.data.frame(anova(lg, sm, test="Chisq"))[2,5]
}

#' Analyse average; do F-test
norm_fun05 <- function(i){
    a <- aggregate(Yn[,i] ~ subj + grp, FUN=mean)
    grp <- a$grp
    y <- a[,3]
    lg <- lm(y ~ grp)
    sm <- update(lg, .~. - grp)
    as.data.frame(anova(lg, sm, test="F"))[2,6]
}

#' Analyse average; do X2-test
norm_fun06 <- function(i){
    a <- aggregate(Yn[,i] ~ subj + grp, FUN=mean)
    grp <- a$grp
    y <- a[,3]
    lg <- lm(y ~ grp)
    sm <- update(lg, .~. - grp)
    as.data.frame(anova(lg, sm, test="Chisq"))[2,5]
}

#' Account for clustering in mixed model; do X2-test
norm_fun11 <- function(i){
    ##cat("i : ", i, "\n")
    lg <- suppressMessages(lmer(Yn[,i] ~ grp + (1|subj), REML=FALSE))
    sm <- suppressMessages(update(lg, .~. - grp))
    as.data.frame(anova(lg, sm))[2, "Pr(>Chisq)"]
}

#' Account for clustering in mixed model; do F-test
norm_fun12 <- function(i){
    lg <- suppressMessages(lmer(Yn[,i] ~ grp + (1|subj), REML=FALSE))
    sm <- suppressMessages(update(lg, .~. - grp))
    KRmodcomp(lg, sm)$test$p.value[1]
}

#' Account for clustering in mixed model; do PB-test
norm_fun13 <- function(i){
    if (i %% 10 == 0) cat("i : ", i, "\n")
    lg <- suppressMessages(lmer(Yn[,i] ~ grp + (1|subj), REML=FALSE))
    sm <- suppressMessages(update(lg, .~. - grp))
    PBmodcomp(lg, sm, nsim=PBSIM, cl=4)$test$p.value[2]
    ##seqPBmodcomp(lg, sm, nsim=PBSIM, cl=1)$test$p.value[2]
}

## N <- 20
## N <- 100
## v <- sapply(1:N, norm_fun13)

N <- ncol(Yn)
PBSIM <- 999

funlist=list(norm_fun01, norm_fun02,
             norm_fun05, norm_fun06,
             norm_fun11, norm_fun12, norm_fun13) 

nval <- lapply(funlist, function(fun) {sapply(1:N, fun)})

save(nval, file="nval.RData")


ntab <- lapply(nval, cp) %>% do.call(rbind, .)
colnames(ntab) <- c("0.01", "0.05", "0.10")

ntab <- as.data.frame(ntab)
rownames(ntab) <- c("lm+F", "lm+X2", "avg_lm+F", "avg_lm+X2",
                    "mixed+X2", "mixed+F", "mixed+PB")

ntab


## nc <- 48
## ll <- split(matrix(1:48, nr=nc),1:nc)
## l1 <- ll[[1]]
## system.time(mclapply(ll, function(l){ lapply(l, nfun4) }))
## system.time(sapply(1:50, nfun4))


#' ## Correlated binomials
#' 

binom_fun1 <- function(i){
    y <- Yb[,i]
    lg <- glm(cbind(y, M-y) ~ grp, family=binomial)
    sm <- update(lg, .~. - grp)
    as.data.frame(anova(lg, sm, test="Chisq"))[2, "Pr(>Chi)"]
}

binom_fun2 <- function(i){
    y <- Yb[,i]
    lg <- glmer(cbind(y, M-y) ~ grp + (1|subj), family=binomial)
    sm <- update(lg, .~. - grp)
    as.data.frame(anova(lg, sm))[2,8]
}

binom_fun4 <- function(i){
    y <- Yb[,i]
    lg <- glmer(cbind(y, M-y) ~ grp + (1|subj), family=binomial)    
    sm <- update(lg, .~. - grp)
    seqPBmodcomp(lg, sm, nsim=PBSIM, cl=1)$test$p.value[2]
}


funlist <- list(binom_fun1, binom_fun2, binom_fun4)
#N <- ncol(Yn)
N <- 20

val <- lapply(funlist, function(fun) {sapply(1:N, fun)})

btab <- lapply(val, cp) %>% do.call(rbind, .)
colnames(btab) <- c("0.01", "0.05", "0.10")




breflist <- list(bref1, bref2, bref4)
btab <- do.call(rbind, lapply(breflist, cp))

colnames(btab) <- c("0.01", "0.05", "0.10")
kable(btab)









pois_fun1 <- function(i){
    y <- Yp[,i]
    lg <- glm(y ~ grp, family=poisson)
    sm <- update(lg, .~. - grp)
    as.data.frame(anova(lg, sm, test="Chisq"))[2, "Pr(>Chi)"]
}

pois_fun2 <- function(i){
    y <- Yp[,i]
    lg <- glmer(y ~ grp + (1|subj), family=poisson)
    sm <- update(lg, .~. - grp)
    as.data.frame(anova(lg, sm))[2,8]
}

pois_fun4 <- function(i){
    y <- Yp[,i]
    lg <- glmer(y ~ grp + (1|subj), family=poisson)    
    sm <- update(lg, .~. - grp)
    seqPBmodcomp(lg, sm, nsim=PBSIM, cl=1)$test$p.value[2]
}

pref1 <- sapply(1:N, bfun1)
pref1 <- pref1[!is.na(pref1)]

pref2 <- sapply(1:N, bfun2)
pref2 <- pref2[!is.na(pref2)]

pref4 <- sapply(1:N, bfun4)
pref4 <- pref4[!is.na(pref4)]

preflist <- list(pref1, pref2, pref4)
ptab <- do.call(rbind, lapply(preflist, cp))

colnames(ptab) <- c("0.01", "0.05", "0.10")
kable(ptab)

save(nreflist, breflist, preflist, file="reflist.RData")






## #' Data to tartu-talk
## #' 
## options("digits"=4)
## dub <- data.frame(y1=Yn[,1], y2=Yb[,1], grp, subj)
## dput(dub)


## #' linear model; ignore correlation
## lg <- lm(y1 ~ grp, data=dub)
## sm <- update(lg, .~. - grp)
## anova(lg)

## lg %>% summary  %>% coef %>% as.data.frame -> cf
## u <- (lg %>% summary  %>% coef)[,3]
## cf$chisq <- 1-pchisq(u^2, df=1)
## cf

## #' linear model to each stratum; same as analyzing average
## dub$E <- with(dub, interaction(grp, subj))
## lg <- aov(y1 ~ grp + Error(E), data=dub)
## summary(lg)

## duba <- aggregate(y1~grp+subj, FUN=mean, data=dub)
## lm(y1~grp, data=duba) %>% summary %>% coef

## #' mixed model
## lg3 <- lmer(y1 ~ grp + (1|subj), data=dub, REML=F)
## coef(summary(lg3))
## sm3 <- update(lg3, .~. - grp)
## anova(lg3, sm3)


## coef(summary(lg))
## coef(summary(lg3))

## load_all("pbkrtest")
## KRmodcomp(lg3, sm3)
## PBmodcomp(lg3, sm3)
## seqPBmodcomp(lg3, sm3)


i <- 215
lg <- lmer(Yn[,i] ~ grp + (1|subj), REML=FALSE)
sm <- update(lg, .~. - grp)


source("seqPB.R")
seqPBmodcomp(lg, sm, nsim=PBSIM, cl=1)$test$p.value[2]


lg <- lmer(Yn[,i] ~ grp + (1|subj), REML=FALSE)
sm <- update(lg, .~. - grp)


simdata <- simulate(sm, 100)

m <- foo(lg, sm, simdata=simdata)

yyy <- simdata[,41]
sm2 <-

    refit(sm, newresp = yyy)
        lg2 <- refit(lg, newresp = yyy)

options("warn"=-1)
refit(sm, newresp = yyy, control=list(verbose=0))


a<-try(refit(lg, newresp = yyy))



foo <- function (lg, sm, nsim = 20, seed = NULL, simdata = simulate(sm, 
    nsim = nsim, seed = seed)) 
{
    val <- rep(0, ncol(simdata))
    for (j in 1:ncol(simdata)){
        yyy <- simdata[, j]
        sm2 <- refit(sm, newresp = yyy, control=list(verbose=0))
        lg2 <- refit(lg, newresp = yyy, control=list(verbose=0))
        val[j]  <- 2 * (logLik(lg2, REML = FALSE) - logLik(sm2, REML = FALSE))
    }
    val
}


