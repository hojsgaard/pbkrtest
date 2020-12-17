load_all("pkg")

document("pkg")




library(pbkrtest)
library(ggplot2)

ggplot(sleepstudy) + geom_line(aes(Days, Reaction, group=Subject, color=Subject))

fm0 <- lmer(Reaction ~ Days + (Days|Subject), data=sleepstudy)
fm1 <- update(fm0, .~. - Days)

p0 <- anova(fm0, fm1)
p1 <- PBmodcomp(fm0, fm1)
p2 <- KRmodcomp(fm0, fm1)
p3 <- SATmodcomp(fm0, fm1)

p0
p1
p2
p3

tidy(p0)
tidy(p1)
tidy(p2)
tidy(p3)



























prmatrix((mat<-p2$formula.small), collab = rep_len("", ncol(mat)), rowlab = rep_len("", ncol(mat)))

print.default(p1)


print.default(p2)




set.seed(1213)
data(sleepstudy)
SIZE <- 50
sleepstudy <- sleepstudy[sample(nrow(sleepstudy), size=SIZE), ]








Parametric Bootstrap and Kenward Roger Based Methods for Mixed Model Comparison




devfun <- lmer(Reaction ~ Days + I(Days^2) + (Days|Subject), data=sleepstudy, devFunOnly=T)

theta <- getME(fm0, "theta")
sigma <- getME(fm0, "sigma")

devfun(c(theta))


get_devfun(fm0)





L <- rbind(c(0, 1, 0), 
           c(0, 0, 1))

microbenchmark::microbenchmark(
                    KRmodcomp(fm0, L), SATmodcomp(fm0, L))







fit2 <- update(fit1, .~. - time)
    L <- model2remat(fit1, fit2)



source("sh_satter.R");source("sh_contest.R")
get_sddf(fm, L)



data(beets)



(fm <- lmer2(Reaction ~ Days + + I(Days^2) + (Days|Subject), sleepstudy))

# Define 2-df contrast - since L has 2 (linearly independent) rows
# the F-test is on 2 (numerator) df:
L <- rbind(c(0, 1, 0), 
           c(0, 0, 1))

# Make the 2-df F-test of any effect of Days:1

## KRmodcomp(fm, L)
contestMD(fm, L, ddf="Satterthwaite")
contestMD(fm, L, ddf="Kenward-Roger")

fit1@Jac_list


model  <- fm0


## Fra lmer2
mc <- model@call
model <- eval.parent(mc)
## if(devFunOnly) return(model)
## Make an lmerModLmerTest object:
args <- as.list(mc)
args$devFunOnly <- TRUE

Call <- as.call(c(list(quote(lme4::lmer)), args[-1]))
devfun <- eval.parent(Call)

out <- list()
## Fra as_lmerModLT

tol <- 1e-6
is_reml <- getME(model, "is_REML")
## Coerce 'lme4-model' to 'lmerModLmerTest':
##res <- as(model, "lmerModLmerTest")                           ##
## Set relevant slots of the new model object:
res@sigma <- sigma(model)                                     ##     
res@vcov_beta <- as.matrix(vcov(model))                       ##    

out$sigma <- sigma(model)
out$vcov_beta <- as.matrix(vcov(model))                       ##

#varpar_opt <- unname(c(res@theta, res@sigma))
varpar_opt <- unname(c(getME(model, "theta"), getME(model, "sigma")))

## Compute Hessian:
  h <- numDeriv::hessian(func=devfun_vp, x=varpar_opt, devfun=devfun,
                         reml=is_reml)
  # Eigen decompose the Hessian:
  eig_h <- eigen(h, symmetric=TRUE)
  evals <- eig_h$values
  neg <- evals < -tol
  pos <- evals > tol
  zero <- evals > -tol & evals < tol
  if(sum(neg) > 0) { # negative eigenvalues
    eval_chr <- if(sum(neg) > 1) "eigenvalues" else "eigenvalue"
    evals_num <- paste(sprintf("%1.1e", evals[neg]), collapse = " ")
    warning(sprintf("Model failed to converge with %d negative %s: %s",
                    sum(neg), eval_chr, evals_num), call.=FALSE)
  }
  # Note: we warn about negative AND zero eigenvalues:
  if(sum(zero) > 0) { # some eigenvalues are zero
    eval_chr <- if(sum(zero) > 1) "eigenvalues" else "eigenvalue"
    evals_num <- paste(sprintf("%1.1e", evals[zero]), collapse = " ")
    warning(sprintf("Model may not have converged with %d %s close to zero: %s",
                    sum(zero), eval_chr, evals_num))
  }
  # Compute vcov(varpar):
  pos <- eig_h$values > tol
  q <- sum(pos)
  # Using the Moore-Penrose generalized inverse for h:
  h_inv <- with(eig_h, {
    vectors[, pos, drop=FALSE] %*% diag(1/values[pos], nrow=q) %*%
      t(vectors[, pos, drop=FALSE]) })


## res@vcov_varpar <- 2 * h_inv # vcov(varpar)
  out$vcov_vpar <- 2 * h_inv # vcov(varpar)
  # Compute Jacobian of cov(beta) for each varpar and save in list:
  Jac <- numDeriv::jacobian(func=get_covbeta, x=varpar_opt, devfun=devfun)

  res@Jac_list <- lapply(1:ncol(Jac), function(i)
    array(Jac[, i], dim=rep(length(res@beta), 2))) # k-list of jacobian matrices ##

out$Jac_list <- lapply(1:ncol(Jac), function(i)
    array(Jac[, i], dim=rep(length(getME(model, "beta")), 2))) # k-list of jacobian matrices ##

out

res



aux <- compute_auxillary(fm0)

source("sh_satter.R");source("sh_contest.R")
SATmodcomp(fm0, L)
KRmodcomp(fm0, L)


aux <- tmp$aux
q <- tmp$q
PtL <- tmp$PtL
d   <- tmp$d

grad_PLcov <- lapply(1:q, function(m) {
    vapply(aux$Jac_list, function(J)
        qform(PtL[m, ], J), numeric(1L))
})

nu_m <- vapply(1:q, function(m) {
    2*(d[m])^2 / qform(grad_PLcov[[m]], aux$vcov_varpar)
}, numeric(1L)) # 2D_m^2 / g'Ag



grad_PLcov[[m]]
aux$vcov_varpar

qform(grad_PLcov[[m]], aux$vcov_varpar)


























load_all("pbkrtest")
data(beets)

dd <- sleepstudy[sample.int(nrow(sleepstudy), 100),]
             
(fit1 <- lmer(Reaction ~ Days + (Days|Subject), dd))

load_all("pbkrtest")
ss <- SATmodcomp(fit1, c(0,1))
kk <- KRmodcomp(fit1, c(0,1))
pp <- PBmodcomp(fit1, c(0,1))

str(ss)
str(kk)
str(pp)


vcov(fit1)
vcovAdj(fit1)
compute_auxillary(fit1)$vcov_beta





## These are the same models;
## FIXME why are there warnings in fit0b
fit0  <- lmer(Reaction ~ 1 + (Days|Subject), sleepstudy)
fit0b <- remat2model(fit1, c(0, 1))

## FIXME: Why are there two almost identical functions
## What is this; they are all equal...
get_Lb_ddf(fit1, c(0,1))      ## 17 denominator df's
get_Lb_ddf(fit1, c(1,1))      ## 17 denominator df's
get_Lb_ddf(fit1, c(1,0))      ## 17 denominator df's
get_Lb_ddf(fit1, fixef(fit1)) ## 17 denominator df's

get_ddf_Lb(fit1, c(0,1))      ## 17 denominator df's
get_ddf_Lb(fit1, c(1,1))      ## 17 denominator df's
get_ddf_Lb(fit1, c(1,0))      ## 17 denominator df's
get_ddf_Lb(fit1, fixef(fit1)) ## 17 denominator df's

## These are all different
fit1 %>% remat2model(c(0,1)) %>% fixef()
fit1 %>% remat2model(c(1,1)) %>% fixef()
fit1 %>% remat2model(c(1,0)) %>% fixef()
fit1 %>% remat2model(fixef(fit1)) %>% fixef()

vcov(fit1)
vcovAdj(fit1)

compute_auxillary(fit1)

load_all("pbkrtest")
SATmodcomp(fit1, c(0,1))                 
SATmodcomp(fit1, matrix(c(0,1),nrow=1))

load_all("pbkrtest"); KRmodcomp(fit1, ~.-Days)

PBmodcomp(fit1, ~.-Days)
SATmodcomp(fit1, ~.-Days)                  





traceback()

KRmodcomp(fit1, c(0,1))                  
KRmodcomp(fit1, matrix(c(0,1),nrow=1))
aa <- KRmodcomp(fit1, update(fit1, .~.-Days))
KRmodcomp(fit1, update(fit1, .~.-Days))  %>% summary


load_all("pbkrtest")
pp<-PBmodcomp(fit1, c(0,1))                  
PBmodcomp(fit1, matrix(c(0,1),nrow=1))   
PBmodcomp(fit1, update(fit1, .~.-Days))

load_all("pbkrtest")
rr <- PBrefdist(fit1, c(0,1))                  
PBrefdist(fit1, matrix(c(0,1),nrow=1))   
PBrefdist(fit1, update(fit1, .~.-Days))

load_all("pbkrtest")
ss <- SATmodcomp(fit1, c(0,1))
KRmodcomp(fit1, c(0,1))
PBmodcomp(fit1, c(0,1), nsim=100, cl=1)



m2 <- ~.-Days
largeModel <- fit1

formula2model <- function(largeModel, formula)
    UseMethod("formula2model")

formula2model.lmerMod <- function(largeModel, formula){
    ff <- update(formula(largeModel), formula)
    cl <- getCall(fit1)
    args <- as.list(cl)
    args$formula <- ff
    Call <- as.call(c(list(quote(lme4::lmer)), args[-1]))
    smallModel <- eval.parent(Call)
    smallModel
}




                          
formula2model(fit1, m2)








PBmodcomp(fit1, L)
traceback()


L <- matrix(c(0,1),nrow=1)

load_all("pbkrtest")
m2 <- remat2model(fit1, L)
m2 <- remat2model(fit1, L, REML=FALSE)

getME(fit1, "is_REML")
getME(m2, "is_REML")

load_all("pbkrtest")
PBrefdist(fit1, m2, details=10, cl=1)

(fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
L1 <- cbind(0,1)
SATmodcomp(fm1, L1)

(fm2 <- lmer(Reaction ~ Days + I(Days^2) + (Days|Subject), sleepstudy))
## Define 2-df contrast - since L has 2 (linearly independent) rows
## the F-test is on 2 (numerator) df:
L2 <- rbind(c(0, 1, 0), # Note: ncol(L) == length(fixef(fm))
            c(0, 0, 1))

load_all("pbkrtest"); SATmodcomp(fm2, L2)

out <- pbkrtest:::.do_sampling(fit1, m2, 100, cl=1, get_fun=pbkrtest:::.get_refdist_merMod)

out <- pbkrtest:::.do_sampling(fit1, m2, 100, cl=NULL, get_fun=pbkrtest:::.get_refdist_merMod)



foo <- function(){
    simulate(m2, 10)
}

foo()








load_all("pbkrtest")
fit2 <- lmer(sugpct ~ block + sow + harvest + 
                      (1 | block:harvest), data=beets, REML=FALSE)

fit2 <- lmer(sugpct ~ block + sow + harvest + 
                      (1 | block:harvest), data=beets, REML=FALSE)
fit3 <- update(fit2, ~.-harvest)

KRmodcomp(fit2, fit3) %>% summary
PBmodcomp(fit2, fit3) %>% summary

load_all("pbkrtest")
KRanova(fit2, test="chisq")
KRanova(fit2, test="F")
KRanova(fit2)
