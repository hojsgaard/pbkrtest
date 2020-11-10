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
