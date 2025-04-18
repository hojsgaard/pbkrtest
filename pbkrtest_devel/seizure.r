library(tidyverse)
library(lme4)
library(broom.mixed)
library(pbkrtest)
options("digits"=3)
dat <- geepack::seizure |> as_tibble()
dat$subject <- factor(1:nrow(dat))
dat |> head()
dat[,1:4] |> cor()

dat.long <- dat |> pivot_longer(cols=c(y1, y2, y3, y4), names_to = "period")
dat.long |> head()

fit1 <- glmer(value ~ age + (1|subject), data=dat.long, family=poisson())
fit2 <- glmer(value ~ (1|subject), data=dat.long, family=poisson())

y <- simulate(fit2, 1)[,1]
dat2.long <- dat.long
dat2.long$value <- y


fitp1.glmer <- glmer(value ~ age + (1|subject), data=dat2.long, family=poisson())
fitp2.glmer <- glmer(value ~ (1|subject), data=dat2.long, family=poisson())

fitp1.glm   <- glm(  value ~ age , data=dat2.long, family=poisson())
fitp2.glm   <- glm(  value ~ 1, data=dat2.long, family=poisson())

fitn1.lmer  <- lmer( value ~ age + (1|subject), data=dat2.long, REML=F)
fitn2.lmer  <- lmer( value ~ (1|subject), data=dat2.long, REML=F)

fitn1.glm   <- glm(  value ~ age , data=dat2.long, family=gaussian())
fitn2.glm   <- glm(  value ~ 1, data=dat2.long, family=gaussian())


## PBmodcomp(fitp1.glmer, fitp2.glmer)
## anovax(fitp1.glmer, fitp2.glmer, test="x2", control=list(nsim=50))

anovax(fitp1.glmer, fitp2.glmer, test="x2")
anovax(fitp1.glm, fitp2.glm, test="x2")
anovax(fitn1.lmer, fitn2.lmer, test="x2")
anovax(fitn1.glm, fitn2.glm, test="x2")


PBmodcomp(fitp1.glmer, fitp2.glmer)
X2modcomp(fitp1.glmer, fitp2.glmer)
PBmodcomp(fitp1.glm, fitp2.glm)
X2modcomp(fitp1.glm, fitp2.glm)
PBmodcomp(fitn1.lmer, fitn2.lmer)
X2modcomp(fitn1.lmer, fitn2.lmer)
PBmodcomp(fitn1.glm, fitn2.glm)
X2modcomp(fitn1.glm, fitn2.glm)




## refit.glm <- function(object, newresp, ...){
##     local_data <- eval(object$call$data)
##     y_var <- all.vars(formula(object))[1]
##     local_data[[y_var]] <- newresp
##     out <- update(object, data=local_data)
##     return(out)
## }



lg |> logLik()
sm |> logLik()


lg2 <- refit(lg, y)
sm2 <- refit(sm, y) 

getLRT(lg, sm)
getLRT(lg2, sm2)



get_coverage <- function(lg, sm, sim, sig.level=c(0.1, 0.05, 0.01)){
    nsim <- ncol(sim)
    v <- sapply(1:nsim, function(i){
        y <- sim[[i]]
        lg2 <- suppressWarnings(refit(lg, y))
        sm2 <- suppressWarnings(refit(sm, y))    
        getLRT(lg2, sm2)
    })
    pp <- t(v)[,3]

    out <- sapply(sig.level, function(s){
        sum(pp <= s) / length(pp)
    })
    names(out) <- sig.level
    return(out)
}

nsim <- 100
sim <- simulate(fitp2.glmer, nsim)


lg <- fitp1.glmer
sm <- fitp2.glmer
get_coverage(lg, sm, sim)

lg <- fitp1.glm
sm <- fitp2.glm
get_coverage(lg, sm, sim)

lg <- fitn1.lmer
sm <- fitn2.lmer
get_coverage(lg, sm, sim)

lg <- fitn1.glm
sm <- fitn2.glm
get_coverage(lg, sm, sim)







## fit1 <- lmer(value ~ age + (1|subject), data=dat.long)
## fit2 <- lmer(value ~ (1|subject), data=dat.long)

## fit1 <- glmer(value ~ age + (1|subject), data=dat.long)
## fit2 <- lmer(value ~ (1|subject), data=dat.long)

## fm1 <- gls(value ~ age, data=dat.long,
##            correlation = corAR1(form = ~ 1 | subject))
## fm2 <- gls(value ~ 1, data=dat.long,
##            correlation = corAR1(form = ~ 1 | subject))


getLRT(fm1, fm2)

logLik(fm1)

logLik(update(fm1, method="ML"))

logLik(fm1, method="REML")
logLik(fm1)

dat2 <- dat.long
dat2$value <- y
update(fm1, data=dat2)
fm1

cl$method <- "ML"
gls_refit(object, sim[,2])

y <- sim[[1]]



y <- sim[,3]
print(head(y))

load_all("_pbkrtest")
fit1a <- refit(fit1, y)
fit2a <- refit(fit2, y)

fit1b <- update(fit1a, REML=F)
fit2b <- update(fit2a, REML=F)

fit1c <- update(fit1, REML=F)
fit2c <- update(fit2, REML=F)


fit1 |> getME("y") |> head()   ## Originale data
fit1c |> getME("y") |> head()  ## Originale data
fit1b |> getME("y") |> head()  ## Originale data
fit1a |> getME("y") |> head()  ## Nye data

fit1 |> logLik()
fit1c |> logLik()
fit1b |> logLik()
fit1a |> logLik()

2*(logLik(fit1) - logLik(fit2))
2*(logLik(fit1, REML=F) - logLik(fit2, REML=F))
2*(logLik(fit1a) - logLik(fit2a))
2*(logLik(fit1a, REML=F) - logLik(fit2a, REML=F))

getLRT(fit1, fit2)
getLRT(fit1a, fit2a)    

getME(fit2, "y") |> head()
getME(fit1, "y") |> head()
getME(fit2a, "y") |> head()
getME(fit1a, "y") |> head()

fit2a <- refit(fit2, y)
fit2a <- update(refit(fit2, y))
fit1a <- update(refit(fit1, y))
    


lapply(1:nsim, function(i){    
    anovax(fit1, fit2, test="x2")
    anovax(fit1a, fit2a, test="x2")
})

replicate(3, {
    y <- simulate(fit1, 1)[,1]
    anovax(refit(fit0, sim[[2]]), refit(fit1, sim[[2]]), test="x2")
})

replicate(3, {
    y <- simulate(fit1, 1)[,1]
    anovax(refit(fit0, y), refit(fit1, y), test="x2")
})

replicate(3, {
    y <- simulate(fit1, 1)[,1]
    print(head(y))
    lg <- update(fit1, y ~ . + age)
    sm <- update(fit1, y ~ .)
    print(lg); print(sm)
    dd <- anovax(lg, sm, test="x2") |> as.data.frame()
    print(dd)
    dd[2, "Pr(>Chisq)"] |> as.numeric()
})





