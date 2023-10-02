










require(lme4)
require(optimx) ## FIXME :: Add to package
require(pbkrtest)
library(magrittr)
## Linear mixed effects model:
data(beets, package="pbkrtest")
options(warn=-1)

lg <- lmer(sugpct ~ block + sow + harvest + (1|block:harvest),
              data=beets, REML=FALSE)
sm <- update(lg, .~. -harvest)




## source("seqPB.R")
## (qq <- seqPBmodcomp(lg, sm, h=5)) %>% summary

load_all("pbkrtest")
(pp <- PBmodcomp(lg, sm, nsim=100))  %>% summary




options("mc.cores")[[1]]




load_all("pbkrtest")
(pp <- PBmodcomp(lg, sm, nsim=200))  %>% summary


mclapply(rep(50, 4), function(n) rnorm(n), mc.cores=1.4)


library(parallel)
clus <- makeCluster(rep("localhost", 4))
options("mc.cores"=NULL)
options(cl=NULL)

load_all("pbkrtest")

## Controlling parallel computing

options("mc.cores"=NULL)
options(cl=NULL)
ss <- PBrefdist(lg, sm, nsim=50)

options("mc.cores"=3)
options(cl=NULL)
ss <- PBrefdist(lg, sm, nsim=50)
o
options("mc.cores"=NULL)
options(cl=clus)
ss <- PBrefdist(lg, sm, nsim=50)

options("mc.cores"=NULL)
options(cl=NULL)
ss <- PBrefdist(lg, sm, nsim=50, cl=3)
ss <- PBrefdist(lg, sm, nsim=50, cl=clus)

options("mc.cores"=3)
options(cl=NULL)
ss <- PBrefdist(lg, sm, nsim=50, cl=3)
ss <- PBrefdist(lg, sm, nsim=50, cl=clus)

ss <- PBrefdist(lg, sm, nsim=50)
options(cl=clus)
ss <- PBrefdist(lg, sm, nsim=50)


ss <- PBrefdist(lg, sm, nsim=50, cl=2)
ss <- PBrefdist(lg, sm, nsim=50, cl=clus)



options(cl=4)
ss <- PBrefdist(lg, sm, nsim=50)
ss <- PBrefdist(lg, sm, nsim=50, cl=2)
ss <- PBrefdist(lg, sm, nsim=50, cl=clus)

load_all("pbkrtest")
options(cl=clus)
ss <- PBrefdist(lg, sm, nsim=50)
ss <- PBrefdist(lg, sm, nsim=50, cl=2)
ss <- PBrefdist(lg, sm, nsim=50, cl=clus)

y.sim <- simulate(sm, 10)
yy <- y.sim[,1]
refit(sm, newresp = yy)
refit(lg, newresp = yy)

Sys.info()['sysname'] == "Windows"
library(parallel)
nc <- detectCores()
cl <- makeCluster(rep("localhost", nc))


## Linear model

load_all("pbkrtest")
lg2 <- lm(dist ~ speed + I(speed^2), data=cars)
sm2 <- lm(dist ~ speed, data=cars)
anova(lg2, sm2)
load_all("pbkrtest")
rd2 <- PBrefdist(lg2, sm2, nsim=100)
rd2



## Logistic regression

load_all("pbkrtest")
data(respiratory, package="geepack")
respiratory$center <- factor(respiratory$center)
head(respiratory)


lg3 <- glm(outcome ~ center + treat + age + baseline, data=respiratory,
           family=binomial())
sm3 <- update(lg3, . ~ . - center)
anova(lg3, sm3, test="Chisq")
coef(summary(lg3))
rd3 <- PBrefdist(lg3, sm3, nsim=1000)
rd3

options(cl=4)
load_all("pbkrtest")
lg4 <- glmer(outcome ~ center + treat + age + baseline + (1 | id), data=respiratory, family=binomial())
sm4 <- update(lg4, . ~ . - center)
anova(lg4, sm4, "Chisq"=TRUE)
rd4 <- PBrefdist(lg4, sm4, nsim=500)
rd4


    
                    cbpp, binomial, nAGQ = 0))




library(geepack)
gee.ind <- geeglm(outcome ~ center + treat + age + baseline, data=respiratory, id=id, 
          family=binomial(), corstr="independence")
gee.exc <- geeglm(outcome ~ center + treat + age + baseline, data=respiratory, id=id, 
             family=binomial(), corstr="exchangeable")

options("digits"=8)
coef(summary(m1))
coef(summary(gee.ind))
coef(summary(gee.exc))













