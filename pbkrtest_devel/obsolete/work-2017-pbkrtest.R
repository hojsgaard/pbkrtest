require(lme4)
require(optimx)
require(pbkrtest)
## Linear mixed effects model:
data(beets, package="pbkrtest")

sug   <- lmer(sugpct ~ block + sow + harvest + (1|block:harvest),
data=beets, REML=FALSE, control =lmerControl(optimizer =
"optimx",optCtrl = list(method = "nlminb", starttests = FALSE, kkt =
FALSE)))

sug.h <- update(sug, .~. -harvest)

sm <- sug.h
lg <- sug
y.sim <- simulate(sml, 10)
yy <- y.sim[,1]
refit(sm, newresp = yy)
refit(lg, newresp = yy)





sug2   <- lmer(sugpct ~ block + sow + harvest + (1|block:harvest),
              data=beets, REML=FALSE)
sug2.h <- update(sug2, .~. -harvest)


PBmodcomp(sug2, sug2.h, nsim=50)

PBmodcomp(sug, sug.h, nsim=50)








rr <- PBrefdist(sug, sug.h, nsim=50)



## Error in optwrap(object@optinfo$optimizer, ff, x0, lower = lower,
control = control$optCtrl,  :
##   must specify 'method' explicitly for optimx




devtools::install_url) ("https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-4.tar.gz")



library(lmSupport)
library(lme4)
library(pbkrtest)
packageVersion("pbkrtest")


# 2 x 2 fully within-subjects design

# One observation per cell, therefore variance components are confounded

# simulate data -----------------------------------------------------------

set.seed(12346)
n <- 40 # number of subjects
r <- 2 # number of replications
dat <- data.frame(subject_id = rep(seq(n), each=2*r),
                  subInt = rep(rnorm(n), each=2*r),
                  subSlope1 = rep(rnorm(n), each=2*r),
                  subSlope2 = rep(rnorm(n), each=2*r),
                  subSlope3 = rep(rnorm(n), each=2*r),
                  prestigeC = rep(rep(c(-0.5,0.5), each=r), times=n),
                  scienceC = rep(rep(c(-0.5,0.5), each=r/2), times=n))
dat <- within(dat, like <- subInt + (.4 + subSlope1)*prestigeC + (.2 + subSlope2)*scienceC +
                (.6 + subSlope3)*prestigeC*scienceC + rnorm(n*r*2))
varDescribe(dat)


# analyze -----------------------------------------------------------------

# Create the full model that reprduces the 2 x 2 within-subjects ANOVA
model_1g <- lmer(like ~ 1 + prestigeC * scienceC + (1 + prestigeC * scienceC|subject_id),
                 control=lmerControl(check.nobs.vs.nRE="ignore"), data=dat)
summary(model_1g)

# Create compact models for KR model comparisons
dat$interC <- dat$prestigeC*dat$scienceC
model_1g.0p <- lmer(like ~ 1 + scienceC + interC + (1 + prestigeC * scienceC|subject_id),
                 control=lmerControl(check.nobs.vs.nRE="ignore"), data=dat)
model_1g.0s <- lmer(like ~ 1 + prestigeC + interC + (1 + prestigeC * scienceC|subject_id),
                    control=lmerControl(check.nobs.vs.nRE="ignore"), data=dat)
model_1g.0i <- lmer(like ~ 1 + prestigeC + scienceC + (1 + prestigeC * scienceC|subject_id),
                    control=lmerControl(check.nobs.vs.nRE="ignore"), data=dat)

# Get the two main effects and the interaction effect
KRmodcomp(model_1g, model_1g.0p)
KRmodcomp(model_1g, model_1g.0s)
KRmodcomp(model_1g, model_1g.0i)

# Run "Anova" to show that this function produces exactly the same output
Anova(model_1g, type = 3, test = "F")



# Check whether the the 2 x 2 RM ANOVA with data in wide format produces
# EXACTLY the same results

# transform data into wide format -----------------------------------------

dattemp <- dat
dattemp$index<-rep(1:4, each=1)
dattemp$prestigeC<-NULL
dattemp$scienceC<-NULL
dattemp$subInt<-NULL
dattemp$subSlope1<-NULL
dattemp$subSlope2<-NULL
dattemp$subSlope3<-NULL
dattemp$interC<-NULL

datw<-reshape(data = dattemp,
              idvar = "subject_id",
              v.names = c("like"),
              timevar = "index",
              direction = "wide")


# analyze -----------------------------------------------------------------

datw$mainpre <- ((-.5)*datw$like.1)+((-.5)*datw$like.2)+((.5)*datw$like.3)+((.5)*datw$like.4)
datw$mainsci <- ((-.5)*datw$like.1)+((.5)*datw$like.2)+((-.5)*datw$like.3)+((.5)*datw$like.4)
datw$inter <- ((-.5)*datw$like.1)+((.5)*datw$like.2)+((.5)*datw$like.3)+((-.5)*datw$like.4)

m2d <- lm(mainpre ~ 1, data=datw)
m2e <- lm(mainsci ~ 1, data=datw)
m2f <- lm(inter ~ 1, data=datw)
summary(m2d)
summary(m2e)
summary(m2f)





