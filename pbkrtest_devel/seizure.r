library(tidyverse)
library(lme4)
library(broom.mixed)
library(pbkrtest)

dat <- geepack::seizure |> as_tibble()
dat$subject <- factor(1:nrow(dat))
dat |> head()
dat[,1:4] |> cor()

dat.long <- dat |> pivot_longer(cols=c(y1, y2, y3, y4), names_to = "period")
dat.long |> head()

fit1 <- glmer(value ~ age + base + trt + period + (1|subject), data=dat.long, family=poisson)
fit1 |> tidy()

document("_pbkrtest")
load_all("_pbkrtest")
anovax(fit1, .~. - period, test="pb")

document("_pbkrtest")
load_all("_pbkrtest")
anovax(fit1, .~. - period, test="x2")

anovax(fit1, test="x2")




anova(fit1, test="Chisq")

dat2 <- simulate(fit1, 5)
dat2

y <- dat2[[2]]
update(fit1, y ~ .)
