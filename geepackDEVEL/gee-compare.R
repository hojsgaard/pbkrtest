# Dependencies
library(geepack)
library(geeM)

load_all("geepack")
load_all("geeM2")

dat <- readr::read_csv("tiago.csv")
dat <- transform(dat,
                 id2=factor(id), id3=as.integer(factor(id)),
                 wave2=as.integer(wave))
dat %>% head
dat %>% lapply(storage.mode)

dat$id %>% unique  %>% length

## order data; bloody mess
dat$id3 %>% diff
dat2 <- doBy::orderBy(~id + wave, data=dat)
dat2$id3 %>% diff

## Issue: What can id be? character? numeric? factor? functions below
## respond differently
##
## Issue: Same with waves

CORSTR  <- "independence"
FORMULA <- vi ~ vd + cov

hip_gg <- geeglm(data = dat2,
                formula = FORMULA 
                family = poisson,
                ## id = id,
                id = id2,
                waves = wave,
                corstr=CORSTR)
summary(hip_gg)

hip_gm2 <- geem2(data = dat2,
                formula = FORMULA,
                family = poisson,
                id = id3,
                waves = wave,
                corstr=CORSTR)
summary(hip_gm2)

hip_gm <- geem(data = dat,
                formula = FORMULA,
                family = poisson,
                id = id, 
                waves = wave2,
                corstr=CORSTR)
summary(hip_gm)


coef(hip_gg)
coef(hip_gm2)
coef(hip_gm)

summary(hip_gg)  %>% coef
summary(hip_gm2) %>% coef ## Hmmm; I need se's
summary(hip_gm)  %>% coef ## Hmmm; I need se's



