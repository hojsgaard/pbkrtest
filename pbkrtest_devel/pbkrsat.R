#' ## Satterthwaite in pbkrtest
#' ### Søren Højsgaard
#' date: `date()`

library(ggplot2)
library(broom)
load_all("pbkrtest")

#' ## Sleep study data

qplot(Days, Reaction, color=Subject, data=sleepstudy) + geom_line()

#' Quadratic curve:

(fit1 <- lmer(Reaction ~ Days + I(Days^2) + (Days|Subject), sleepstudy))
summary(fit1) %>% coef

#' Remove quadratic term; corresponds to restriction matrix (0, 0, 1)

(fit2 <- update(fit1, ~.-I(Days^2)))
summary(fit2) %>% coef

#' ## Three methods available
#'
#' Each method takes argument in different forms

L <- c(0, 0, 1)
#' ### Kenward-Roger
#' 
k1 <- KRmodcomp(fit1, L); k1
k2 <- KRmodcomp(fit1, fit2); k2
k3 <- KRmodcomp(fit1, ~.-I(Days^2)); k3

#' ### Satterthwaite
s1 <- SATmodcomp(fit1, L); s1
s2 <- SATmodcomp(fit1, fit2); s2
s3 <- SATmodcomp(fit1, ~.-I(Days^2)); s3

#' ### Parametric bootstrap
p1 <- PBmodcomp(fit1, L); p1
p2 <- PBmodcomp(fit1, fit2); p2
p3 <- PBmodcomp(fit1, ~.-I(Days^2)); p3


#' ## Beets data

data(beets)

fit1 <- lmer(sugpct ~ block + sow + harvest + 
                 (1 | block:harvest), data=beets, REML=FALSE)

#' Remove harvest effect

fit2 <- update(fit1, ~.-harvest)

L <- c(rep(0, 7), 1)
#' ### Kenward-Roger
#' 
k1 <- KRmodcomp(fit1, L); k1
k2 <- KRmodcomp(fit1, fit2); k2
k3 <- KRmodcomp(fit1, ~.-harvest); k3

#' ### Satterthwaite
s1 <- SATmodcomp(fit1, L); s1
s2 <- SATmodcomp(fit1, fit2); s2
s3 <- SATmodcomp(fit1, ~.-harvest); s3

#' ### Parametric bootstrap
p1 <- PBmodcomp(fit1, L); p1
p2 <- PBmodcomp(fit1, fit2); p2
p3 <- PBmodcomp(fit1, ~.-harvest); p3

#' Remove sow effect
#' 
fit2 <- update(fit1, ~.-sow)

model2remat(fit1, fit2) %>% zapsmall

L <- matrix(0, nr=4, nc=8)
L[, 4:7] <- diag(1, 4)

#' ### Kenward-Roger
#' 
k1 <- KRmodcomp(fit1, L); k1
k2 <- KRmodcomp(fit1, fit2); k2
k3 <- KRmodcomp(fit1, ~.-sow); k3

#' ### Satterthwaite
s1 <- SATmodcomp(fit1, L); s1
s2 <- SATmodcomp(fit1, fit2); s2
s3 <- SATmodcomp(fit1, ~.-sow); s3

#' ### Parametric bootstrap
p1 <- PBmodcomp(fit1, L); p1
p2 <- PBmodcomp(fit1, fit2); p2
p3 <- PBmodcomp(fit1, ~.-sow); p3
