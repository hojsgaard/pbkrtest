library(lme4)
library(pbkrtest)
options("digits"=5)
data("beets", package = "pbkrtest")
model <- sug  <- lmer(sugpct ~ block + sow + harvest + (1 | block:harvest), data = beets, REML = FALSE)

load_all("lmerTest")

rho$thopt <- getME(model, "theta")
sigma(model)

vec <- c(theta=getME(model, "theta"), sigma=sigma(model))

dd <- devfun5(model,  getME(model, "is_REML"))
hh <- myhess(dd, vec)
AA <- 2*solve(hh)



h <- myhess(dd, c(rho$thopt, sigma = rho$sigma))
