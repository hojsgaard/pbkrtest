
#Consider regression models for the `cars` dataset:

mod0 <- lm(dist ~ 1, data=cars); coef(mod0)
mod1 <- update(mod0, .~. + speed); coef(mod1)
mod2 <- update(mod1, .~. + I(speed^2)); coef(mod2)
#'
#' Reducing `mod2` to `mod0` corresponds to restricting the model space for
#' `mod2` and so on:
#'
L21 <- model2remat(mod2, mod1); L21
L20 <- model2remat(mod2, mod0); L20
L10 <- model2remat(mod1, mod0); L10

#' The other way around is that given a restriction matrix and a large
#' model, we can construct the corresponding smaller model:

new1  <- remat2model(mod2, L21); coef(new1)
new0a <- remat2model(mod2, L20); coef(new0a)
new0b <- remat2model(mod1, L10); coef(new0b)
#'
#' It should be checked that the original and new model matrices span the
#' same space. For now we will simply check that the fitted values are
#' practically identical:
#'
#'
#' eps <- 1e-8
#' max(abs(fitted(new1)  - fitted(mod1))) < eps
#' max(abs(fitted(new0a) - fitted(mod0))) < eps
#' max(abs(fitted(new0b) - fitted(mod0))) < eps
