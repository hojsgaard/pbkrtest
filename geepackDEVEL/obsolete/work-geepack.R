library(MuMIn)
require(geepack)

## Don't show: 
 if(require(geepack)) { 
## End(Don't show)

data(ohio)

fm1 <- geeglm(resp ~ age * smoke, id = id, data = ohio,
    family = binomial, corstr = "exchangeable", scale.fix = TRUE)
fm2 <- update(fm1, corstr = "ar1")
fm3 <- update(fm1, corstr = "unstructured")

model.sel(fm1, fm2, fm3, rank = QIC)

## Not run: 
##D # same result:
##D     dredge(fm1, m.lim = c(3, NA), rank = QIC, varying = list(
##D     corstr = list("exchangeable", "unstructured", "ar1")
##D     ))      
## End(Not run)
## Don't show: 
 } 
Loading required package: geepack


library(MuMIn)
Cement$X1 <- cut(Cement$X1, 3)
Cement$X2 <- cut(Cement$X2, 2)

fm1 <- glm(formula = y ~ X1 + X2 * X3, data = Cement)
fm2 <- update(fm1, . ~ . - X1 - X2)
fm3 <- update(fm1, . ~ . - X2 - X3)

## ranked with AICc by default
(msAICc <- model.sel(fm1, fm2, fm3))

## ranked with BIC
model.sel(fm1, fm2, fm3, rank = AIC, rank.args = alist(k = log(nobs(x))))


library(geepack)

fm1 <- geeglm(formula = y ~ X1 + X2 * X3, data = Cement)
fm2 <- update(fm1, . ~ . - X1 - X2)
fm3 <- update(fm1, . ~ . - X2 - X3)


geepack::QIC(fm1)
