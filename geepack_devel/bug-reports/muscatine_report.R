







## center age

## in order to be sure (is actually already ordered, but geeglm
## asks the observations to be ordered like that
musc <- musc[order(musc$id), ] 
library(geepack)
library(tidyverse)
library(geeM)
musc$zz <-  as.numeric(!musc$numobese)
mm1 <- glm(numobese ~ gender, data=musc, family=binomial())
mm2 <- glm(cbind(numobese, zz) ~ gender, data=musc, family=binomial())

load_all("../geepack")





geem1 <- as.geem(gee1)

geem2 <- as.geem(gee2)

## model specification with fixed.value gives error message
gee1 <- geeglm(formula = numobese ~ gender + cage + cage2 +
                   gender:cage + gender:cage2, id = id,
               waves = occasion, data = musc, family = binomial(),
               scale.fix = TRUE)

gee1


## attempt to reproduce Table 11.2, p. 308
## R will abort

gee2 <- geeglm(formula = numobese ~ gender + cage + cage2 +
          gender:cage + gender:cage2, id = id,
          waves = occasion, data = muscatine, family = binomial(),
          corstr = "unstructured")




## FUN LITTLE EXAMPLE

foo <- function(formula, data, ...){
    call <- match.call()
    mf <- match.call(expand.dots = FALSE)
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    Y <- model.response(mf, "any")
    mt <- attr(mf, "terms")
    X <- if (!is.empty.model(mt)) 
        model.matrix(mt, mf, contrasts=NULL)

    list(call=call, mf=mf, mt=mt, Y=Y, X=X)
}


data(ohio, package="geepack")
dat <- ohio[1:20,]
dat$resp0  <- as.numeric(!dat$resp)
dat$resp[1] <- NA

fit <- foo(cbind(resp, resp0) ~ age + smoke + age:smoke, data=dat)
fit


match.call(foo)

mf <- match.call(foo, call("foo", "cbind(resp, resp0) ~ age + smoke + age:smoke", dat))

mf[[1L]] <- as.name("model.frame")
mf <- eval(mf)
Y <- model.response(mf, "any")

fit <- foo(resp ~ age + smoke + age:smoke, data=dat)
fit


foo(cbind(numobese, zz) ~ gender, data=musc)

###
