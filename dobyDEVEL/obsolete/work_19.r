#'
#' ## A an alternative based on dplyr and broom:
#' ##  gg <- CO2 %>% dplyr::group_by(Treatment)
#' ##  gg %>% dplyr::do(broom::tidy(lm(1 / uptake ~ log(conc), data=.)))



library(doBy)
library(broom)
library(geepack)
library(lme4)
data(Orthodont, package="nlme")

load_all("doby")


lm1  <- lm(distance ~ age + Sex, data=Orthodont)

glm1  <- glm(distance ~ age + Sex, data=Orthodont)
glm2  <- glm(distance ~ age + Sex, data=Orthodont, family=Gamma(link="identity"))
gee1 <- geeglm(distance ~ age + Sex, id=Subject, data=Orthodont)
lme1 <- lmer(distance ~ age + Sex + (1|Subject), data=Orthodont)

tidy(lm1)
tidy(lm1, conf.int=T)

l1 <- rbind(m1=c(1, 1, 1), m2=c(1,1,0))
l1 <- rbind(c(1, 1, 1), c(1,1,0))
l2 <- c(1, 1, 1)

load_all("doby");
le1  <- linest(lm1, l1); le1
load_all("doby");
le1  <- linest(lm1, l2); le1

load_all("doby");
ee <- esticon(lm1, l1, beta0=c(17,17));
load_all("doby");
ee <- esticon(lm1, l2);

ee

tidy(ee)

load_all("doby"); 
tidy(ee, conf.int=T, conf.level=.99) ## sært
tidy(ee, conf.int=T) ## sært

load_all("doby");
confint(ee)

confint(ee, 1)
confint(lm1, 1)


data(iris)
lm1  <- lm(Sepal.Length ~ Sepal.Width + Species + Sepal.Width : Species, data=iris)
## Note that the setosa parameters are set to zero
coef(lm1)

## Estimate the intercept for versicolor

load_all("doby")
lambda1 <- c(1, 0, 1, 0, 0, 0)
ee  <- esticon(lm1, L=lambda1)






     
     ## Estimate the difference between versicolor and virgica intercept
     ## and test if the difference is 1
     lambda2 <- c(0, 1, -1, 0, 0, 0)
     ee <- esticon(lm1, L=lambda2, beta0=1)
     
     ## Do both estimates at one time
     esticon(lm1, L=rbind(lambda1, lambda2), beta0=c(0, 1))
     
     ## Make a combined test for that the difference between versicolor and virgica intercept
     ## and difference between versicolor and virginica slope is zero:
     lambda3 <- c(0, 0, 0, 0, 1, -1)
     esticon(lm1, L=rbind(lambda2, lambda3), joint.test=TRUE)
     




confint.default(ee)

vcov(lm1)
vcov(ee)



confint(lm1, c("age", "SexFemaless"))



confint_tidy(ee) ## sært

.ci_fun(ee, level=.95) ## Ser rigtigt ud
.ci_fun(ee, level=.99) ## Samme...




confint(ee)

vcov.esticon_class <- function(object, ...){
    attr(object, "vcv")
}

diag(ee$std.error^2)




load_all("doby")
le2  <- linest(lm1, l2) ## FIXME
le2

load_all("doby")
linest(lm1)
aa <- linest.lm(lm1)
confint(aa) ## FIXME

linest(lm1)
linest(glm1)

linest(gee1)
linest(lme1)

load_all("doby")
linest(lm1)
linest(lm1)

load_all("doby")
linest(lm1) %>% tidy
linest(glm1) %>% tidy
linest(glm2) %>% tidy
linest(gee1) %>% tidy
linest(lme1) %>% tidy

load_all("doby")
linest(lm1) %>% tidy(conf.int=T)
linest(glm1) %>% tidy(conf.int=T)
linest(glm2) %>% tidy(conf.int=T)
linest(gee1) %>% tidy(conf.int=T)
linest(lme1) %>% tidy(conf.int=T)

load_all("doby")
linest(glm1)
linest.glm(glm1)

linest(glm2)
linest.glm(glm2)

load_all("doby")
linest(gee1)
linest.geeglm(glm1)

load_all("doby")
linest(lme1)
linest.lmerMod(lme1) 





