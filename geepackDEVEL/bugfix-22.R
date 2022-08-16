https://github.com/hojsgaard/geepack/issues/1#issue-804440981

library(geepack)
data("respiratory")

load_all("geepackDEVEL/geepack")
mgp0 <- geeglm(outcome ~ 1, 
              data = respiratory,
              id = with(respiratory, interaction(center, id)),

              family = "binomial", corstr = "exchangeable")
load_all("geepackDEVEL/geepack")
anova(mgp0)

mglm0 <- glm(outcome ~ 1, data = respiratory, family = "binomial")

anova(mglm0)


 mgp <- geeglm(outcome ~ treat, 
              data = respiratory,
              id = with(respiratory, interaction(center, id)),
              family = "binomial", corstr = "exchangeable")
 mgp2 <- geeglm(outcome ~ treat + sex + age + baseline, 
              data = respiratory,
              id = with(respiratory, interaction(center, id)),
              family = "binomial", corstr = "exchangeable")

load_all("geepackDEVEL/geepack")
 anova(mgp, mgp2)



     Df Deviance Resid. Df Resid. Dev
NULL                   443     609.41
sex   1   1.6059       442     607.80
> AIC
