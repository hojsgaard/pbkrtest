library(geepack)

mss <- read.csv("mss1.csv")

fet=geeglm(total~exer+alcohol+as.factor(visit)+LOAN+as.factor(REL_3),id=ID,data=na.omit(mss),family=gaussian,corstr = "exchangeable")

fet.glm=glm(total~exer+alcohol+as.factor(visit)+LOAN+as.factor(REL_3),data=na.omit(mss),family=gaussian)

coef(summary(fet))

coef(summary(fet.glm))






library(magrittr)

data(dietox)
dietox$Cu     <- as.factor(dietox$Cu)
mf <- formula(Weight ~ Cu * (Time + I(Time^2) + I(Time^3)))
gee1 <- geeglm(mf, data=dietox, id=Pig, family=poisson("identity"), corstr="ar1")
gee1
coef(gee1)
vcov(gee1)
vcov(gee1)  %>% diag  %>% sqrt

summary(gee1)
coef(summary(gee1))

vcov(gee1)



mf2 <- formula(Weight ~ Cu * Time + I(Time^2) + I(Time^3))
gee2 <- geeglm(mf2, data=dietox, id=Pig, family=poisson("identity"), corstr="ar1")
anova(gee2)
