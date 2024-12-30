
library(geepack)
 
data(iris)
 
set.seed(1)
 
iris$id <- sample(1:10, replace = TRUE, size = nrow(iris))
 
# Should throw error/warning, but does not
fit <- geeglm(Sepal.Length ~ Petal.Length, id = id, data = iris, family = gaussian, corstr = "exchangeable")
 
# No warning thrown, produces plausible results
summary(fit)
tidy(fit)
 
# correct way to code
iris_arranged <- dplyr::arrange(iris, id)
 
fit2 <- geeglm(Sepal.Length ~ Petal.Length, id = id, data = iris_arranged, family = gaussian, corstr = "exchangeable")
 
summary(fit2)
tidy(fit2)
 
all.equal(coef(fit), coef(fit2)) # "Mean relative difference: 0.00527"
all.equal(vcov(fit), vcov(fit2)) # "Mean relative difference: 0.192"













library(tidyverse)
library(geepack)

# Don't need 10000 to see the "problem"; thats why I wrote a "minimal" reproducible example
N <- 60
dat <- tibble(ID = Vectorize(digest::sha1)(1:N),
              var1 = rep(c(0,1),N/2),var2 = rep(c("cat1","cat2"),N/2))

dat$ID  %>% unique %>% length()
## Same as N - so all clusters have length 1.. Hmmm
dat$ID2 <- factor(dat$ID)

m1 <- geeglm(var1 ~ var2, data = dat, id =  ID)
m1
m2 <- geeglm(var1 ~ var2, data = dat, id =  ID2)
m2

summary(m1)
summary(m2)
## So - would you still claim that geeglm works by turning ID into a factor?









library(geepack)
document("pkg")
load_all("pkg")

muscatine$cage <- muscatine$age - 12                                         
muscatine$cage2 <- muscatine$cage^2                                          

f1 <- numobese ~ gender                                                 
f2 <- numobese ~ gender + cage + cage2 +                                
    gender:cage + gender:cage2                                          

gee1 <- geeglm(formula = f1, id = id,                                   
               waves = occasion, data = muscatine, family = binomial(),      
               corstr = "independence")                                 

gee2 <- geeglm(formula = f2, id = id,                                   
               waves = occasion, data = muscatine, family = binomial(),      
               corstr = "independence")                                 

QIC(gee1)
QIC(gee2)

tidy(gee1)                                                              
tidy(gee2)                                                              





library(geepack)
document("pkg")
load_all("pkg")

timeorder <- rep(1:5, 6)
tvar      <- timeorder + rnorm(length(timeorder))
idvar <- rep(1:6, each=5)
uuu   <- rep(rnorm(6), each=5)
yvar  <- 1 + 2*tvar + uuu + rnorm(length(tvar))
simdat <- data.frame(idvar, timeorder, tvar, yvar)
head(simdat,12)

load_all("pkg")
mod1 <- geeglm(yvar~tvar, id=idvar, data=simdat, corstr="ar1")
mod1





data(dietox)
dietox$Cu     <- as.factor(dietox$Cu)
mf <- formula(Weight ~ Cu * (Time + I(Time^2) + I(Time^3)))
gee1 <- geeglm(mf, data=dietox, id=Pig, family=poisson("identity"), corstr="ar1")
gee1
coef(gee1)
vcov(gee1)
summary(gee1)
coef(summary(gee1))



mf2 <- formula(Weight ~ Cu * Time + I(Time^2) + I(Time^3))
gee2 <- geeglm(mf2, data=dietox, id=Pig, family=poisson("identity"), corstr="ar1")


anova(gee2)


    














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
