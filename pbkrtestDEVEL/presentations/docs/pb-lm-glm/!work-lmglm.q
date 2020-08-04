data(cars)
library(pbkrtest)

lm1 <- lm(speed~-1+dist+I(dist^2),data=cars)
lm0 <- update(lm1, .~.-I(dist^2))

anova(lm1, lm0)

glm1 <- glm(speed~-1+dist+I(dist^2),data=cars)
glm0 <- update(glm1, .~.-I(dist^2))

anova(glm1,glm0,test="Chisq")

cl <- makeCluster(8)
PBmodcomp(glm1, glm0, nsim=1000, cl=cl)
stopCluster(cl)

library(LiSciData)

data(ship,package='LiSciData')

par(mfrow=c(2,2))
boxplot(y~type, data=ship)
boxplot(y/months~type, data=ship)
plot(log(y+.5)~log(months), data=ship, pch=as.character(type))

ship$yearf   <- as.factor(ship$year)
ship$periodf <- as.factor(ship$period)
levels(ship$yearf)<-c("60-64","65-69","70-74","75-79")
levels(ship$periodf)<-c("60-74","75-79")

M1 <- glm(y~ type+yearf+periodf+offset(log(months)), data=ship,
          family=poisson(link=log))
M0 <- update(M1, .~.-periodf)

library(pbkrtest)
cl <- makeCluster(8)
PBmodcomp(M1, M0, nsim=1000, cl=cl)

anova(M1, M0, test="Chisq")
