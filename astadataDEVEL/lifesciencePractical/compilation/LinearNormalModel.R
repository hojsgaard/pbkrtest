### R code from vignette source 'LinearNormalModel.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: LinearNormalModel.Rnw:135-136
###################################################
options("width"=80, "prompt"=" ", continue="  ")


###################################################
### code chunk number 2: LinearNormalModel.Rnw:140-146
###################################################
xyplot2 <- function (x, data, ...)
{
    cl <- match.call()
    cl[[1]] <- as.name("xyplot")
    print(eval(cl))
}


###################################################
### code chunk number 3: LinearNormalModel.Rnw:195-197
###################################################
data(dietsup, package="LiSciData")
head(dietsup)


###################################################
### code chunk number 4: uu
###################################################
m1 <-  glm(y ~x + I(x^2),data=dietsup)


###################################################
### code chunk number 5: LinearNormalModel.Rnw:246-247
###################################################
pf(99.3,1,7,lower.tail=FALSE)


###################################################
### code chunk number 6: LinearNormalModel.Rnw:254-256
###################################################
set.seed(998)
y  <- rf(n=1000,1,7)


###################################################
### code chunk number 7: LinearNormalModel.Rnw:259-260
###################################################
q95 <- qf(0.95,1,7)


###################################################
### code chunk number 8: LinearNormalModel.Rnw:263-266
###################################################
hist(y,probability=TRUE)
lines(density(y))
abline(v=q95)


###################################################
### code chunk number 9: gg
###################################################
data(gestation, package="LiSciData")
gestation<- reshape(gestation,direction='long',
            varying=list(age=c('boys.age','girls.age'),
            weight=c('boys.weight','girls.weight')),
            v.names=c('age','weight'),
            timevar='sex',times=c('boy','girl'))
gestation$sex<-factor(gestation$sex)
head(gestation)


###################################################
### code chunk number 10: LinearNormalModel.Rnw:339-341
###################################################
m1.alter <- glm(weight ~ sex + age - 1, data = gestation)
coef(summary(m1.alter))[,c(1,2)]


###################################################
### code chunk number 11: mm
###################################################
m1 <- glm(weight~sex+age-1,data=gestation)
par(mfrow=c(2,2))
plot(m1,pch=c(16,1)[gestation$sex],cex=2)


###################################################
### code chunk number 12: LinearNormalModel.Rnw:394-399
###################################################
data(sexism,package='LiSciData')
sexism <- transform(sexism, score = sexism)
sexism <- transform(sexism, type = factor(type))
sexism <- transform(sexism, gender.type = interaction(gender,type))
head(sexism)


###################################################
### code chunk number 13: LinearNormalModel.Rnw:414-415
###################################################
m1 <- glm(score ~ gender + type+ gender:type,data=sexism)


###################################################
### code chunk number 14: LinearNormalModel.Rnw:424-426
###################################################
sexism <- transform(sexism,fit=predict(m1))
coplot(fit ~ type | gender, data = sexism ,panel = panel.smooth, pch = c(18))


###################################################
### code chunk number 15: LinearNormalModel.Rnw:431-435
###################################################
library(lattice)
print(
xyplot(fit~type|gender,data=sexism,type='l')
)


###################################################
### code chunk number 16: LinearNormalModel.Rnw:438-441
###################################################
print(
xyplot(fit~type,groups=gender,data=sexism,type='l',auto.key=TRUE)
)


###################################################
### code chunk number 17: LinearNormalModel.Rnw:467-471
###################################################
old<-options()$contrasts
options(contrasts=c('contr.sum','contr.poly'))
m1L<- glm(score ~ gender + type+ gender:type,data=sexism)
drop1(m1L,.~.,test='F')


###################################################
### code chunk number 18: LinearNormalModel.Rnw:480-481
###################################################
options(old)


###################################################
### code chunk number 19: LinearNormalModel.Rnw:494-496
###################################################
data(bactsucrose, package="LiSciData")
head(bactsucrose)


