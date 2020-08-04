library(geepack)
library(doBy)
library(faraway)

# glm with offset in rate model
data(dicentric)
dicentric$dosef <- factor(dicentric$doseamt)
dicentric$logcells <- log(dicentric$cells)
dicentric$logdoserate <- log(dicentric$doserate)


mod9 <- glm(ca~offset(logcells)+logdoserate*dosef,family=poisson, data=dicentric)
summary(mod9)

load_all("doBy")
LSmatrix(mod9,effect='dosef',at=list(logdoserate=log(0.1))) # fejl knyttet til offset()

load_all("doBy")
.get_linest_list(mod9, effect="dosef")


load_all("doBy")
M<-LSmatrix(mod9,effect='dosef',at=list(logcells=log(100),logdoserate=log(0.1))) # fejl knyttet til offset()

load_all("doBy")
g<-.get_linest_list(mod9, effect="dosef",at=list(logcells=log(100),logdoserate=log(0.1)))


mod9a <- glm(ca~dosef,family=poisson, data=dicentric)
summary(mod9a)
LSmeans(mod9a,effect='dosef') #ok

load_all("doBy")
mod9b <- glm(ca~offset(logcells)+dosef,family=poisson, data=dicentric)
#summary(mod9b)
LSmeans(mod9b,effect='dosef',at=list(logcells=log10(100))) # fejl knyttet til offset()

mod9c <- glm(ca~logdoserate*dosef,family=poisson, data=dicentric)
summary(mod9c)
LSmeans(mod9c,effect='dosef',at=list(doserate=0.1)) #tidligere fejl er forsvundet

mod9d <- glm(ca~offset(logcells),family=poisson, data=dicentric)
summary(mod9d)
predict(mod9)
LSmeans(mod9d,at=list(logcells=log(100))) # fejl knyttet til offset()

mod9e <- glm(ca~1,offset=logcells,family=poisson, data=dicentric)
summary(mod9e)
predict(mod9)
LSmeans(mod9e)  # ny fejlbesked men stadig knyttet til offset()
LSmeans(mod9e,at=list(logcells=log(100))) # ny fejlbesked men stadig knyttet til offset()
LSmeans(mod9e,at=list(offset=log(100))) # ny fejlbesked men stadig knyttet til offset()
LSmeans(mod9e,offset=log(100)) # ny fejlbesked men stadig knyttet til offset()
