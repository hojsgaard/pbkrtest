### R code from vignette source 'birthweight-practical.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: birthweight-practical.Rnw:135-136
###################################################
options("width"=80, "prompt"=" ", continue="  ")


###################################################
### code chunk number 2: birthweight-practical.Rnw:140-146
###################################################
xyplot2 <- function (x, data, ...)
{
    cl <- match.call()
    cl[[1]] <- as.name("xyplot")
    print(eval(cl))
}


###################################################
### code chunk number 3: birthweight-practical.Rnw:195-200
###################################################
library(Epi)
data(births)
head(births)
dim(births)
sapply(births, class)


###################################################
### code chunk number 4: birthweight-practical.Rnw:212-213
###################################################
hist(births$bweight)


