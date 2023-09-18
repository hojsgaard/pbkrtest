### R code from vignette source 'shoes.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: shoes.Rnw:15-17
###################################################
data(shoes, package="MASS")
shoes


###################################################
### code chunk number 2: shoes.Rnw:30-31
###################################################
with(shoes, t.test(A, B))


###################################################
### code chunk number 3: shoes.Rnw:38-39
###################################################
with(shoes, t.test(A, B, paired=T))


