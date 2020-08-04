### R code from vignette source 'estimable.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: estimable.Rnw:43-46
###################################################
ff <- factor(rep(1:3, each=2))
X  <- cbind(rep(1,6),model.matrix(~0+ff)); X
y  <- 1:6


###################################################
### code chunk number 2: estimable.Rnw:50-52
###################################################
S <- svd(X)
lapply(S, zapsmall)


###################################################
### code chunk number 3: estimable.Rnw:57-59
###################################################
B <- S$v[, S$d/max(S$d) < 1e-10, drop=F]
(B <- as.numeric(B/max(B)))


###################################################
### code chunk number 4: estimable.Rnw:66-68
###################################################
lam <- c(1, 1, 0, 0)
sum( lam*B ) # Orthogonal


###################################################
### code chunk number 5: estimable.Rnw:72-74
###################################################
(b.hat <- as.numeric(MASS::ginv(t(X)%*%X) %*% t(X) %*% y))
sum( lam * b.hat )


###################################################
### code chunk number 6: estimable.Rnw:79-83
###################################################
X2 <- X
X2[,3]<-0
(b.hat2 <- as.numeric(MASS::ginv(t(X2)%*%X2) %*% t(X2) %*% y))
sum( lam * b.hat2 )


###################################################
### code chunk number 7: estimable.Rnw:88-91
###################################################
lam2 <- c(1, 0, 0, 0)
sum( lam2 * b.hat )
sum( lam2 * b.hat2 )


