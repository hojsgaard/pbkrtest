library(magrittr)
y <- cars$dist
X <- model.matrix(~speed+I(speed^2), data=cars)

mm <- lm(dist ~ speed + I(speed^2), data=cars) %>% summary

beta <- c(1,1,1)
s    <- 1

X %*% beta


devfun <- function(parm){
    s <- parm[length(parm)]
    beta <- parm[1:(length(parm)-1)]
    -sum(dnorm(x=y, mean=X %*% beta, sd=s, log=T))    
}

op <- optim(c(2,1,0,10), devfun)$par

H <- hessian(devfun, x=op)
H

Hi <- H[1:3, 1:3] %>% solve

Hi  <- H %>% solve


%>% "["(1:3, 1:3)

vcov(mm)
eps <- 1e-6
L <- matrix(c(0,0,1), nr=1)
beta <- coef(mm)
vcov_beta <- vcov(mm)

VLbeta <- L %*% vcov_beta %*% t(L) 
VLbeta

eig_VLbeta <- eigen(VLbeta)
P   <- eig_VLbeta$vectors
d   <- eig_VLbeta$values
tol <- max(eps * d[1], 0)
pos <- d > tol
qq   <- sum(pos) # rank(VLbeta)


PtL <- crossprod(P, L)[1:qq,, drop=FALSE]
## print(PtL)
    
t2 <- drop(PtL %*% beta)^2 / d[1:qq]
Fvalue <- sum(t2) / qq

grad_PLcov <- lapply(1:qq, function(m) {
    vapply(Jac_list, function(J)
        qform(PtL[m, ], J), numeric(1L))
})







beta <- getME(model, "beta")
aux  <- compute_auxillary(model)
    VLbeta <- L %*% aux$vcov_beta %*% t(L) # Var(contrast) = Var(Lbeta)


