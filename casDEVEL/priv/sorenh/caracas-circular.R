install_github("r-cas/caracas")
library(caracas)

#' ## Cholesky
#' Write x ~ AR(1) as system of regressions. Gives
#' e = L x

d  <- 5

## AR(1) as regressions
L.ar <- diag_("1", d)
L.ar[cbind(2:d, 1:(d-1))] <- "-a"
L.ar

## Wrap around
L2.loop <- L.ar
L2.loop[1, d] <- "-a"

#' Variance of x (assuming Var(e)=I) under AR(1)
L.ari <- inv(L.ar)
L.ari
V1 <- L.ari %*% t(L.ari)
V1 ## Variance
K1 <- t(L.ar) %*% L.ar
K1 ## Concentration

#' Variance of x (assuming Var(e)=I) under "wrap"

L2.loopi <- inv(L2.loop)
L2.loopi
V2 <- L2.loopi %*% t(L2.loopi)
V2 ## Variance
K2 <- t(L2.loop) %*% L2.loop
K2 ## Concentration


## How different are the log-likelihoods?

## Wishart matrix
S <- nxn_matrix(d, "s", sym=T)    

logL.ar   <- det(K1) - sum(diag(K1 %*% S))
logL.wrap <- det(K2) - sum(diag(K2 %*% S))

logL.ar - logL.wrap

## #' Simplification of L2.loopi and V2: get rid of denominator
## L2.loopi

den <- as_sym2(expression(1-a^d), list(d=d))
S2i <- L2.loopi * den 

V2.prop <- S2i %*% t(S2i)
V2.prop

## Sanity check
(V2.prop / (den^2) - V2)  %>% simplify()




## den <- as_sym2("1-a^d", list(d=d))


 
## Alternative
## K1. <- inv(V1) %>% simplify()
## max(abs(as_expr(K1 - K1.)))
## This inversion takes time:
## K2. <- inv(V2)  %>% simplify()  
## max(abs(as_expr(K2 - K2.)))










