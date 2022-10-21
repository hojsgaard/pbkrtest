library(Ryacas)

M <- "{{1,2},{2,1}}"
as_r(M) # Hvorfor ikke numeric?

e_val <- M %>% y_fn("EigenValues")  %>% yac_str()
yac_str(paste0("EigenVectors(", M, ",", e_val, ")"))

x <- ysym("x")
M <- "{{1,1},{2,x}}"
e_val <- M %>% y_fn("EigenValues")  %>% yac_str()
yac_str(paste0("EigenVectors(", M, ",", e_val, ")"))




#' ## Friday afternoon stats

library(Ryacas)
#' Write e=Lx for different Ls.

#' Plain AR(1)
L1 <- "{{ 1,  0,  0,  0},
        {-a,  1,  0,  0},
        { 0, -a,  1,  0},
        { 0,  0, -a,  1}}"
L1 <- ysym(L1)

#' Wrap around
L2 <- L1
L2[1, 4] <- "-a"
L2

#' Variance (assuming Var(e)=I)
L1i <- solve(L1)
L1i

V1 <- L1i %*% t(L1i)
V1
## K1 <- solve(V1) %>% simplify()
K1 <- t(L1) %*% L1
K1

L2i <- solve(L2)
L2i

V2 <- L2i %*% t(L2i)
V2
## K2 <- solve(V2)  %>% simplify()
K2 <- t(L2) %*% L2
K2

#' Simplification of L2i
L2i
f <- ysym("1-a^4")
S2i <- L2i * f
S2i <- S2i  %>% simplify() %>% simplify()
S2i
#' Now
## S2i / f == L2i
W2 <- S2i %*% t(S2i)
#' Moreover
##W2 / (f * f) == V2


