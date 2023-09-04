## Fit uafhængighedsmodel i 2 x 2 tabel.

library(caracas)
options(caracas.print.prettyascii = TRUE)
options("digits" = 3)

load_all("../../caracas")


y_ <- c("y11", "y21", "y12", "y22")
y  <- as_sym(y_)
tab <- data.frame(r=c(1,2,1,2), c=c(1,1,2,2), y=y_)
tt  <- xtabs(~r+c, data=tab)
tt[] <- y_


## Under independece, the general form for p_{ij} is p_{ij}=u a_i * b_j$

def_sym(u, r1, r2, s1, s2, k0, k1, k2)

## For identifiability, use only u, r2, s2
p <- as_sym(c("u", "u*r2", "u*s2", "u*r2*s2"))
l  <- sum(y * log(p))
L  <- -l + k0 * (sum(p) - 1) 
vars <- list(u, r2, s2, k0)
gL <- der(L, vars)
sol <- solve_sys(to_vector(gL), vars)[[1]]
sol

## For identifiability, restrict that r1=s1=1
p <- as_sym(c("u*r1*s1", "u*r2*s1", "u*r1*s2", "u*r2*s2")) 
l  <- sum(y * log(p))
L  <- -l + k0 * (sum(p) - 1) + k1 * (r1-1) + k2 * (s1-2)
vars <- list(u, r1, r2, s1, s2, k0, k1, k2)
gL <- der(L, vars)
sol <- solve_sys(to_vector(gL), vars)
sol

## Work on log-scale (parms change interpretation)
## For identifiability, restrict that r1=s1=0
log.p  <- as_sym(c("u+r1+s1", "u+r2+s1", "u+r1+s2", "u+r2+s2")) 
l  <- sum(y * log.p)
L  <- -l + k0 * (sum(exp(log.p)) - 1) + k1 * (r1-0) + k2 * (s1-0)
vars <- list(u, r1, r2, s1, s2, k0, k1, k2)
gL <- der(L, vars)
sol <- solve_sys(to_vector(gL), vars)
sol

## Work on log-scale (parms change interpretation)
## For identifiability, restrict that r1+r2=s1+s2=0
log.p  <- as_sym(c("u+r1+s1", "u+r2+s1", "u+r1+s2", "u+r2+s2")) 
l  <- sum(y * log.p)
L  <- -l + k0 * (sum(exp(log.p)) - 1) + k1 * (r1+r2-0) + k2 * (s1+s2-0)
vars <- list(u, r1, r2, s1, s2, k0, k1, k2)
gL <- der(L, vars)
sol <- solve_sys(to_vector(gL), vars)
sol







sol$u
sol$u * sol$r2
sol$u * sol$s2
sol$u * sol$r2 * sol$s2







k_ <- sol$k
r1_ <- sol$r1
r2_ <- sol$r2
s1_ <- sol$s1
s2_ <- sol$s2

y_ <- c(3, 7, 11, 13)
subs(k_, c("y11", "y21", "y12", "y22"), y_)
subs(r2_, c("y1", "y2", "y3", "y4"), y_)
subs(s1_, c("y1", "y2", "y3", "y4"), y_)


r <- vector_sym(2, "r")
s <- vector_sym(2, "s")




p <- as_sym(paste0("p", 1:3))
y <- as_sym(paste0("y", 1:3))
def_sym(a) 
l <- sum(y * log(p))
L <- -l + a * (sum(p) - 1); L

gL <- der(L, list(p, a))
sols <- solve_sys(gL, list(p, a)) # takes an RHS argument which defaults to zero
sols


library(caracas)

def_sym_vec(c("y_i", "n_i", "theta_i"))

li <- n_i * log(theta_i) + (n_i - y_i) * log(1 - theta_i)

N <- 2
xi <- vector_sym(N, "x_i")
b  <- vector_sym(N, "b")
sum(xi * b)
eta_i <- t(xi) %*% b
theta_i <- (1 + exp(-eta_i))^-1

der(eta_i, b)
der(theta_i, b)

listify(eta_i)[[1]]


1 / eta_i



theta_i = 1 / (1 + exp(-eta_i))

theta_i = 1 / (1 + exp(-eta_i))



theta <- 1 / (1+exp(-X %*% beta))


ngrp <- 2 # Number of groups
nspg <- 2 # Number of subjects per group
g <- seq_len(ngrp)
f <- factor(rep(g, each = nspg))
## X <- as_sym(model.matrix(~ f))


N <- 4
q <- 2

X <- matrix_sym(N, q, "x")
y  <- paste0("y", 1:N) %>% as_sym()
n  <- paste0("n", 1:N) %>% as_sym()
X;y;n

## Binomial logL
theta <- paste0("theta", 1:N) %>% as_sym()
l_theta  <- sum(y * log(theta) + (n-y) * log (1-theta))
l_theta

## Linear predictor
beta <- paste0("beta", seq_len(q)) %>% as_sym()
eta  <- X %*% beta
eta

dim(score)
dim(hessian)

X_ <- model.matrix(~factor(c(1, 1, 2, 2)))
n_ <- rep(10, 4)
y_ <- c(3, 4, 7, 8)
X_; n_; y_




score_ <- score
score_ <- subs(score_, n, n_)
score_ <- subs(score_, y, y_)
score_ <- subs(score_, X, X_) 

hessian_ <- hessian
hessian_ <- subs(hessian_, n, n_)
hessian_ <- subs(hessian_, y, y_)
hessian_ <- subs(hessian_, X, X_)

beta_ <- c(1,1)
for (i in 1:10){
    score_i   <- as_expr(subs_vec(score_, beta, beta_))
    hessian_i <- as_expr(subs_vec(hessian_, beta, beta_))
    
    beta_ <- beta_ - solve(hessian_i) %*% score_i
    print(beta_)
}

glm(cbind(y_, n_ - y_) ~ f, family = binomial())


















score2 <- score
score2 <- subs2(score2, as_character_matrix(n), n_)
score2 <- subs2(score2, as_character_matrix(y), y_)
score2 <- subs2(score2, as_character_matrix(X), X_)

hessian2 <- hessian
hessian2 <- subs2(hessian2, as_character_matrix(n), n_)
hessian2 <- subs2(hessian2, as_character_matrix(y), y_)
hessian2 <- subs2(hessian2, as_character_matrix(X), X_)

beta_ <- c(1,1)

score2_ <- subs2(score2, as_character_matrix(beta), beta_)
hessian2_ <- subs2(hessian2, as_character_matrix(beta), beta_)

beta_ <- beta_ - solve(eval(hessian2_)) %*% eval(score2_)





subs2 <- function(s, x, v){

    if (!inherits(s, c("expression", "caracas_symbol")))
        stop("s must be R expression or caracas_symbol\b")
    
    if(inherits(s, "caracas_symbol"))
        s <- as_expr(s)

    s <- s[[1]]

    v <- as.list(v)
    names(v) <- x
    
    s2 <- do.call("substitute", list(s, v))
    return(as.expression(s2))
}








eta <- as_sym(paste0("eta", 1:N))




## Connect theta to linear predictor


theta
der_theta_eta <- der(theta, eta)
der_theta_eta


der_eta_beta <- der(eta, beta)
der_eta_beta ## Transp to get J
dim(der_eta_beta)
dim(der_theta)

der_theta_eta %*% t(der_eta_beta)

eta_ <- paste0("eta", 1:N)
as_character_matrix(eta)

der_theta_eta
subs_vec(der_theta_eta, eta_, eta)









t(der(theta2, beta))

