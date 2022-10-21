## ----setup, include=FALSE-----------------------------------------------------
options(prompt = 'R> ', continue = '+ ')
knitr::opts_chunk$set(echo = TRUE, cache = FALSE, message = FALSE,
                      fig.height = 3, fig.width = 5, prompt = TRUE)
library(caracas)


## ---- echo=FALSE--------------------------------------------------------------
options(caracas.print.prettyascii = TRUE)
options("digits" = 3)


## -----------------------------------------------------------------------------
ngrp <- 3 # Number of groups
nspg <- 3 # Number of subjects per group
g <- seq_len(ngrp)
f <- factor(rep(g, each = nspg))
y <- as_sym(paste0("y", seq_along(f)))
X <- as_sym(model.matrix(~ f))


## -----------------------------------------------------------------------------
XtX <- t(X) %*% X
XtXinv <- inv(XtX) # Shorthand for solve_lin(XtX)


## -----------------------------------------------------------------------------
nicemat <- function(m, s){
    as_factor_list(paste0("1/", s), s*m)
}


## -----------------------------------------------------------------------------
Xty <- t(X) %*% y
beta_hat <- XtXinv %*% Xty
y_hat <- X %*% beta_hat


## -----------------------------------------------------------------------------
P <- X %*% XtXinv %*% t(X)


## -----------------------------------------------------------------------------
X2 <- as_sym(model.matrix(~ -1 + f))


## -----------------------------------------------------------------------------
X2ty <- t(X2) %*% y
beta2_hat <- inv(t(X2) %*% X2) %*% X2ty
y_hat <- X2 %*% beta2_hat


## -----------------------------------------------------------------------------
P2 <- X2 %*% inv(t(X2) %*% X2) %*% t(X2)


## -----------------------------------------------------------------------------
dat <- data.frame(a=c("a1", "a2", "a1", "a2"), b=c("b1", "b1", "b2", "b2"))
X <- as_sym(model.matrix(~ a + b, data=dat))
y <- c("y11", "y21", "y12", "y22") %>% as_sym()


## -----------------------------------------------------------------------------
Xty <- t(X) %*% y
XtX <- t(X) %*% X
XtXinv <- inv(XtX) # Shorthand for solve_lin(XtX)
beta_hat <- XtXinv %*% Xty
y_hat <- X %*% beta_hat


## ----logistic-----------------------------------------------------------------
N <- 4
q <- 2

X <- matrix_sym(N, q, "x")
y <- paste0("y", seq_len(N)) %>% as_sym()
n <- paste0("n", seq_len(N)) %>% as_sym()
theta <- paste0("theta", seq_len(N)) %>% as_sym()
beta <- paste0("beta", seq_len(q)) %>% as_sym()
X; y; n


## -----------------------------------------------------------------------------
l_theta  <- sum(y * log(theta) + (n-y) * log (1-theta))
l_theta


## -----------------------------------------------------------------------------
eta  <- X %*% beta
eta


## -----------------------------------------------------------------------------
l_beta <- subs(l_theta, theta, 1 / (1 + exp(-eta)))
l_beta


## -----------------------------------------------------------------------------
## Score and Hessian
score   <- der(l_beta, beta)  %>% matrify()
hessian <- der2(l_beta, beta) %>% matrify()

S <- score(l_beta, beta)
H <- hessian(l_beta, beta)



## -----------------------------------------------------------------------------
D_ <- data.frame(d = 1:N)
X_ <- model.matrix(~., D_)
colnames(X_) <- c("x1", "x2")
n_ <- rep(10, N)
y_ <- 1:N
X_; n_; y_


## -----------------------------------------------------------------------------
S_ <- S
S_ <- subs(S_, cbind(n, y, X), cbind(n_, y_, X_)) %>% simplify()

H_ <- H
H_ <- subs(H_, cbind(n, y, X), cbind(n_, y_, X_)) %>% simplify()


## -----------------------------------------------------------------------------
beta_ <- c(1, 1)
for (i in 1:5){
    S_i <- as_expr(subs(S_, beta, beta_))
    H_i <- as_expr(subs(H_, beta, beta_))  
    beta_ <- beta_ - solve(H_i, S_i)
}
beta_


## ----glm----------------------------------------------------------------------
a <- X_[, 2]
glm(cbind(y_, n_ - y_) ~ D_$d, family = binomial())

