#' Using `caracas` for calculating Jacobian.
#' 

library(caracas)


N <- 4
q <- 2

X  <- matrix_sym(N, q, "x")
y  <- paste0("y", seq_len(N)) %>% as_sym()
n  <- paste0("n", seq_len(N)) %>% as_sym()
mu <- paste0("mu", seq_len(N)) %>% as_sym()
beta <- paste0("beta", seq_len(q)) %>% as_sym()
X; y; n

#' Jacobian for composite function rss(mu(beta))
#' 
#' Setting one:
#' 
rss <- sum((y-mu)^2) ## Maps mu (R^N) into a scalar (R); Jacobian is 1 x N
mu_ <- X %*% beta ## Maps beta (R^q) into mu (R^N); Jacobian is N x q

rss
mu_

J.rss <- jacobian(rss, mu)
J.rss
dim(J.rss)

J.mu <- jacobian(mu_, beta)
J.mu
dim(J.mu)

J_ <- J.rss %*% J.mu
## Replace mu with X %*% beta
J1 <- subs(J_, mu, mu_)
J1 
dim(J1)

#' Jacobian for composite function rss(mu(beta))

#' Setting two

rss2 <- sum(sum((y - X %*% beta)^2))
J2   <- jacobian(rss2, beta)

#' Same result
(J2 - J1)  %>% simplify()






