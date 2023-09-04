library(caracas)
nr <- 2
nc <- 2
y <- matrix_sym(nr, nc, "y")
dim(y) <- c(nr*nc, 1)
y

y_ <- c("y_11", "y_21", "y_12", "y_22")
y  <- as_sym(y_)

dat <- expand.grid(r=factor(1:nr), s=factor(1:nc))
X <- model.matrix(~r+s, data=dat) |> as_sym()
b <- vector_sym(ncol(X), "b")
mu <- exp(X %*% b)


N <- nrow(X)
q <- ncol(X)
## X <- matrix_sym(N, q, "x")
## n <- vector_sym(N, "n")
y <- vector_sym(N, "y")
m <- vector_sym(N, "m")
s <- vector_sym(N, "s")
b <- vector_sym(q, "b")


## log-likelihood as function of m
logLm  <- sum(y * log(m))
## log-likelihood as function of s

m_ <- exp(s)
logLs <- subs(logLm, m, m_)


## linear predictor as function of regression coefficients:
s_  <- X %*% b
## log-Likelihood as function of regression coefficients:
logLb <- subs(logLs, s, s_)



logLb_ <- subs(logLb, y, y_val)

logLb_func <- as_func(logLb_, vec_arg = TRUE)

optim(c(0.33, 1.2, 1.4), logLb_func,
      method="BFGS",
      control = list(fnscale = -1), hessian = TRUE)


XX <- as.data.frame(as_expr(X))
y_val <- c(2,4,5,19)
ddd <- cbind(y=as_expr(y_val), XX)
glm(y~-1+V1+V2+V3, family=poisson, data=ddd)







