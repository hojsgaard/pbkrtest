# extract_lme_params <- function(fit) {
#   if (!inherits(fit, "lme")) {
#     stop("Model object must be of class 'lme'")
#   }
# 
#   # Varianskomponenter (inkl. residual)
#   vc <- VarCorr(fit)
#   var_names <- rownames(vc)
#   variances <- as.numeric(vc[, "Variance"])
#   names(variances) <- var_names
# 
#   # Korrelationsparametre (fx AR(1), compound symmetry ...)
#   cor_struct <- fit$modelStruct$corStruct
#   if (!is.null(cor_struct)) {
#     cor_params <- coef(cor_struct, unconstrained = FALSE)
#   } else {
#     cor_params <- NULL
#   }
# 
#   # Returnér resultat som liste
#   list(
#     variances = variances,
#     correlation = cor_params
#   )
# }
# 
# 
# extract_gls_params <- function(model) {
# #  stopifnot(inherits(model, "gls"))
#   
#   out <- list()
#   
#   # Residual std dev and variance
#   out$residual_sd <- model$sigma
#   out$residual_var <- model$sigma^2
#   
#   # Correlation parameters (if any)
#   if (!is.null(model$modelStruct$corStruct)) {
#     out$correlation <- coef(model$modelStruct$corStruct, unconstrained = FALSE)
#   }
#   
#   # Variance structure (heteroscedasticity)
#   if (!is.null(model$modelStruct$varStruct)) {
#     out$variance_structure <- coef(model$modelStruct$varStruct, unconstrained = FALSE)
#   }
#   
#   return(out)
# }
# 
# 
# 
# 
# 
# 
# W <- get_lme_covariance_matrix(lgm2)


### Our code

```{r}
Vfun2 <- function(r.free, nb, bs){
    r     <- tanh(r.free)
    V.b   <- toeplitz(c(r^(0:(bs-1))))    
    I     <- diag(1, nb)
    V.all <- kronecker(I, V.b)
    return(V.all)
}
```

```{r}
worker <- function(y, X, V) {
    Lt <- chol(V)
    Vi <- chol2inv(Lt)
    ld_V <- 2*sum(log(diag(Lt)))
    n   <- nrow(X)    
    b   <- solve(t(X) %*% Vi  %*% X, t(X) %*% Vi %*% y) 
    res <- (y - X %*% b)
    Q   <- t(res) %*% Vi %*% res
    v   <- Q / n    
    return(list(b=b, n=n, v=v, ld_V=ld_V, Q=Q))
}

logL.1 <- function(parm, X, y, n.block, block.size) {
    ## v is estimated from residual sums of squares
    r.free <- parm[1] ## rho
    V   <- Vfun2(r.free, n.block, block.size)
    aux <- worker(y, X, V)        
    obj <- as.numeric(-0.5*(aux$n * log(aux$v) + aux$ld_V + aux$Q / aux$v))        
    obj <- -obj
    attributes(obj) <- list(aux=aux)
    return(obj)
}

logL.2 <- function(parm, X, y, n.block, block.size) {
    ## v is multiplied onto V
    r.free   <- parm[1] ## rho
    v.free   <- parm[2] ## sigma^2
    v   <- exp(v.free)
    V   <- v * Vfun2(r.free, n.block, block.size)
    aux <- worker(y, X, V)    
    obj <- as.numeric(-0.5*(aux$ld_V + aux$Q))
    obj <- -obj
    attributes(obj) <- list(aux=aux)
    return(obj) 
}

logL.3 <- function(parm, X, y, n.block, block.size) {
    ## v is estimated as separate parameter 
    r.free <- parm[1] ## rho 
    v.free <- parm[2] ## sigma^2
    v   <- exp(v.free)
    V   <- Vfun2(r.free, n.block, block.size)
    aux <- worker(y, X, V)  
    obj <- as.numeric(-0.5*(aux$n * log(v) + aux$ld_V + aux$Q / v))
    obj <- -obj
    attributes(obj) <- list(aux=aux)
    return(obj)
}
```





```{r}
y   <- getME(lmm1, "y")
X   <- getME(lmm1, "X")
Z   <- getME(lmm1, "Z")

n.block <- unique(dat$Subject) |> length()
block.size <- 4
```


```{r}
args <- list(X=X, y=y, n.block=n.block, block.size=block.size)
logL.1. <- set_default(logL.1, args)
logL.2. <- set_default(logL.2, args)
logL.3. <- set_default(logL.3, args)

par.1 <- set_free(0, list(cor=1, var=2))
par.2 <- set_free(c(0, 0), list(cor=1, var=2))

opt.1 <- optim(par.1, logL.1.)
opt.2 <- optim(par.2, logL.2.)
opt.3 <- optim(par.2, logL.3.)

opt.1$par
opt.2$par
opt.3$par

opt.1$par |> set_free(list(cor=1, var=2))
opt.2$par |> set_free(list(cor=1, var=2))
opt.3$par |> set_free(list(cor=1, var=2))

logL.1.(opt.1$par)
logL.2.(opt.2$par)
logL.3.(opt.3$par)
```





```{r}
# def_sym(sigma)
# size <- 4
# def_sym(r)
# 
# II <- diag_(1, length(unique(dat$Subject)))
# 
# R <- kronecker(II, make_ar1(r, size))
# G_  <- diag_("tau^2", ncol(Z))
# R_  <- diag_("sigma^2", nrow(Z))
# Z_  <- as_sym(Z)
# ZGZt <- Z_ %*% G_ %*% t(Z_)
# V <- ZGZt + sigma^2 * R
# Vi <- inv_woodbury(sigma^2 * R, Z_, G_, method="ge")
# 
# 
# 
# Rprof()
# for (i in 1:250) {shoptim(c(r=0, sigma=1, tau=4), logL6)}
# Rprof(NULL)
# summaryRprof()
# 
# shoptim(c(r=0, sigma=1, tau=4), logL1)
# ## shoptim(c(r=0, sigma=1, tau=4), logL2)
# ## shoptim(c(r=0, sigma=1, tau=4), logL3)
# shoptim(c(r=0, sigma=1, tau=4), logL4)
# shoptim(c(r=0, sigma=1, tau=4), logL5)
# shoptim(c(r=0, sigma=1, tau=4), logL6)
# 
# microbenchmark::microbenchmark(
#                     shoptim(c(r=0, sigma=1, tau=4), logL1),
#                     ## shoptim(c(r=0, sigma=1, tau=4), logL2),
#                     ## shoptim(c(r=0, sigma=1, tau=4), logL3),
#                     shoptim(c(r=0, sigma=1, tau=4), logL4),
#                     shoptim(c(r=0, sigma=1, tau=4), logL5),
#                     shoptim(c(r=0, sigma=1, tau=4), logL6),
#                     times=3
#                 )
# 

```











# SCRAP below here

All functions here rely on that there exists a V (symbolic) and (X, y) numeric

```{r}
logL1 <- function(parm){
    Vi.   <- as_expr(subs(Vi, parm))
    bb    <- solve(t(X) %*% Vi. %*% X, t(X) %*% Vi. %*% y)
    res   <- y - X %*% bb
    Q     <- t(res) %*% Vi. %*% res
    out <- as.numeric(-0.5*(-log(det(Vi.)) + Q))
    -out
}

logL2 <- function(parm){
    Vi.   <- as(Vi, "function")(parm)
    bb    <- solve(t(X) %*% Vi. %*% X, t(X) %*% Vi. %*% y)
    res   <- y - X %*% bb
    Q     <- t(res) %*% Vi. %*% res
    out <- as.numeric(-0.5*(-log(det(Vi.)) + Q))
    -out
}

logL3 <- function(parm){
    Vi.   <- do.call(as_func(Vi), as.list(parm))
    bb    <- solve(t(X) %*% Vi. %*% X, t(X) %*% Vi. %*% y)
    res   <- y - X %*% bb
    Q     <- t(res) %*% Vi. %*% res
    out <- as.numeric(-0.5*(-log(det(Vi.)) + Q))
    -out
}

logL4 <- function(parm){
    Vi.   <- solve(do.call(as_func(V), as.list(parm)))
    bb    <- solve(t(X) %*% Vi. %*% X, t(X) %*% Vi. %*% y)
    res   <- y - X %*% bb
    Q     <- t(res) %*% Vi. %*% res
    out <- as.numeric(-0.5*(-log(det(Vi.)) + Q))
    -out
}

logL5 <- function(parm){
    Vi.   <- solve(do.call(as_func(V), as.list(parm)))
    XtVi <- t(X) %*% Vi.
    bb    <- solve(XtVi %*% X, XtVi %*% y)
    res   <- y - X %*% bb
    Q     <- t.default(res) %*% Vi. %*% res
    out <- as.numeric(-0.5*(-log(det(Vi.)) + Q))
    -out
}


logL6 <- function(parm){
  Vfun <- as_func(V)
    Vi.   <- solve(do.call(Vfun, as.list(parm)))
    XtVi <- t.default(X) %*% Vi.
    bb    <- solve(XtVi %*% X, XtVi %*% y)
    res   <- y - X %*% bb
    Q     <- t.default(res) %*% Vi. %*% res
    out <- as.numeric(-0.5*(-log(det(Vi.)) + Q))
    -out
}


logL7 <- function(parm){
    V.   <- do.call(as_func(V), as.list(parm))
    Vi.  <- chol2inv(chol(as(V., "sparseMatrix")))
    XtVi <- t(X) %*% Vi.
    bb    <- solve(XtVi %*% X, XtVi %*% y)
    res   <- y - X %*% bb
    Q     <- t(res) %*% Vi. %*% res
    out <- as.numeric(-0.5*(log(det(V.)) + Q))
    -out
}



logL1 <- function(parm){
    parm[1] <- exp(parm[1])
    parm[2] <- exp(parm[2])
    Vi. <- solve(as_expr(subs(V, parm)))
    bb    <- solve(t(X) %*% Vi. %*% X, t(X) %*% Vi. %*% y)
    res   <- y - X %*% bb
    Q     <- t(res) %*% Vi. %*% res
    out <- as.numeric(-0.5*(-log(det(Vi.)) + Q))
    -out
}


```





library(caracas)

# Example symbolic matrices
M1 <- as_sym(matrix(c("a", "b", "b", "a"), 2, 2))
M2 <- as_sym(matrix(c("x", "y", "y", "x"), 2, 2))
mat_list <- list(M1, M2)

# Convert each caracas matrix to character matrix
char_list <- lapply(mat_list, as_character_matrix)

# Function to create block diagonal character matrix
block_diag_char_matrix <- function(char_matrices) {
  total_rows <- sum(sapply(char_matrices, nrow))
  total_cols <- sum(sapply(char_matrices, ncol))
  
  # Create empty matrix
  out <- matrix("0", nrow = total_rows, ncol = total_cols)
  
  row_pos <- 1
  col_pos <- 1
  
  for (mat in char_matrices) {
    nr <- nrow(mat)
    nc <- ncol(mat)
    out[row_pos:(row_pos + nr - 1), col_pos:(col_pos + nc - 1)] <- mat
    row_pos <- row_pos + nr
    col_pos <- col_pos + nc
  }
  
  return(out)
}

# Build character block diagonal matrix
blk_char <- block_diag_char_matrix(char_list)

# Convert to caracas symbolic matrix
blk_sym <- as_sym(blk_char)

# Result
blk_sym


bdia





















