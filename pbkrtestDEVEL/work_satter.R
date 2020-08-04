load_all("pbkrtest")

(fm0 <- lmer(Reaction ~ Days + I(Days^2) + (Days|Subject), data=sleepstudy))


devfun <- lmer(Reaction ~ Days + I(Days^2) + (Days|Subject), data=sleepstudy, devFunOnly=T)

theta <- getME(fm0, "theta")
sigma <- getME(fm0, "sigma")

devfun(c(theta))


get_devfun(fm0)





L <- rbind(c(0, 1, 0), 
           c(0, 0, 1))

microbenchmark::microbenchmark(
                    KRmodcomp(fm0, L), SATmodcomp(fm0, L))







fit2 <- update(fit1, .~. - time)
    L <- model2remat(fit1, fit2)



source("sh_satter.R");source("sh_contest.R")
get_sddf(fm, L)



data(beets)



(fm <- lmer2(Reaction ~ Days + + I(Days^2) + (Days|Subject), sleepstudy))

# Define 2-df contrast - since L has 2 (linearly independent) rows
# the F-test is on 2 (numerator) df:
L <- rbind(c(0, 1, 0), 
           c(0, 0, 1))

# Make the 2-df F-test of any effect of Days:1

## KRmodcomp(fm, L)
contestMD(fm, L, ddf="Satterthwaite")
contestMD(fm, L, ddf="Kenward-Roger")

fit1@Jac_list


model  <- fm0


## Fra lmer2
mc <- model@call
model <- eval.parent(mc)
## if(devFunOnly) return(model)
## Make an lmerModLmerTest object:
args <- as.list(mc)
args$devFunOnly <- TRUE

Call <- as.call(c(list(quote(lme4::lmer)), args[-1]))
devfun <- eval.parent(Call)

out <- list()
## Fra as_lmerModLT

tol <- 1e-6
is_reml <- getME(model, "is_REML")
## Coerce 'lme4-model' to 'lmerModLmerTest':
##res <- as(model, "lmerModLmerTest")                           ##
## Set relevant slots of the new model object:
res@sigma <- sigma(model)                                     ##     
res@vcov_beta <- as.matrix(vcov(model))                       ##    

out$sigma <- sigma(model)
out$vcov_beta <- as.matrix(vcov(model))                       ##

#varpar_opt <- unname(c(res@theta, res@sigma))
varpar_opt <- unname(c(getME(model, "theta"), getME(model, "sigma")))

## Compute Hessian:
  h <- numDeriv::hessian(func=devfun_vp, x=varpar_opt, devfun=devfun,
                         reml=is_reml)
  # Eigen decompose the Hessian:
  eig_h <- eigen(h, symmetric=TRUE)
  evals <- eig_h$values
  neg <- evals < -tol
  pos <- evals > tol
  zero <- evals > -tol & evals < tol
  if(sum(neg) > 0) { # negative eigenvalues
    eval_chr <- if(sum(neg) > 1) "eigenvalues" else "eigenvalue"
    evals_num <- paste(sprintf("%1.1e", evals[neg]), collapse = " ")
    warning(sprintf("Model failed to converge with %d negative %s: %s",
                    sum(neg), eval_chr, evals_num), call.=FALSE)
  }
  # Note: we warn about negative AND zero eigenvalues:
  if(sum(zero) > 0) { # some eigenvalues are zero
    eval_chr <- if(sum(zero) > 1) "eigenvalues" else "eigenvalue"
    evals_num <- paste(sprintf("%1.1e", evals[zero]), collapse = " ")
    warning(sprintf("Model may not have converged with %d %s close to zero: %s",
                    sum(zero), eval_chr, evals_num))
  }
  # Compute vcov(varpar):
  pos <- eig_h$values > tol
  q <- sum(pos)
  # Using the Moore-Penrose generalized inverse for h:
  h_inv <- with(eig_h, {
    vectors[, pos, drop=FALSE] %*% diag(1/values[pos], nrow=q) %*%
      t(vectors[, pos, drop=FALSE]) })


## res@vcov_varpar <- 2 * h_inv # vcov(varpar)
  out$vcov_vpar <- 2 * h_inv # vcov(varpar)
  # Compute Jacobian of cov(beta) for each varpar and save in list:
  Jac <- numDeriv::jacobian(func=get_covbeta, x=varpar_opt, devfun=devfun)

  res@Jac_list <- lapply(1:ncol(Jac), function(i)
    array(Jac[, i], dim=rep(length(res@beta), 2))) # k-list of jacobian matrices ##

out$Jac_list <- lapply(1:ncol(Jac), function(i)
    array(Jac[, i], dim=rep(length(getME(model, "beta")), 2))) # k-list of jacobian matrices ##

out

res



aux <- compute_auxillary(fm0)

source("sh_satter.R");source("sh_contest.R")
SATmodcomp(fm0, L)
KRmodcomp(fm0, L)


aux <- tmp$aux
q <- tmp$q
PtL <- tmp$PtL
d   <- tmp$d

grad_PLcov <- lapply(1:q, function(m) {
    vapply(aux$Jac_list, function(J)
        qform(PtL[m, ], J), numeric(1L))
})

nu_m <- vapply(1:q, function(m) {
    2*(d[m])^2 / qform(grad_PLcov[[m]], aux$vcov_varpar)
}, numeric(1L)) # 2D_m^2 / g'Ag



grad_PLcov[[m]]
aux$vcov_varpar

qform(grad_PLcov[[m]], aux$vcov_varpar)
























