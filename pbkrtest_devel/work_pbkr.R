
These two examples work in 0.5.4 but not in the latest CRAN version 0.5.5.

library(lme4)
library(pbkrtest)

gm1 <- glmer(cbind(incidence, size-incidence) ~ period + (1 | herd), data=cbpp, family=binomial)
gm0 <- glmer(cbind(incidence, size-incidence) ~ 1 + (1 | herd), data=cbpp, family=binomial)

PBmodcomp(gm1, gm0, nsim=5)
PBmodcomp(gm1, gm0, nsim=50, cl=1)
# Error in `[[<-`(`*tmp*`, i, value = value) : no such index at level 1
 
gm1 <- glmer(incidence ~ period + (1 | herd), data=cbpp, family=poisson)
gm0 <- glmer(incidence ~ 1 + (1 | herd), data=cbpp, family=poisson)

PBmodcomp(gm1, gm0, nsim=5)
PBmodcomp(gm1, gm0, nsim=50, cl=1)

# Error in glmer(formula = incidence ~ (1 | herd), data = dd, family = poisson,  : 
#   unused argument (REML = FALSE)








load_all()

data(sleepstudy, package = "lme4")
sleepstudy$Days2 <- sleepstudy$Days^2


extract_lmer_cov_components <- function(model) {
  stopifnot("lmerMod" %in% class(model))
  
  # Extract the theta parameters
  theta <- getME(model, "theta")
  
  # Extract the sparse structure (unscaled)
  Lambdat <- getME(model, "Lambdat")
  
  # Determine how many components (should match length of theta)
  n_components <- length(theta)
  
  # Lambdat is a dgCMatrix (sparse)
  # It has rows = # random effects, columns = # random effects
  # But its nonzeros are ordered to match theta scaling
  # We need to figure out which columns correspond to which theta
  
  # lme4 encodes theta scaling via lower-triangular factors
  # It's usually block-diagonal per random effect grouping
  # The mapping is in the lower-level ST structure
  ## reTrms <- getME(model, "reTrms")
  ## STlist <- reTrms$ST

STlist <- getME(model, "ST")
  
  # Build list of component matrices
  Lambda_k_list <- list()
  theta_labels <- c()
  
  col_start <- 1
  for (i in seq_along(STlist)) {
    ST_block <- STlist[[i]]
    dims <- dim(ST_block)
    n_elem <- length(ST_block[lower.tri(ST_block, diag=TRUE)])
    
    for (j in 1:n_elem) {
      # Create a matrix the same size as Lambdat with zeros
      comp_matrix <- Lambdat
      comp_matrix@x <- rep(0, length(Lambdat@x))
      
      # Fill in only this block's part
      idx <- col_start
      comp_matrix[, idx] <- Lambdat[, idx]
      
      # Add to list
      Lambda_k_list[[length(Lambda_k_list) + 1]] <- comp_matrix
      theta_labels <- c(theta_labels, paste0("Group", i, "_Theta", j))
      
      col_start <- col_start + 1
    }
  }
  
  if (length(Lambda_k_list) != length(theta)) {
    warning("Number of extracted component matrices does not match number of theta parameters!")
  }
  
  names(Lambda_k_list) <- theta_labels
  
  # Return as list
  return(list(
    theta = theta,
    Lambda_components = Lambda_k_list
  ))
}

library(lme4)
m <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

result <- extract_lmer_cov_components(m)
result$theta
# -> numeric vector of estimated variance parameters

g <- result$Lambda_components
# -> list of sparse matrices corresponding to each parameter

lapply(g, dim)

l <- get_SigmaG(m)
lapply(l$G, dim)







# LMER example (lme4)
lmer_fit1 <- lmer(Reaction ~ Days + Days2 + (1 | Subject),
                        data = sleepstudy, REML = FALSE)
lmer_fit0 <- update(lmer_fit1, . ~ . - Days2)


lmer_fit1 <- lmer(Reaction ~ Days  + (1 | Subject),
                        data = sleepstudy, REML = FALSE)
lmer_fit0 <- update(lmer_fit1, . ~ . - Days)

set.seed(42)

res1 <- PBmodcomp(lmer_fit1, lmer_fit0, nsim = 500)
res2 <- pb2_modcomp(lmer_fit1, lmer_fit0, nsim = 500)
res1
res2

res1 |> summary()
res2 |> summary()











ref1 <- PBrefdist(lmer_fit1, lmer_fit0, nsim = 1200, cl=1)
ref2a <- pb_refdist(lmer_fit1, lmer_fit0, nsim = 1200, nworkers = 1, engine="serial")
## ref2b <- pb_refdist(lmer_fit1, lmer_fit0, nsim = 1200, nworkers = 1, engine="parallel")
## ref2c <- pb_refdist(lmer_fit1, lmer_fit0, nsim = 1200, nworkers = 1, engine="future")

ref1 |> summary()
ref2a$ref |> summary()
## ref2b$ref |> summary()
## ref2c$ref |> summary()

plot(sort(ref1), sort(ref2a$ref)); abline(c(0,1))
