#' @examples
#'
#'
#'
#' 
#'   sleepstudy$Days2 <- sleepstudy$Days^2
#'
#' lm_fit1 <- lm(Reaction ~ Days + Days2, data = sleepstudy)
#'   lm_fit0 <- update(lm_fit1, . ~ . - Days2)
#' 
#'   gls_fit1 <- gls(Reaction ~ Days + Days2, data = sleepstudy, method = "ML")
#'   gls_fit0 <- update(gls_fit1, . ~ . - Days2)
#'
#'   lme_fit1 <- lme(Reaction ~ Days + Days2, random = ~ 1 | Subject,
#'                         data = sleepstudy, method = "ML")
#'   lme_fit0 <- update(lme_fit1, . ~ . - Days2)
#'   
#'   lmer_fit1 <- lmer(Reaction ~ Days + Days2 + (1 | Subject),
#'                           data = sleepstudy, REML = FALSE)
#'   lmer_fit0 <- update(lmer_fit1, . ~ . - Days2)
#'
#'   glmer_fit1 <- glmer(Reaction ~ Days + Days2 + (1 | Subject),
#'                           data = sleepstudy, family=Gamma("log"))
#'   glmer_fit0 <- update(glmer_fit1, . ~ . - Days2)
#'
#'
#' res_lm <- pb2_modcomp(lm_fit1, lm_fit0, nsim=NSIM)
#' res_gls <- pb2_modcomp(gls_fit1, gls_fit0, nsim=NSIM)
#' res_lme <- pb2_modcomp(lme_fit1, lme_fit0, nsim=NSIM)
#' res_lmer <- pb2_modcomp(lmer_fit1, lmer_fit0, nsim=NSIM)
#' res_glmer <- pb2_modcomp(glmer_fit1, glmer_fit0, nsim=NSIM) ## Warnings....
#'
#' NSIM <- 300
#'
#' res_lm <- pb2_modcomp(lm_fit1, lm_fit0, sequential=TRUE, nsim=NSIM, nworkers=1)
#' res_gls <- pb2_modcomp(gls_fit1, gls_fit0, sequential=TRUE, nsim=NSIM, nworkers=1)
#' res_lme <- pb2_modcomp(lme_fit1, lme_fit0, sequential=TRUE, nsim=NSIM, nworkers=1)
#' res_lmer <- pb2_modcomp(lmer_fit1, lmer_fit0, sequential=TRUE, nsim=NSIM, nworkers=1)
#' res_glmer <- pb2_modcomp(glmer_fit1, glmer_fit0, sequential=TRUE, nsim=NSIM, nworkers=1) ## Warnings....
#' 
#' res_lm |> summary()
#' res_gls |> summary()
#' res_lme |> summary()
#' res_lmer |> summary()
#' res_glmer |> summary()
#'
#' res_lm |> plot()
#' res_gls |> plot()
#' res_lme |> plot()
#' res_lmer |> plot()
#' res_glmer |> plot()
#' 
#' ref_lm <- pb_refdist(lm_fit1, lm_fit0, nsim=NSIM)
#' ref_gls <- pb_refdist(gls_fit1, gls_fit0, nsim=NSIM)
#' ref_lme <- pb_refdist(lme_fit1, lme_fit0, nsim=NSIM)
#' ref_lmer <- pb_refdist(lmer_fit1, lmer_fit0, nsim=NSIM)
#' ref_glmer <- pb_refdist(glmer_fit1, glmer_fit0, nsim=NSIM) ## Warnings....
#'
#' ref_lm |> summary()
#' ref_gls |> summary()
#' ref_lme |> summary()
#' ref_lmer |> summary()
#' ref_glmer |> summary()
#'
#' ref_lm |> summarize_pb()
#' ref_gls |> summarize_pb()
#' ref_lme |> summarize_pb()
#' ref_lmer |> summarize_pb()
#' ref_glmer |> summarize_pb()










#' @examples
#'
#'
#' 
#' if (requireNamespace("lme4") && requireNamespace("nlme")) {
#'   data(sleepstudy, package = "lme4")
#'   sleepstudy$Days2 <- sleepstudy$Days^2
#'   NSIM <- 10
#' 
#'   # LM example
#'   lm_fit1 <- lm(Reaction ~ Days + Days2, data = sleepstudy)
#'   lm_fit0 <- update(lm_fit1, . ~ . - Days2)
#'   set.seed(42)
#'   res_lm <- pb2_modcomp(lm_fit1, lm_fit0, nsim=NSIM)
#'   res_lm
#'   summary(res_lm)
#'   plot(res_lm, show.chisq=TRUE)
#'
#'   # GLS example
#'   gls_fit1 <- nlme::gls(Reaction ~ Days + Days2, data = sleepstudy, method = "ML")
#'   gls_fit0 <- update(gls_fit1, . ~ . - Days2)
#'   set.seed(42)
#'   res_gls <- pb2_modcomp(gls_fit1, gls_fit0, nsim=NSIM)
#'   res_gls
#'   summary(res_gls)
#'   plot(res_gls, show.chisq=TRUE)
#'
#'   # LME example
#'   lme_fit1 <- nlme::lme(Reaction ~ Days + Days2, random = ~ 1 | Subject,
#'                         data = sleepstudy, method = "ML")
#'   lme_fit0 <- update(lme_fit1, . ~ . - Days2)
#'   set.seed(42)
#'   res_lme <- pb2_modcomp(lme_fit1, lme_fit0, nsim=NSIM)
#'   res_lme
#'   summary(res_lme)
#'   plot(res_lme, show.chisq=TRUE)
#'
#'   # LMER example (lme4)
#'   lmer_fit1 <- lme4::lmer(Reaction ~ Days + Days2 + (1 | Subject),
#'                           data = sleepstudy, REML = FALSE)
#'   lmer_fit0 <- update(lmer_fit1, . ~ . - Days2)
#'   set.seed(42)
#'   res_lmer <- pb2_modcomp(lmer_fit1, lmer_fit0, nsim=NSIM)
#'   res_lmer
#'   summary(res_lmer)
#'   plot(res_lmer, show.chisq=TRUE)
#'
#'   # Sequential example
#'   set.seed(42)
#'   res_seq <- pb2_modcomp(lmer_fit1, lmer_fit0, sequential = TRUE, h = 20, nsim=NSIM)
#'   res_seq
#'   summary(res_seq)
#'   plot(res_seq, show.chisq=TRUE)
#' 
#' }
#'


#'
#' @examples
#'
#' sleepstudy$Days2 <- sleepstudy$Days^2
#' NSIM <- 10
#' 
#' lm_fit1 <- lm(Reaction ~ Days, data = sleepstudy)
#' lm_fit0 <- update(lm_fit1, . ~ . - Days)
#' set.seed(42)
#' lrt_lm <- getLRT(lm_fit1, lm_fit0)
#' lr_sim_lm <- pb_refdist(lm_fit1, lm_fit0, nsim=NSIM, engine = "future", nworkers =2)
#' summarize_pb(lrt_lm, lr_sim_lm)
#' 
#' gls_fit1 <- gls(Reaction ~ Days, data = sleepstudy, method="ML")
#' gls_fit0 <- update(gls_fit1, . ~ . - Days)
#' set.seed(42)
#' lrt_gls <- getLRT(gls_fit1, gls_fit0)
#' lr_sim_gls <- pb_refdist(gls_fit1, gls_fit0, nsim=NSIM, engine = "future", nworkers = 2)
#' summarize_pb(lrt_gls, lr_sim_gls)
#' 
#' lme_fit1 <- lme(Reaction ~ Days, random = ~ 1 | Subject, data = sleepstudy, method="ML")
#' lme_fit0 <- update(lme_fit1, . ~ . - Days)
#' set.seed(42)
#' lrt_lme <- getLRT(lme_fit1, lme_fit0)
#' lr_sim_lme <- pb_refdist(lme_fit1, lme_fit0, nsim=NSIM, engine = "future", nworkers = 2)
#' summarize_pb(lrt_lme, lr_sim_lme)
#' 
#' lmer_fit1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy, REML=FALSE)
#' lmer_fit0 <- update(lmer_fit1, . ~ . - Days)
#' set.seed(42)
#' lrt_lmer <- getLRT(lmer_fit1, lmer_fit0)
#' lr_sim_lmer <- pb_refdist(lmer_fit1, lmer_fit0, nsim=NSIM, engine = "future", nworkers = 2)
#' summarize_pb(lrt_lmer, lr_sim_lmer)
#'

#' sleepstudy$Days2 <- sleepstudy$Days^2
#' 
#' lm_fit1 <- lm(Reaction ~ Days + Days2, data = sleepstudy)
#' lm_fit0 <- update(lm_fit1, . ~ . - Days2)
#' set.seed(42)
#' lrt_lm <- getLRT(lm_fit1, lm_fit0)
#' lr_sim_lm <- pb_refdist(lm_fit1, lm_fit0, nsim=NSIM, engine = "future", nworkers =2)
#' summarize_pb(lrt_lm, lr_sim_lm)
#' 
#' gls_fit1 <- gls(Reaction ~ Days + Days2, data = sleepstudy, method="ML")
#' gls_fit0 <- update(gls_fit1, . ~ . - Days2)
#' set.seed(42)
#' lrt_gls <- getLRT(gls_fit1, gls_fit0)
#' lr_sim_gls <- pb_refdist(gls_fit1, gls_fit0, nsim=NSIM, engine = "future", nworkers = 2)
#' summarize_pb(lrt_gls, lr_sim_gls)
#' 
#' lme_fit1 <- lme(Reaction ~ Days + Days2, random = ~ 1 | Subject, data = sleepstudy, method="ML")
#' lme_fit0 <- update(lme_fit1, . ~ . - Days2)
#' set.seed(42)
#' lrt_lme <- getLRT(lme_fit1, lme_fit0)
#' lr_sim_lme <- pb_refdist(lme_fit1, lme_fit0, nsim=NSIM, engine = "future", nworkers = 2)
#' summarize_pb(lrt_lme, lr_sim_lme)
#' 
#' lmer_fit1 <- lmer(Reaction ~ Days + Days2 + (1 | Subject), data = sleepstudy, REML=FALSE)
#' lmer_fit0 <- update(lmer_fit1, . ~ . - Days2)
#' set.seed(42)
#' lrt_lmer <- getLRT(lmer_fit1, lmer_fit0)
#' lr_sim_lmer <- pb_refdist(lmer_fit1, lmer_fit0, nsim=NSIM, engine = "future", nworkers = 2)
#' summarize_pb(lrt_lmer, lr_sim_lmer) |> summary()
#'
#' lr_sim_lmer <- pb_refdist_sequential(lmer_fit1, lmer_fit0, nsim=NSIM,
#'     engine = "future", nworkers = 2)
#' summarize_pb(lrt_lmer, lr_sim_lmer)


#' @examples
#' if (requireNamespace("lme4") && requireNamespace("nlme")) {
#'   data(sleepstudy, package = "lme4")
#'   sleepstudy$Days2 <- sleepstudy$Days^2
#'   NSIM <- 20
#'   # LM example
#'   lm_fit1 <- lm(Reaction ~ Days + Days2, data = sleepstudy)
#'   lm_fit0 <- update(lm_fit1, . ~ . - Days2)
#'   set.seed(42)
#'   res_lm <- pb_refdist(lm_fit1, lm_fit0, nsim=NSIM)
#'   summary(res_lm)
#'   plot(res_lm, show.chisq=TRUE)
#'
#'   # GLS example
#'   gls_fit1 <- nlme::gls(Reaction ~ Days + Days2, data = sleepstudy, method="ML")
#'   gls_fit0 <- update(gls_fit1, . ~ . - Days2)
#'   set.seed(42)
#'   res_gls <- pb_refdist(gls_fit1, gls_fit0, nsim=NSIM)
#'   summary(res_gls)
#'   plot(res_gls, show.chisq=TRUE)
#'
#'   # LME example
#'   lme_fit1 <- nlme::lme(Reaction ~ Days + Days2, random = ~ 1 | Subject,
#'                         data = sleepstudy, method="ML")
#'   lme_fit0 <- update(lme_fit1, . ~ . - Days2)
#'   set.seed(42)
#'   res_lme <- pb_refdist(lme_fit1, lme_fit0, nsim=NSIM)
#'   summary(res_lme)
#'   plot(res_lme, show.chisq=TRUE)
#'
#'   # LMER example (lme4)
#'   lmer_fit1 <- lme4::lmer(Reaction ~ Days + Days2 + (1 | Subject),
#'                           data = sleepstudy, REML=FALSE)
#'   lmer_fit0 <- update(lmer_fit1, . ~ . - Days2)
#'   set.seed(42)
#'   res_lmer <- pb_refdist(lmer_fit1, lmer_fit0, nsim=NSIM)
#'   summary(res_lmer)
#'   plot(res_lmer, show.chisq=TRUE)
#'
#'   # Sequential example
#'   set.seed(42)
#'   res_seq <- pb_refdist_sequential(lmer_fit1, lmer_fit0, h = 20, nsim=NSIM)
#'   summary(res_seq)
#'   plot(res_seq, show.chisq=TRUE)
#' }
#'



library(nlme)

fm1 <- lme(distance ~ age, data = Orthodont) # random is ~ age
fm2 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)
summary(fm1)
summary(fm2)
     
simulate(fm1)


orthSim <-
    simulate.lme(list(fixed = distance ~ age, data = Orthodont,
                      random = ~ 1 | Subject),
                 nsim = 3, # limited here for speed
                 m2 = list(random = ~ age | Subject))
     

## These two examples work in 0.5.4 but not in the latest CRAN version 0.5.5.

library(lme4)
library(pbkrtest)
packageVersion("pbkrtest")

gm1 <- glmer(cbind(incidence, size-incidence) ~ period + (1 | herd), data=cbpp, family=binomial)
gm0 <- glmer(cbind(incidence, size-incidence) ~ 1 + (1 | herd), data=cbpp, family=binomial)

PBmodcomp(gm1, gm0, nsim=5)
PBmodcomp(gm1, gm0, nsim=50)
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
library(pbkrtest)
load_all("_pbkrtest")
sleepstudy <- sleepstudy |> transform(Days2 = Days^2)


lmer_fit1 <- lmer(Reaction ~ Days + Days2 + (1 | Subject),
                        data = sleepstudy, REML = FALSE)
lmer_fit0 <- update(lmer_fit1, . ~ . - Days2)

res1 <- PBmodcomp(lmer_fit1, lmer_fit0, nsim = 500, seed=123)
res1


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
