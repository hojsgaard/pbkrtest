##' @title Get variance covariance matrix for lmerMod and gls 
##' @param object An lmerMod or gls object
##' @return A sparse matrix
##' @author Søren Højsgaard
##'
##' @export
get_vary <- function(object){
    UseMethod("get_vary")
}

##' @export
get_vary.lmerMod <- function(object){
    ## if (!inherits(object, "lmerMod")) stop("The model must be a fitted lmer object.")
    out <- get_matrices_from_lmer(object)
    t(out$Zt) %*% out$G %*% out$Zt + out$R
}

##' @export
get_vary.gls <- function(object){
    ## if (!inherits(object, "lmerMod")) stop("The model must be a fitted lmer object.")
    out <- worker_gls(object)
    out <- as(out, "sparseMatrix")    
    return(out)
}

worker_gls <- function(model){
    ## Extract covariance parameters
    sigma2 <- sigma(model)^2                 ## Residual variance
    cor_params <- coef(model$modelStruct$corStruct, unconstrained = FALSE)  ## Correlation parameter
    
    ## Print parameters
    ## cat("Residual variance (sigma^2):", sigma2, "\n")
    ## cat("Correlation parameter (rho):", cor_params, "\n")
    
    ## Construct the full covariance matrix
    C <- corMatrix(model$modelStruct$corStruct)
    V <- sigma2 * as.matrix(bdiag(C))
    
    ## Print the covariance matrix
    ## print(V)
    V
}

get_matrices_from_lmer <- function(model){
    ## https://stackoverflow.com/questions/45650548/get-residual-variance-covariance-matrix-in-lme4
    if (!inherits(model, "lmerMod")) stop("The model must be a fitted lmer object.")

    var.d <- Matrix::crossprod(getME(model,"Lambdat"))
    Zt <- getME(model,"Zt")
    vr <- sigma(model)^2
    
    G <- vr * var.d
    ZGZt <- t(Zt) %*% G %*% Zt
    R <- vr * Diagonal(ncol(Zt))
    var.y <- ZGZt + R
    
    list(Zt=Zt, G=G, R=R)
}




## get_vary.lme <- function(object){
##     ## if (!inherits(object, "lmerMod")) stop("The model must be a fitted lmer object.")
##     out <- compute_full_covariance_matrix(object)
##     out <- as(out, "sparseMatrix")
##     return(out)
## }

## compute_full_covariance_matrix <- function(object) {
##   if (!inherits(object, "lme")) stop("Object must be of class 'lme'")
  
##   # Extract grouping structure and unique groups
##   grouping <- getGroups(object)
##   unique_groups <- unique(grouping)
##   num_obs <- length(grouping)
  
##   # Extract random effects covariance matrix
##   re_struct <- object$modelStruct$reStruct
##   random_effects_matrix <- NULL
##   if (!is.null(re_struct$Subject)) {
##     random_effects_matrix <- as.matrix(pdMatrix(re_struct$Subject))
##   }
  
##   # Initialize full covariance matrix with residual variance only
##   residual_var <- object$sigma^2
##   full_cov_matrix <- diag(residual_var, num_obs, num_obs)
  
##   # Add random effects covariance structure if available
##   if (!is.null(random_effects_matrix)) {
##     group_indices <- split(seq_len(num_obs), grouping)
##     for (i in seq_along(unique_groups)) {
##       group_idx <- group_indices[[i]]
##       n <- length(group_idx)
      
##       # Expand random effects covariance matrix for group
##       if (nrow(random_effects_matrix) != ncol(random_effects_matrix)) {
##         stop("Random effects matrix must be square. Check the model structure.")
##       }
##       group_cov_matrix <- random_effects_matrix[1:min(nrow(random_effects_matrix), n), 
##                                                 1:min(ncol(random_effects_matrix), n)]
      
##       # Fill diagonal with group covariance if dimensions allow
##       expanded_group_cov <- diag(0, nrow = n, ncol = n)
##       expanded_group_cov[1:nrow(group_cov_matrix), 1:ncol(group_cov_matrix)] <- group_cov_matrix
      
##       # Add to the full covariance matrix
##       full_cov_matrix[group_idx, group_idx] <- full_cov_matrix[group_idx, group_idx] + expanded_group_cov
##     }
##   } else {
##     message("No random effects structure detected. Using residual variance only.")
##   }
  
##   return(full_cov_matrix)
## }



## get_full_covariance_lme <- function(model) {
##   # Extract grouping factor
##   groups <- model$groups[[1]]
  
##   # Extract variances and correlations from VarCorr
##   varcorr <- VarCorr(model)
##   re_stddevs <- as.numeric(varcorr[1:(nrow(varcorr) - 1), "StdDev"])  # Random effect stddevs
  
##   # Extract correlations
##   n_re <- length(re_stddevs)  # Number of random effects (e.g., intercept and slope)
##   re_corrs <- diag(n_re)  # Initialize as identity matrix
##   if (n_re > 1) {
##     # Extract correlation values dynamically
##     corr_indices <- which(lower.tri(re_corrs))  # Indices for the lower triangular part
##     corr_values <- as.numeric(varcorr[corr_indices, "Corr"])  # Extract correlations
##     re_corrs[lower.tri(re_corrs)] <- corr_values  # Populate the lower triangular part
##     re_corrs <- re_corrs + t(re_corrs) - diag(diag(re_corrs))  # Make symmetric
##   }
  
##   # Build G for a single group
##   G_group <- diag(re_stddevs) %*% re_corrs %*% diag(re_stddevs)  # Covariance matrix
  
##   # Expand G to all groups (block-diagonal structure)
##   n_groups <- length(unique(groups))
##   G <- kronecker(Diagonal(n_groups), G_group)  # Block diagonal for all groups
  
##   # Create the random effects design matrix Z
##   Z <- as.matrix(do.call(cbind, lapply(1:n_re, function(i) {
##     as(model.matrix(~ -1 + factor(groups)), "sparseMatrix")
##   })))
  
##   # Residual variance
##   sigma_sq <- model$sigma^2
##   R <- diag(sigma_sq, nrow(Z))  # Residual covariance matrix
  
##   # Full covariance matrix of the response
##   V <- Z %*% G %*% t(Z) + R
  
##   # Return components
##   list(Z = Z, G = G, R = R, V = V)
## }


