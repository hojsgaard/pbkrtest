simulate_lme <- function(object, nsim = 1, seed = NULL) {
  if (!inherits(object, "lme")) stop("Object must be an 'lme' object.")
  
  # Set seed for reproducibility
  if (!is.null(seed)) set.seed(seed)
  
  # Extract model components
  fixed_effects <- fixef(object)  # Fixed effects
  residual_var <- object$sigma^2  # Residual variance
  
  # Explicitly retrieve the data used in the model
  data <- getData(object)
  if (is.null(data)) stop("The model's data cannot be retrieved.")
  data <- as.data.frame(data)  # Ensure it is a data frame
  
  # Design matrix for fixed effects
  X <- model.matrix(formula(object, fixed.only = TRUE), data = data)
  
  # Grouping structure for random effects
  grouping <- getGroups(object)
  
  # Variance of random effects
  random_effects_var <- VarCorr(object)
  
  # Debugging step: Inspect random effects variance structure
  print("Random effects variance structure:")
  print(random_effects_var)
  
  random_effects_var_matrices <- lapply(random_effects_var, function(var) {
    var_matrix <- as.matrix(var)
    if (any(is.na(var_matrix)) || any(!is.finite(var_matrix))) {
      stop("Invalid random effects covariance matrix: contains NA or infinite values.")
    }
    return(var_matrix)
  })
  
  # Simulate data
  simulations <- replicate(nsim, {
    # Simulate random effects
    random_effects_sim <- lapply(random_effects_var_matrices, function(var_matrix) {
      MASS::mvrnorm(1, mu = rep(0, nrow(var_matrix)), Sigma = var_matrix)
    })
    
    # Combine fixed and random effects
    y_sim <- numeric(nrow(data))
    for (group in unique(grouping)) {
      group_idx <- which(grouping == group)
      group_random_effects <- random_effects_sim[[group]]
      y_sim[group_idx] <- X[group_idx, ] %*% fixed_effects + group_random_effects
    }
    
    # Add residual error
    y_sim <- y_sim + rnorm(nrow(data), mean = 0, sd = sqrt(residual_var))
    return(y_sim)
  })
  
  if (nsim == 1) {
    return(as.vector(simulations))  # Return as vector for single simulation
  } else {
    return(simulations)  # Return as matrix for multiple simulations
  }
}




simulate_lme_one <- function(object, psim = 1, data = NULL, ...) {
  if (!inherits(object, "lme")) stop("Object must be of class 'lme'")
  
  # Retrieve data used in the model
  if (is.null(data)) {
    data <- getData(object)
  }
  if (is.null(data)) stop("Data cannot be retrieved. Provide 'data' explicitly.")
  data <- as.data.frame(data)
  
  # Get fixed effects and design matrix
  fixed_effects <- fixef(object)
  X <- model.matrix(formula(object, fixed.only = TRUE), data = data)
  
  # Extract variance-covariance structure for random effects
  random_effects_var <- VarCorr(object)
  grouping <- getGroups(object)
  unique_groups <- unique(grouping)
  
  # Dynamically construct random effects covariance matrix
  random_effects_labels <- rownames(random_effects_var)
  num_random_effects <- sum(random_effects_labels != "Residual")
  cov_matrix <- matrix(0, nrow = num_random_effects, ncol = num_random_effects)
  
  for (i in seq_len(num_random_effects)) {
    for (j in seq_len(num_random_effects)) {
      if (i == j) {
        cov_matrix[i, j] <- as.numeric(random_effects_var[random_effects_labels[i], "Variance"])
      } else {
        cov_name <- paste0(random_effects_labels[i], ":", random_effects_labels[j])
        if (cov_name %in% colnames(random_effects_var)) {
          corr <- as.numeric(random_effects_var[cov_name, "Corr"])
          cov_matrix[i, j] <- corr * sqrt(cov_matrix[i, i] * cov_matrix[j, j])
        }
      }
    }
  }
  
  # Validate the random effects covariance matrix
  if (any(is.na(cov_matrix)) || any(!is.finite(cov_matrix))) {
    stop("Invalid random effects covariance matrix: contains NA or infinite values.")
  }
  
  # Simulate random effects for each group
  random_effects_sim <- MASS::mvrnorm(
    n = length(unique_groups),
    mu = rep(0, num_random_effects),
    Sigma = cov_matrix
  )
  
  # Combine fixed and random effects
  y_sim <- numeric(nrow(data))
  for (i in seq_along(unique_groups)) {
    group_idx <- which(grouping == unique_groups[i])
    group_random_effects <- random_effects_sim[i, ]
    group_contribution <- X[group_idx, ] %*% fixed_effects
    for (k in seq_along(group_random_effects)) {
      if (random_effects_labels[k] %in% colnames(data)) {
        group_contribution <- group_contribution +
          group_random_effects[k] * data[[random_effects_labels[k]]][group_idx]
      } else {
        group_contribution <- group_contribution + group_random_effects[k]
      }
    }
    y_sim[group_idx] <- group_contribution
  }
  
  # Add residual variance
  residuals <- rnorm(nrow(data), mean = 0, sd = object$sigma)
  y_sim <- y_sim + residuals
  
  return(y_sim)
}

