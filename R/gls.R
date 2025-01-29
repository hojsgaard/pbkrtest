##' @title Returns covariance matrix for gls model
##' @param model A gls object
##' @return A matrix
##' @author Søren Højsgaard
##' @export
gls_cov_matrix <- function(model) {
  if (!inherits(model, "gls")) stop("Model must be a 'gls' object.")
  
  # Hent antal observationer
  n_obs <- nrow(getData(model))
  
  # Hent residual standard error
  sigma <- model$sigma
  
  # Hent korrelationsstruktur
  correlation_structure <- model$modelStruct$corStruct
  if (!is.null(correlation_structure)) {
    if (inherits(correlation_structure, "corCompSymm")) {
      # Hvis corCompSymm, konstruer korrelationsmatrix manuelt
      rho <- coef(correlation_structure, unconstrained = FALSE)["Rho"]
      group <- getGroups(model)  # Hent gruppering fra modellen
      levels_group <- unique(group)
      
      # Konstruer en blokdiagonal korrelationsmatrix
      corr_matrix <- matrix(0, nrow = n_obs, ncol = n_obs)
      for (g in levels_group) {
        idx <- which(group == g)
        block_size <- length(idx)
        block_matrix <- matrix(rho, nrow = block_size, ncol = block_size)
        diag(block_matrix) <- 1
        corr_matrix[idx, idx] <- block_matrix
      }
    } else {
      # Prøv at udtrække korrelationsmatrix normalt
      corr_matrix <- corMatrix(correlation_structure)
    }
  } else {
    # Ingen korrelation - brug identitetsmatrix
    corr_matrix <- diag(1, n_obs)
  }
  
  if (!is.matrix(corr_matrix) || nrow(corr_matrix) != n_obs || ncol(corr_matrix) != n_obs) {
    stop("Invalid correlation matrix. Check the model structure.")
  }
  
  # Juster for variansstrukturen, hvis den findes
  var_struct <- model$modelStruct$varStruct
  if (!is.null(var_struct)) {
    weights <- varWeights(var_struct)
    weight_matrix <- diag(weights)
    cov_matrix <- sigma^2 * weight_matrix %*% corr_matrix %*% weight_matrix
  } else {
    cov_matrix <- sigma^2 * corr_matrix
  }
  
  return(cov_matrix)
}

##' @title Simulate data for 'gls' object
##' @description Simulate data for 'gls' object
##' @param object A gls object
##' @param nsim Number of simulations
##' @param seed Seed for random number generator
##' @param object A gls object
##' @param ... Additional arguments; currently not used. 
##' @return A dataframe with nsim columns
##' @author Søren Højsgaard
##' @export
simulate.gls <- function(object, nsim = 1, seed = NULL, ...) {
  if (!inherits(object, "gls")) stop("Object must be a 'gls' object.")
  
  # Sæt seed for reproducerbarhed
  if (!is.null(seed)) set.seed(seed)
  
  # Hent antal observationer
  n_obs <- nrow(getData(object))
  
  # Hent fitted værdier (forventede værdier)
  fitted_values <- fitted(object)
  
  # Beregn kovariansmatricen
  cov_matrix <- gls_cov_matrix(object)
  
  ## Simulér data fra multivariat normalfordeling
  if (!is.null(seed))
      set.seed(seed)
  simulations <- MASS::mvrnorm(nsim, mu = fitted_values, Sigma = cov_matrix)

  simulations <- as.data.frame(t(simulations))

  return(simulations)
}





##' @title Refit a gls object to new response variable
##' @param object A gls object
##' @param newresp A numeric vector
##' @return A gls object 
##' @author Søren Højsgaard
##' 
##' @export
refit_gls <- function(object, newresp) {
  if (!inherits(object, "gls")) stop("Object must be a 'gls' object.")
  
  # Hent data fra modellen
  original_data <- getData(object)
  
  # Skab en ny formel
  original_formula <- formula(object)
  term_labels <- attr(terms(original_formula), "term.labels")
  
  # Håndter tilfælde af y ~ 1
  if (length(term_labels) == 0) {
    new_formula <- as.formula("new_response ~ 1")
  } else {
    new_formula <- as.formula(paste(
      "new_response", "~", paste(term_labels, collapse = "+")
    ))
  }
  
  # Kopiér miljøet for den oprindelige model
  formula_env <- environment(original_formula)
  if (is.null(formula_env)) formula_env <- parent.frame()
  
  # Tilføj den nye variabel til miljøet
  assign("new_response", newresp, envir = formula_env)
  
  # Opdater modelkaldet
  updated_call <- object$call
  updated_call$model <- new_formula
  
  # Evaluer det opdaterede modelkald
  updated_object <- eval(updated_call, parent.frame())
  
  # Returnér den nye model
  return(updated_object)
}

## ##' @title Refit a lme object to new response variable
## ##' @param object A lme object
## ##' @param newresp A numeric vector
## ##' @return A gls object 
## ##' @author Søren Højsgaard
## ##' @export
## refit_lme <- function(object, newresp) {
##   if (!inherits(object, "lme")) stop("Object must be a 'lme' object.")
  
##   # Hent data fra modellen
##   original_data <- getData(object)
  
##   # Tilføj den nye responsvariabel til data
##   original_data[["new_response"]] <- newresp
  
##   # Skab en ny formel
##   original_formula <- formula(object)
##   term_labels <- attr(terms(original_formula), "term.labels")
  
##   # Håndter tilfælde af y ~ 1
##   if (length(term_labels) == 0) {
##     new_formula <- as.formula("new_response ~ 1")
##   } else {
##     new_formula <- as.formula(paste(
##       "new_response", "~", paste(term_labels, collapse = "+")
##     ))
##   }
  
##   # Opdater modelkaldet
##   updated_call <- object$call
##   updated_call$fixed <- new_formula
##   updated_call$data <- original_data
##   updated_call$control <- lmeControl(opt = "nlminb", maxIter = 400, msMaxIter = 600, niterEM = 150)
  
##   # Brug startværdier fra den oprindelige model
##   ## updated_call$start <- list(fixed = fixef(object))
  
##   # Evaluer det opdaterede modelkald
##   updated_object <- eval(updated_call, parent.frame())
  
##   # Returnér den nye model
##   return(updated_object)
## }



##' @title Variance and correlation parameters from gls object
##' @param object A gls object
##' @return Variance and covariance parameters of object 
##' @author Søren Højsgaard
##' @export
gls_varcorr <- function(object) {
  if (!inherits(object, "gls")) stop("Object must be a 'gls' object.")
  
  # Initialize a list to store parameters
  parameters <- list()
  
  # Extract residual standard error
  parameters$sigma <- object$sigma
  
  # Extract correlation parameters
  correlation_structure <- object$modelStruct$corStruct
  if (!is.null(correlation_structure)) {
    # Estimated parameters
    parameters$correlation <- coef(correlation_structure, unconstrained = FALSE)
    parameters$correlation_structure <- class(correlation_structure)
  } else {
    parameters$correlation <- "None"
  }
  
  # Extract variance parameters
  var_struct <- object$modelStruct$varStruct
  if (!is.null(var_struct)) {
    # Estimated parameters
    parameters$variances <- coef(var_struct, unconstrained = FALSE)
    parameters$variance_structure <- class(var_struct)
  } else {
    parameters$variances <- "None"
  }
  
  # Return the parameters
  return(parameters)
}
