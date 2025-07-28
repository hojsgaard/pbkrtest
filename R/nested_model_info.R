#' @title Resolve Nested Model Representation
#'
#' @description
#' Constructs or extracts a nested model (`fit0`) from a full model (`fit1`) 
#' using flexible input: a model object, formula, character string, or matrix.
#'
#' This function is useful for preparing models for comparison, e.g., via likelihood ratio test.
#'
#' @param fit1 A fitted model object (e.g., from `lm`, `lmer`, etc.).
#' @param fit0 A nested model specification: a model object, a formula (e.g., `~ . - x`),
#' a character vector of term names to remove, or a restriction matrix.
#'
#' @return A list with:
#' \describe{
#'   \item{formula_large}{Formula for `fit1`.}
#'   \item{formula_small}{Formula for resolved `fit0`.}
#'   \item{large_model}{The full model `fit1`.}
#'   \item{small_model}{The nested model `fit0`.}
#'   \item{L}{Restriction matrix defining the nested model.}
#' }
#'
#' @examples
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   library(lme4)
#'   data(sleepstudy)
#'   fit1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'   fit0 <- lmer(Reaction ~ (Days | Subject), sleepstudy)
#'
#'   get_nested_model_info(fit1, fit0)               # as model object
#'   get_nested_model_info(fit1, ~ . - Days)         # as formula
#'   get_nested_model_info(fit1, "Days")             # as string
#'   ## get_nested_model_info(fit1, c(0, 1))         # numeric (converted to matrix)
#' }
#'
#' @export
get_nested_model_info <- function(fit1, fit0){

    ## cat("handle_models\n")
    
    if (is.character(fit0)){
        fit0 <- doBy::formula_add_str(formula(fit1), terms=fit0, op="-")
        ## cat("Convert character into formula:\n"); print(fit0)
    }

    if (is.numeric(fit0) && !is.matrix(fit0)){ 
        fit0 <- matrix(fit0, nrow=1)
        ## cat("Convert numeric to matrix:\n"); print(fit0) 
    }

    ## Here fit0 is either (1) formula, (2) restriction matrix
    ## or (3) model object.

    if (inherits(fit0, "formula")){
        fit0  <- update(fit1, fit0)
        ## cat("Convert formula to model object: \n");  print(fit0)
    } else {
        if (inherits(fit0, c("Matrix", "matrix"))){
            ## formula.small <- fit0
            fit0 <- restriction_matrix2model(fit1, fit0, REML=FALSE)
            ## cat("Convert matrix to model object: \n");  print(fit0)
        } else {
            if (!identical(class(fit1), class(fit0)))
                stop("Model objects not same class\n")
        }
    }

    L <- model2restriction_matrix(fit1, fit0)
    
    formula.small <- formula(fit0)
    attributes(formula.small) <- NULL
        
    formula.large <- formula(fit1)
    attributes(formula.large) <- NULL
    
    out <- list(formula.large = formula.large,
                formula.small = formula.small,
                largeModel    = fit1,
                smallModel    = fit0,
                L             = L
                )

    return(out)
}




## get_nested_model_info <- function(fit1, fit0) {
##   if (is.character(fit0)) {
##     fit0 <- doBy::formula_add_str(formula(fit1), terms = fit0, op = "-")
##   }

##   if (is.numeric(fit0) && !is.matrix(fit0)) {
##     fit0 <- matrix(fit0, nrow = 1)
##   }

##   if (inherits(fit0, "formula")) {
##     fit0 <- update(fit1, fit0)
##   } else if (inherits(fit0, c("Matrix", "matrix"))) {
##     fit0 <- restriction_matrix2model(fit1, fit0, REML = FALSE)
##   } else {
##     if (!identical(class(fit1), class(fit0))) {
##       stop("Model objects must be of the same class.")
##     }
##   }

##   L <- model2restriction_matrix(fit1, fit0)

##   formula_large <- formula(fit1); attributes(formula_large) <- NULL
##   formula_small <- formula(fit0); attributes(formula_small) <- NULL

##   out <- list(
##     formula_large = formula_large,
##     formula_small = formula_small,
##     large_model = fit1,
##     small_model = fit0,
##     L = L
##   )

##   invisible(out)
## }








## FIXME: There is an issue with mixed models and lmerControl which is not set here..


## #' (fm0 <- lmer(Reaction ~ (Days|Subject), sleepstudy))
## #' (fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
## #' (fm2 <- lmer(Reaction ~ Days + I(Days^2) + (Days|Subject), sleepstudy))
## #'
## #' ## Test for no effect of Days in fm1, i.e. test fm0 under fm1
## #'
## #' load_all()
## #' handle_models(fm1, "Days")
## #' handle_models(fm1, ~.-Days)
## #' handle_models(fm1, cbind(0, 1)) ## FIXME
## #' handle_models(fm1, c(0,1)) ## FIXME

## handle_models <- function(largeModel, smallModel){

##     ## cat("handle_models\n")
    
##     if (is.character(smallModel)){
##         smallModel <- doBy::formula_add_str(formula(largeModel), terms=smallModel, op="-")
##         ## cat("Convert character into formula:\n"); print(smallModel)
##     }

##     if (is.numeric(smallModel) && !is.matrix(smallModel)){ 
##         smallModel <- matrix(smallModel, nrow=1)
##         ## cat("Convert numeric to matrix:\n"); print(smallModel) 
##     }

##     ## Here smallModel is either (1) formula, (2) restriction matrix
##     ## or (3) model object.

##     if (inherits(smallModel, "formula")){
##         smallModel  <- update(largeModel, smallModel)
##         ## cat("Convert formula to model object: \n");  print(smallModel)
##     } else {
##         if (inherits(smallModel, c("Matrix", "matrix"))){
##             ## formula.small <- smallModel
##             smallModel <- restriction_matrix2model(largeModel, smallModel, REML=FALSE)
##             ## cat("Convert matrix to model object: \n");  print(smallModel)
##         } else {
##             if (!identical(class(largeModel), class(smallModel)))
##                 stop("Model objects not same class\n")
##         }
##     }

##     L <- model2restriction_matrix(largeModel, smallModel)
    
##     formula.small <- formula(smallModel)
##     attributes(formula.small) <- NULL
        
##     formula.large <- formula(largeModel)
##     attributes(formula.large) <- NULL
    
##     out <- list(formula.large = formula.large,
##                 formula.small = formula.small,
##                 largeModel    = largeModel,
##                 smallModel    = smallModel,
##                 L             = L
##                 )

##     invisible(out)
## }

## largeModel is model object
## smallModel is
## 1. Model object
## 2. Restriction string
## 3. Restriction formula
## 4. Restriction matrix

## cat("smallModel: \n");     print(smallModel)


## mmm <- handle_models(largeModel, smallModel)
## largeModel <- mmm$largeModel
## smallModel <- mmm$smallModel
## formula.large <- mmm$formula.large
## formula.small <- mmm$formula.small


