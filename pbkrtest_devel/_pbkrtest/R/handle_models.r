## FIXME: There is an issue with mixed models and lmerControl which is not set here..

handle_models <- function(largeModel, smallModel){


    if (is.character(smallModel))
        smallModel <- doBy::formula_add_str(formula(largeModel), terms=smallModel, op="-")
    
    if (inherits(smallModel, "formula"))
        smallModel  <- update(largeModel, smallModel)
    
    if (is.numeric(smallModel) && !is.matrix(smallModel))
        smallModel <- matrix(smallModel, nrow=1)
    
    if (inherits(smallModel, c("Matrix", "matrix"))){
        formula.small <- smallModel
        smallModel <- restriction_matrix2model(largeModel, smallModel, REML=FALSE)
    } else {
        formula.small <- formula(smallModel)
        attributes(formula.small) <- NULL
    }
    
    formula.large <- formula(largeModel)
    attributes(formula.large) <- NULL

    out <- list(formula.large = formula.large,
                formula.small = formula.small,
                largeModel    = largeModel,
                smallModel    = smallModel)

    return(out)

}

## mmm <- handle_models(largeModel, smallModel)
## largeModel <- mmm$largeModel
## smallModel <- mmm$smallModel
## formula.large <- mmm$formula.large
## formula.small <- mmm$formula.small
