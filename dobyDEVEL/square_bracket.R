#' Select elements of vectors and lists without typing square brackets.
#'
#' Select elements of vectors and lists without typing square
#' brackets. Intended for use with pipe operator.
#'
#' @param object An object that list-like or atomic.
#' @param entry Entries to be selected
#'

#' @export
sqb <- function(object, entry){
    if (!(is.list(object) || is.atomic(object)))
        stop("input of wrong type\n")
    if (is.list(object)){
        "[["(object, entry)        
    } else {
        if (is.atomic(object)){
            "["(object, entry)                    
        }
    }
}
