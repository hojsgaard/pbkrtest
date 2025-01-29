#' Select elements of vectors and lists without typing square brackets.
#'
#' Select elements of vectors and lists without typing square
#' brackets. Intended for use with pipe operator.
#'
#' @param object An object that list-like or atomic.
#' @param entry Entries to be selected
#' @examples
#'
#' x1 <- 10 * c(1,2,3,4,5)
#' nth(x1, 4)
#' ## nth(x1,4:5)
#'
#' x2 <- 10 * c(1,2,3,4,5)
#' names(x2) <- letters[1:length(x2)]
#' nth(x2, "a")
#' ## nth(x2, c("a", "e"))
#'
#' nth(iris, 1)
#' ## nth(iris, c(1,2))


#' @export
nth <- function(object, entry) {
    if (length(entry) != 1)
        stop("'entry' must have length 1\n")
    if (!(is.list(object) || is.atomic(object)))
        stop("input of wrong type\n")
    if (is.list(object)) {
        "[["(object, entry)        
    } else {
        if (is.atomic(object)) {
            "["(object, entry)                    
        }
    }
}

#' @export
nth <- function(object, entry) {
    if (length(entry) != 1)
        stop("'entry' must have length 1\n")
    if (!(is.list(object) || is.atomic(object)))
        stop("input of wrong type\n")
    if (is.list(object)) {
        "[["(object, entry)        
    } else {
        if (is.atomic(object)) {
            "["(object, entry)                    
        }
    }
}
