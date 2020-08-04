#'
#' # From sweave to an R script
#' #### Søren Højsgaard
#'
#'
#'
#' ## The shoes data
#'
#' Consider the shoes data from the MASS package:
#'
data(shoes, package="MASS")
shoes
#'
#'
#' We shall do
#' *   an unpaired $t$-test
#'   and
#' *   a paired $t$-test
#'
#' Compare two shoe types with a $t$-test:
#'
with(shoes, t.test(A, B))
#'
#' The test is misleading because observations are paired. A better
#' alternative is to make a paired $t$-test:
#'
with(shoes, t.test(A, B, paired=T))
#'
#'
#'
