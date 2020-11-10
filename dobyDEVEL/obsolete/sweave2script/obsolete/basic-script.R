#'
#'
#'
#' ---
#'
#' # Summarizing data
#' #### Søren Højsgaard
#'
#' The `shoes` data is a list of two vectors, giving the wear of shoes of
#' materials `X` and `Y` for one foot each of ten boys.
data(shoes, package="MASS")
names(shoes) <- c("x","y")
shoes
#'
#' First focus on data for material `A`;
x <- shoes$x; x
#'
#' We shall look at measures of where is the "location" or "center" of the data and what is the
#' "spread" of data.
#'
#' ---
#'
#' For the sum $x_1 + x_2 + x_3 + \dots + x_7+ x_8$ we write
#' $$
#'   x_. = \Sigma_{i=1}^8 x_i = x_1 + x_2 + x_3 + \dots + x_7+ x_8
#' $$
#'
