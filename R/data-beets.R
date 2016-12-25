#' @title beets data
#'
#' @description Yield and sugar percentage in sugar beets from a split plot
#'     experiment.  Data is obtained from a split plot experiment. There are 3
#'     blocks and in each of these the harvest time defines the "whole plot" and
#'     the sowing time defines the "split plot". Each plot was \eqn{25 m^2} and
#'     the yield is recorded in kg. See 'details' for the experimental layout.
#'
#' @name data-beets
#' @docType data
#' @format The format is: chr "beets"
#'
#' @details
#' \preformatted{  
#' Experimental plan
#' Sowing times            1        4. april
#'                         2       12. april
#'                         3       21. april
#'                         4       29. april
#'                         5       18. may
#' Harvest times           1        2. october
#'                         2       21. october
#' Plot allocation:
#'                Block 1     Block 2     Block 3
#'             +-----------|-----------|-----------+
#'       Plot  | 1 1 1 1 1 | 2 2 2 2 2 | 1 1 1 1 1 | Harvest time
#'        1-15 | 3 4 5 2 1 | 3 2 4 5 1 | 5 2 3 4 1 | Sowing time
#'             |-----------|-----------|-----------|
#'       Plot  | 2 2 2 2 2 | 1 1 1 1 1 | 2 2 2 2 2 | Harvest time
#'       16-30 | 2 1 5 4 3 | 4 1 3 2 5 | 1 4 3 2 5 | Sowing time
#'             +-----------|-----------|-----------+  
#' }
#'
#' @references Ulrich Halekoh, Søren Højsgaard (2014)., A Kenward-Roger
#'     Approximation and Parametric Bootstrap Methods for Tests in Linear Mixed
#'     Models - The R Package pbkrtest., Journal of Statistical Software,
#'     58(10), 1-30., \url{http://www.jstatsoft.org/v59/i09/}
#' 
#' @keywords datasets
#'
#' @examples
#' data(beets)
#' 
#' beets$bh <- with(beets, interaction(block, harvest))
#' summary(aov(yield ~ block + sow + harvest + Error(bh), beets))
#' summary(aov(sugpct ~ block + sow + harvest + Error(bh), beets))
#' 
"beets"
