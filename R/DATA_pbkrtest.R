#' Sugar beets data
#' 
#' Yield and sugar percentage in sugar beets from a split plot
#'     experiment.  The experimental layout was as follows: There were
#'     three blocks. In each block, the harvest time defines the
#'     "whole plot" and the sowing time defines the "split plot". Each
#'     plot was \eqn{25 m^2} and the yield is recorded in kg. See
#'     'details' for the experimental layout. The data originates from
#'     a study carried out at The Danish Institute for Agricultural
#'     Sciences (the institute does not exist any longer; it became
#'     integrated in a Danish university).
#'
#' @name data-beets
#' @docType data
#' @format A dataframe with 5 columns and 30 rows. 
#' @concept data
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
#'     58(10), 1-30., \url{https://www.jstatsoft.org/v59/i09/}
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


#' @title Budworm data
#' 
#' @description Experiment on the toxicity to the tobacco budworm
#'     Heliothis virescens of doses of the pyrethroid
#'     trans-cypermethrin to which the moths were beginning to show
#'     resistance. Batches of 20 moths of each sex were exposed for
#'     three days to the pyrethroid and the number in each batch that
#'     were dead or knocked down was recorded. Data is reported in
#'     Collett (1991, p. 75).
#'
#' @concept data
#' @name data-budworm
#' @docType data
#' 
#' @format This data frame contains 12 rows and 4 columns:
#'
#' \describe{
#' \item{sex:}{sex of the budworm.}
#' \item{dose:}{dose of the insecticide trans-cypermethrin (in micro grams)}.
#' \item{ndead:}{budworms killed in a trial.}
#' \item{ntotal:}{total number of budworms exposed per trial.}
#' }
#'
#' @references Venables, W.N; Ripley, B.D.(1999) Modern Applied Statistics with
#' S-Plus, Heidelberg, Springer, 3rd edition, chapter 7.2
#'
#' @source Collett, D. (1991) Modelling Binary Data, Chapman & Hall, London,
#' Example 3.7
#'
#' 
#' @keywords datasets
#' @examples
#' 
#' data(budworm)
#' 
#' ## function to caclulate the empirical logits
#' empirical.logit<- function(nevent,ntotal) {
#'    y <- log((nevent + 0.5) / (ntotal - nevent + 0.5))
#'    y
#' }
#' 
#' 
#' # plot the empirical logits against log-dose
#' 
#' log.dose <- log(budworm$dose)
#' emp.logit <- empirical.logit(budworm$ndead, budworm$ntotal)
#' plot(log.dose, emp.logit, type='n', xlab='log-dose',ylab='emprirical logit')
#' title('budworm: emprirical logits of probability to die ')
#' male <- budworm$sex=='male'
#' female <- budworm$sex=='female'
#' lines(log.dose[male], emp.logit[male], type='b', lty=1, col=1)
#' lines(log.dose[female], emp.logit[female], type='b', lty=2, col=2)
#' legend(0.5, 2, legend=c('male', 'female'), lty=c(1,2), col=c(1,2))
#' 
#' \dontrun{
#' * SAS example;
#' data budworm;
#' infile 'budworm.txt' firstobs=2;
#' input sex dose ndead ntotal;
#' run;
#' }
#' 
#' 
"budworm"
