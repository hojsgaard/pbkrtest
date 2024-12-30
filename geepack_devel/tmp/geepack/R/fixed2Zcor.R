##
## Generate zcor vector from 
## 1) fixed correlation matrix
## 2) id information
## 3) waves information

## The zcor-vector contrains entries only for clusters 
## of size larger than 1 

#' @title Construct zcor vector
#' 
#' @description Construct zcor vector (of fixed correlations) from a fixed
#'     working correlation matrix, a specification of clusters and a
#'     specifcation of waves.
#' 
#' @param cor.fixed Matrix
#' @param id Clusters
#' @param waves Vector giving the ordering of observations within clusters.
#' @return A vector which can be passed as the zcor argument to geeglm.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{genZcor}}, \code{\link{geeglm}}
#' @keywords regression
#' @examples
#' 
#' timeorder <- rep(1:5, 6)
#' tvar      <- timeorder + rnorm(length(timeorder))
#' idvar <- rep(1:6, each=5)
#' uuu   <- rep(rnorm(6), each=5)
#' yvar  <- 1 + 2*tvar + uuu + rnorm(length(tvar))
#' simdat <- data.frame(idvar, timeorder, tvar, yvar)
#' head(simdat,12)
#' 
#' simdatPerm <- simdat[sample(nrow(simdat)),]
#' simdatPerm <- simdatPerm[order(simdatPerm$idvar),]
#' head(simdatPerm)
#' 
#' cor.fixed <- matrix(c(1    , 0.5  , 0.25,  0.125, 0.125,
#'                       0.5  , 1    , 0.25,  0.125, 0.125,
#'                       0.25 , 0.25 , 1   ,  0.5  , 0.125,
#'                       0.125, 0.125, 0.5  , 1    , 0.125,
#'                       0.125, 0.125, 0.125, 0.125, 1     ), nrow=5, ncol=5)
#' cor.fixed
#' 
#' zcor <- fixed2Zcor(cor.fixed, id=simdatPerm$idvar, waves=simdatPerm$timeorder)
#' zcor
#' 
#' mod4 <- geeglm(yvar~tvar, id=idvar, data=simdatPerm, corstr="fixed", zcor=zcor)
#' mod4
#' 
#' @export fixed2Zcor
fixed2Zcor <- function(cor.fixed, id, waves){
  zcor <- NULL
  cnt  <- 1
  uniq.id <- unique(id)
  for (ii in uniq.id){
    cwaves <- waves[id==ii]
    if (length(cwaves) > 1) {
      for (kk in 1:(length(cwaves) - 1)){
          for (mm in (kk + 1):length(cwaves)){
              vvv <- cor.fixed[cwaves[mm], cwaves[kk]]
              zcor<-c(zcor, vvv)             
          }
      }
    }
  }
  zcor
}
