
#' genZcor
#' 
#' constructs the design matrix for the correlation structures: independence,
#' echangeable, ar1 and unstructured The user will need this function only as a
#' basis to construct a user defined correlation structure: use genZcor to get
#' the design matrix Z for the unstructured correlation and define the specific
#' correlation structure by linear combinations of the columns of Z.
#' 
#' @aliases genZcor humbelbee
#' @param clusz integer vector giving the number of observations in
#'     each cluster.
#' @param waves integer vector, obervations in the same cluster with
#'     values of wave i and j have the correlation
#'     \eqn{latex}{sigma_ij}.
#' @param corstrv correlation structures:
#'     1=independence, 2=exchangeable, 3=ar1, 4=unstructured.
#' 
#' @return The design matrix for the correlation structure.
#'
#' @author Jun Yan \email{jyan.stat@@gmail.com}
#' @seealso \code{\link{fixed2Zcor}}
#' @keywords regression
#' @examples
#' 
#' # example to construct a Toeplitz correlation structure
#' #    sigma_ij=sigma_|i-j|
#' 
#' # data set with 5 clusters and maximally 4 observations (visits) per cluster
#' gendat <- function() {
#'        id <- gl(5, 4, 20)
#'        visit <- rep(1:4, 5)
#'        y <- rnorm(id)
#'        dat <- data.frame(y, id, visit)[c(-2,-9),]
#' }
#' 
#' set.seed(88)
#' dat <- gendat()
#' 
#' # generating the design matrix for the unstructured correlation
#' zcor <- genZcor(clusz = table(dat$id), waves = dat$visit, corstrv=4)
#'
#' # defining the Toeplitz structure 
#' zcor.toep     <- matrix(NA, nrow(zcor), 3)
#' zcor.toep[,1] <- apply(zcor[,c(1, 4, 6)], 1, sum)
#' zcor.toep[,2] <- apply(zcor[,c(2, 5)], 1, sum)
#' zcor.toep[,3] <- zcor[,3]
#' 
#' zfit1 <- geese(y ~ 1,id = id, data = dat,
#'                    corstr = "userdefined", zcor = zcor.toep)
#' 
#' 
#' zfit2 <- geeglm(y ~ 1,id = id, data = dat,
#'                    corstr = "userdefined", zcor = zcor.toep)
#' 
#' @export genZcor
genZcor <- function(clusz, waves, corstrv) {
  if (corstrv == 1) return (matrix(0, 0, 0))
  crs <- clusz * (clusz - 1) / 2
  if (corstrv == 2 || corstrv == 3) {
    ans <-  matrix(1, length(clusz), 1)
    ##ans <-  matrix(1, sum(crs), 1)
    colnames(ans) <- c("alpha")
  }
  else {
    id <- rep(1:length(clusz), clusz)
    z1 <- unlist(lapply(split(waves, id), crossutri))
    z2 <- unlist(crossutri(1:max(clusz)))
    z <- factor(z1,levels=unique.default(z2))
    ans <- model.matrix(~z - 1)
    znames <- paste("alpha", z2, sep = ".")
    colnames(ans) <- znames
  }
  ans
}


genZodds <- function(clusz, waves, corstrv, ncat) {
  if (corstrv == 1) return (matrix(0,0,0))
  crs <- clusz * (clusz - 1) / 2
  c2 <- ncat * ncat
  if (corstrv == 2 | corstrv == 3) {
    ans <- matrix(1, sum(crs) * c2, 1)
    colnames(ans) <- c("alpha")
  }
  else {
    id <- rep(1:length(clusz), clusz)
    z1 <- unlist(lapply(split(waves, id), crossutri))
    z2 <- unlist(crossutri(1:max(clusz)))
    z  <- factor(z1, levels = unique.default(z2))
    z  <- model.matrix(~z - 1)
    ind <- gl(sum(crs), c2)
    ans <- z[ind,]
    colnames(ans) <- paste("alpha", 1:dim(ans)[2], sep=".")
  }
  ans
}



crossutri <- function(wave) {
  n <- length(wave)
  if (n == 1) return(NULL)
  ans <- rep(0, n*(n-1)/2)
  k <- 1
  for (i in 1:(n-1))
    for (j in (i+1):n) {
      ans[k] <- paste(wave[i], wave[j], sep=":")
      k <- k + 1
    }
  ans
}


