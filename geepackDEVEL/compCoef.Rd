\name{compCoef}
\alias{compCoef}
\title{
Compare Regression Coefficiente between Nested Models
}
\description{
Comparing regression coefficients between models
when one model is nested within another for clustered data.
}
\usage{
compCoef(fit0, fit1)
}
\arguments{
  \item{fit0}{
    a fitted object of class \code{geese}
  }
  \item{fit1}{
    another fitted object of class \code{geese}
  }
}
\value{
  a list of two components:
  \item{delta}{estimated difference in the coefficients of common
    covariates from \code{fit0} and \code{fit1}}
  \item{variance}{estimated variance matrix of delta}
}
\references{
  Allison, P. D. (1995). The impact of random predictors on comparisons
  of coefficients between models: Comment on Clogg, Petkova, and
  Haritou.  \emph{American Journal of Sociology}, \bold{100}(5),
  1294--1305.

  Clogg, C. C., Petkova, E., and Haritou, A. (1995). Statistical methods
  for comparing regression coefficients between models.
  \emph{American Journal of Sociology}, \bold{100}(5), 1261--1293.

  Yan, J., Aseltine, R., and Harel, O. (2011). Comparing Regression
  Coefficients Between Nested Linear Models for Clustered Data with
  Generalized Estimating Equations. \emph{Journal of Educational and
  Behaviorial Statistics}, Forthcoming.  
}
\author{
  Jun Yan \email{jyan.stat@gmail.com}
}
\examples{
## generate clustered data
gendat <- function(ncl, clsz) {
## ncl: number of clusters
## clsz: cluster size (all equal)
  id <- rep(1:ncl, each = clsz)
  visit <- rep(1:clsz, ncl)
  n <- ncl * clsz
  x1 <- rbinom(n, 1, 0.5) ## within cluster varying binary covariate
  x2 <- runif(n, 0, 1)   ## within cluster varying continuous covariate
  ## the true correlation coefficient rho for an ar(1)
  ## correlation structure is 2/3
  rho <- 2/3
  rhomat <- rho ^ outer(1:4, 1:4, function(x, y) abs(x - y))
  chol.u <- chol(rhomat)
  noise <- as.vector(sapply(1:ncl, function(x) chol.u \%*\% rnorm(clsz)))
  y <- 1 + 3 * x1 - 2 * x2 + noise
  dat <- data.frame(y, id, visit, x1, x2)
  dat
}

simdat <- gendat(100, 4)
fit0 <- geese(y ~ x1, id = id, data = simdat, corstr = "un")
fit1 <- geese(y ~ x1 + x2, id = id, data = simdat, corstr = "un")
compCoef(fit0, fit1)
}
\keyword{models}
