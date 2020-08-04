## First and second numerical derivatives
##

numderiv1 <- function(x,...)
  {
    DDx <- c(NA, (x[-c(1,2)] - x[1:(length(x)-2)]) / 2, NA)
    DDx
  }

numderiv2 <- function(x,...)
  {
    DD2x <- c(NA, x[-c(1,2)] - 2*x[2:(length(x)-1)] + x[1:(length(x)-2)], NA)
    DD2x
  }

numderiv1<-function (x, ...) 
  {
    DDx <- c(x[2]-x[1], (x[-c(1, 2)] - x[1:(length(x) - 2)])/2, x[length(x)]-x[length(x)-1])
    DDx
  }

numderiv2 <- function(x,...)
  {
    numderiv1(numderiv1(x))
  }
