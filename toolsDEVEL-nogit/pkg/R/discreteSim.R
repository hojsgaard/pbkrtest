## p <- c(.1,.3,.6)
## n <- 10000



## csp <- cumsum(p)
## u <- runif(n)
## o <- order(u)
## ou <- u[order(u)]


## a1 <- 1*(ou>csp[1])
## for (ii in 2:(length(p)-1)){
##   a1 <- a1 + 1*(ou>csp[ii])
## }

## a1 <- a1 + 1
## x <- a1[o]
