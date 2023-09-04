library(caracas)
library(magrittr)



f <- function(x){
  a * x + b * x^2 + c * sin(x^2)
}

Deriv::Deriv(f, "x")





## https://cran.r-project.org/web/packages/mosaicCalc/vignettes/Calculus_with_R.html

## http://www2.uaem.mx/r-mirror/web/packages/mosaic/vignettes/Calculus.pdf
## https://cran.microsoft.com/snapshot/2015-03-19/web/packages/mosaic/vignettes/V6Calculus.pdf
##https://www.r-bloggers.com/2019/06/calculus-in-r/


def_sym("a", "b", "c", "x")

f <- a * x + b * x^2 + c * sin(x^2)

FF <- intf(f, x) %>% simplify()

limf(f, x, -Inf)

der(FF, x)


PP <- matrix(c(-1, 1, 1, 0, 0, 2, 2, -1), ncol=2)
WW <- matrix(0, nrow=4, ncol=2)
t(PP)
t(WW)

## atomic colour classes
atom   <- which(PP == -1, arr.ind=TRUE)
for (i in 1:nrow(atom)){
    WW[atom[i,, drop=FALSE]] <- paste0("a_", i, "")
}


## composite colour classes
comp   <- which(PP > 0, arr.ind=TRUE)
compid <- unique(PP[comp])

for (i in seq_along(compid)){
    WW[which(PP == compid[i], arr.ind=TRUE)] <- paste0("b_", i, "")
}

t(WW)

sym <- setdiff(unique(c(WW)), "0")
sym

for (i in seq_along(sym))
    assign(sym[i], symbol(sym[i]))

WW <- as_symbol(WW)


rbind(diag(1, 2), WW)

MM <- diag(1, 6)
MM[-c(1,2), 1:2] <- WW
MM <- as_symbol(MM)
