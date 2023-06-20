## ##########################################################
##  Modify a function programmatically
##  Example:
##    ff  <- function(a,b=2,c=4){a+b+c}
##  is specialized to function in which a is replaced by 1, i.e.
##    ff1 <- function(b=2,c=4){1+b+c}
## ##########################################################

specialize <- function(ff, vals){
	expr1 <- as.expression(body(ff))
	expr2 <- do.call("substitute", list(expr1[[1]], vals))
	gg  <- formals(ff)
	idx <-match(names(vals), names(gg))
	idx <- idx[!is.na(idx)]
	if (length(idx)>0){	gg  <- gg[-idx]}
	as.function(c(gg, expr2))
}

## Example
ff  <- function(a,b=2,c=4){a+b+c}
ff1 <- specialize(ff, vals=list(a=1,bb=123))
gg  <- rnorm
gg1 <- specialize(gg, list(n=10))

## Here specialize fails, but Curry works
f  <- function(a) {a <- a + 1; a}
f1 <- specialize(f, list(a = 10))
f1()
library(functional)
Curry(f, a=10)()

## Benchmarking
library(functional)
ff  <- function(a,b=2,c=4){a+b+c}
ff1 <- specialize(ff, vals=list(a=1,bb=123))
ff2 <- Curry(ff, a = 1)
rbenchmark::benchmark(ff1(b=10), ff2(b=10), replications=100000)

gg <- rnorm
gg1 <- specialize(gg, list(n=10))
gg2 <- Curry(gg, n=1000)
rbenchmark::benchmark(gg1(), gg2(), replications=100000)






















