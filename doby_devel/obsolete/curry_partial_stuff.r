In contrast, partial function application refers to the process of fixing a number of arguments to a function, producing another function of smaller arity. Given the definition of {\displaystyle f}f above, we might fix (or 'bind') the first argument, producing a function of type {\displaystyle {\text{partial}}(f)\colon (Y\times Z)\to N}\text{partial}(f) \colon (Y \times Z) \to N. Evaluation of this function might be represented as {\displaystyle f_{\text{partial}}(2,3)}f_\text{partial}(2, 3). Note that the result of partial function application in this case is a function that takes two arguments.

Intuitively, partial function application says "if you fix the first argument of the function, you get a function of the remaining arguments". For example, if function div stands for the division operation x/y, then div with the parameter x fixed at 1 (i.e., div 1) is another function: the same as the function inv that returns the multiplicative inverse of its argument, defined by inv(y) = 1/y.


library(rlang)
library(magrittr)
library(doBy)
rm(list=ls())

load_all("pkg")

f0 = g0 = function(x,y,z) {
  x + y + z + u
}

g1 <- restrict_sub(g0, list(z=100))

u <- 1000
g1(x=1, y=10)

g2 <- restrict_sub(g1, list(y=10000))
g2(x=1)

g3 <- restrict_sub(g2, list(x=3))
g3()



testf <- function(){

  f1 <- restrict_env(f0, list(z=100))
  f2 <- restrict_env(f1, list(y=10000))
  f3 <- restrict_env(f2, list(x=3))
  
  c(f1(x=1, y=10), f2(x=1), f3())  
}

testg <- function(){

  g1 <- restrict_sub(g0, list(z=100))
  g2 <- restrict_sub(g1, list(y=10000))
  g3 <- restrict_sub(g2, list(x=3))
  
  c(g1(x=1, y=10), g2(x=1), g3())  
}

u <- 100000
testf()
testg()













rlang::env_print(globalenv())


f1(x=1,z=100)


rlang::env_print(globalenv())






f2(x=1)





u <- 1000
fun(10,20,30)

args <- list(y=10)

ee <- environment(fun)
ee

form <- formals(fun)
idx <- match(names(args), names(form))
form <- form[-idx]

formals(fun) <- form
fun

ee <- list2env(args, envir=ee)
environment(fun) <- ee

fun(10)

fun(10, 50)



args <- list(z=1000)







arg_env <- attr(fun, 'arg_env')
arg_env
if (is.null(arg_env))
  arg_env <- new.env()


fun


arg_env <- list2env(args, envir=arg_env) 


fun(x=10,z=100)


attr(fun, "arg_env") <- arg_env


x <- 11111
f <- function()x + 1000

g <- function(){
  x <- 9
  f()  
}

f()
g()



ee2 <- list2env(list(y=1, z=10))
ss <- structure(fun, env=ee2)


  
environment(f) <- as.environment(a)

.apply_args <- function(fun, args, last = FALSE) {
    fmls <- formals(fun)
    arg_env <- attr(fun, 'arg_env')
    #arg_env <- environment(fun)
    formals(fun) <- fmls[!names(fmls) %in% names(args)]
    if (last) {
        assign('args_end', append(args, arg_env$args_end), envir = arg_env)
    } else {
        assign('args', append(arg_env$args, args), envir = arg_env)
    }
    structure(fun, class = 'scaffold', arg_env = arg_env)
}

g <- function(x){x+y+z}

e <- new.env()
e$y <- 1111

environment(g) <- e



ee <- new.env()






fun <- f
args <-a

ff <- .apply_args(f, a)

ls(list(), envir=environment(ff))

ff(10)

g <- function(x){x+y}
gg <- structure(g, arg_env=list(y=111))









## THIS WORKS
restrict <- function(fun, args){

  ee <- environment(fun)

  ## rlang::env_print(ee)
  form <- formals(fun)
  idx <- match(names(args), names(form))
  form <- form[-idx]
  
  formals(fun) <- form
  fun

  if (!identical(ee, globalenv()))
    ee <- list2env(args, envir=ee)
  else
    ee <- list2env(args)

  ## rlang::env_print(ee)
  environment(fun) <- ee
  fun
}













library("ggplot2")
library("dlstats")

x3 <- cran_stats(c("doBy", "geepack"))

x3 <- transform(x3, 
  quarter = quarters(start), year = as.integer(format(start, '%Y')))

df2 <- aggregate(downloads ~ quarter + year + package, data=x3, FUN=sum)
df2 <- transform(df2,  yr_qtr=paste0(year, "_", quarter))

ggplot(x2, aes(end, downloads, group=package, color=package)) +
  geom_line() + geom_point(aes(shape=package))  + xlab("month") + 
  ggtitle("downloads - modelling and data handling (1)")


ggplot(df2, aes(1:nrow(df2), downloads, group=package, color=package)) +
  geom_line() + geom_point(aes(shape=package))  +
  theme(axis.text.x =quarter_year) +
  ggtitle("downloads - modelling and data handling (1)")




