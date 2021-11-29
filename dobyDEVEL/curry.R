



## .partial <- function(fun, args) {
##     ## fmls_names <- names(formals(fun))
##     ## if (!'...' %in% fmls_names && !all(names(args) %in% fmls_names)) {
##     ##     stop('The provided arguments to ',
##     ##          deparse(substitute(fun, parent.frame())),
##     ##          ' does not match its definition',
##     ##          call. = FALSE)
##     ## }
##     .apply_args(fun, args)
## }


## ' dobq <- function(fnlist){
## '   lapply(fnlist, function(g) bquote(.(g)()))
## ' }
## ' 
## ' a1 <- dobq(b1.list)
## ' a2 <- dobq(b2.list)


## args <- list(y=10, u=222, z=-5)

## arg_env<-new.env()
## arg_env$args <- list(x=1, y=10, z=100)
## rlang::env_print(arg_env)$args

## common <- intersect(names(arg_env$args), names(args))
## if (length(common)>0)
##   arg_env$args[common] <- NULL
## assign('args', append(arg_env$args, args), envir=arg_env)
## arg_env$args


## env$args




## env$x <- 1
## env$y <- 10
## env$z <- 100






## is.scaffold <- function(fun)
##   inherits(fun, 'scaffold')

## as.scaffold <- function(fun) {

##   if (is.scaffold(fun)) {
##     fun
##   } else {
##     from <- parent.frame()
##     scaffold(fun, from)
##   }
## }

## scaffold <- function(fun, from = parent.frame()) {

##   arg_env <- new.env(parent = emptyenv())
##   assign('args',     list(), envir = arg_env)
##   assign('args_end', list(), envir = arg_env)

##   fmls <- get_formals(fun)
##   arg_getter <- getArgs(arg_env)

##   do_scaffold(fun, arg_env, from)  
##   ## new_fun <- function() {}    
##   ## formals(new_fun) <- fmls
##   ## body(new_fun) <- bquote({
##   ##     args <- arg_getter()
##   ##     do.call(.(fun), args)
##   ##   }, list(fun = substitute(fun, from)))

##   ## structure(new_fun, class = 'scaffold', arg_env = arg_env)
## }
