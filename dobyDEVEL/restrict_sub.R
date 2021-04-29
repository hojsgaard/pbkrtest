






































## #' @rdname restrict
## #' @export
## restrict <- function(fun, args, method="env", envir=parent.frame()){
##   method <- match.arg(method, c("env", "sub"))
##   if (identical(method, "env"))
##     restrict_env(fun, args)
##   else
##     restrict_sub(fun, args, envir)
## }


## #' @rdname restrict
## #' @export
## restrict_env <- function(fun, args, envir=parent.frame()){

##   env <- environment(fun)
##   rlang::env_print(env)

##   form <- formals(fun)
##   idx <- match(names(args), names(form))
##   idx <- idx[!is.na(idx)]
##   if (length(idx) > 0){ form  <- form[-idx]}

##   fun2 <- fun
##   formals(fun2) <- form

##   print("KKKKKKKKKKKKKKKKKK")

##   if (!identical(envir, globalenv())){
##     print(1)
##     envir <- list2env(args, env, parent=envir)
##   }
##   else {
##     print(2)
##     envir <- list2env(args)
##   }
  
## print("LLLLLLLLL")
##   print(fun2)
  
##   ## rlang::env_print(envir)
##   environment(fun2) <- envir
##   fun2
## }




























## #' @rdname restrict
## #' @export
## restrict_sub <- function(fun, args, envir=parent.frame()){
##   body1 <- as.expression(body(fun))
##   body2 <- do.call("substitute", list(body1[[1]], args))

##   form  <- formals(fun)
##   idx <- match(names(args), names(form))
##   idx <- idx[!is.na(idx)]
##   if (length(idx) > 0){ form  <- form[-idx]}

##   as.function(c(form, body2), envir=envir)
## }


## #' @rdname restrict
## #' @export
## restrict_env <- function(fun, args){

##   envir <- environment(fun)
##   rlang::env_print(envir)

##   form <- formals(fun)
##   idx <- match(names(args), names(form))
##   idx <- idx[!is.na(idx)]
##   if (length(idx) > 0){ form  <- form[-idx]}
  
##   formals(fun) <- form

##   print("KKKKKKKKKKKKKKKKKK")
  
##   if (!identical(envir, globalenv()))
##     envir <- list2env(args, envir=envir)
##   else
##     envir <- list2env(args)


##   print(fun)
  
##   ## rlang::env_print(envir)
##   environment(fun) <- envir
##   fun
## }














# 
# f2 <- function(x){x+7}
# f2_ <- curry::partial(f2, list(x=10))
# f2_() # Jeg ville forvente 17
# 
# f1 <- function(x, y){x+y}
# f1_ <- curry::partial(f1, list(x=10))
# f1_(7)
# 


## restrict <- function(fun, args){
##   expr1 <- as.expression(body(fun))
##   expr2 <- do.call("substitute", list(expr1[[1]], args))
##   gg  <- formals(fun)
##   idx <- match(names(args), names(gg))
##   idx <- idx[!is.na(idx)]
##   if (length(idx)>0){ gg  <- gg[-idx]}
##   as.function(c(gg, expr2))
## }
