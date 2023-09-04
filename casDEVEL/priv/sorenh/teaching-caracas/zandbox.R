library(caracas)
def_sym(x1, x2, x3)

g1 <- x1 - x2 + 1
g2 <- x1^2 - x2 + 1
G <- cbind(g1, g2)
x <- cbind(x1, x2)

solve_sys(G, x)

H <- G %*% t(G)
gH <- der(H, x)

gH. <- as_func(gH, vec_arg = T)
H.  <- as_func(H, vec_arg = T)
gamma <- 0.001
x. <- c(10, 10)
gH.(x.)
H.(x.)

x1 <- x. - gamma * gH.(x.)

H.(x1)

for (i in 1:1000000){
x. <- x. - gamma * gH.(x.)
}
x.











g1 <- 3*x1 - cos(x2*x3) - 3/2
g2 <- 4*x1^2 - 625*x2^2 -2*x2-1
g3 <- exp(-x1*x2) + 20*x3 + (10 * pi - 3)/3


G <- cbind(g1, g2, g3)

    
x <- cbind(x1, x2, x3)

sol <- solve_sys(cbind(g1, g2, g3), cbind(x1, x2, x3))
s

J <- t(der(cbind(g1, g2, g3), cbind(x1, x2, x3)))

x. <- c(0, 0)
gamma0 <- 0.01
for (i in 1:10000){
    x. <- x. -  as_expr(gamma0 * subs(gH, x, x.))
    ## print(as.numeric(x.))
    subs(H, x, x.) |> as_expr()  |> as.numeric() |> print() 
}



subs(G, x, x.)

x. <- x. -  as_expr(gamma0 * subs(gH, x, x.))
x.

subs(J, x, x0.)
subs(G, x, x0.)


gH. <- doBy::expr_to_fun(as_expr(gH))


do.call(gH., list(1,2))


eval(gH., list(x1=1,x2=2))








## CONSTRUCT function from expression

library(caracas)
def_sym(y,u,v)
x <- y + u^2 + v^3


e <- as_expr(x)




e <- expression(y <- y+4, v <- v + 1, u^2+v^2+y)
eval(e, list(u=2, v=1, y=-4))

expr_to_mpfun(e)(1,2,3)

a <- expr_to_opfun(e)
(1,2,3)


ee <- expression(1+2)
expr_to_fun2(ee, T)

expr_to_fun2 <- function(expr_, vec_arg=FALSE){

    if (vec_arg){
        expr_to_opfun(expr_)
    } else {
        expr_to_mpfun(expr_)
    }
    
}

expr_to_opfun <- function(e){
    nms <- all.vars(e)
    e_str <- lapply(e, deparse)
    if (length(nms)) {
        aux <- sapply(1:length(nms),
                      function(i) {
                          nm <- nms[i]
                          paste0(nm, " = parm[", i, "]")
                      }
                      )
    } else {
        aux <- NULL
    }

    comb <- c(aux, e_str)    
    
    fun_str <- "function(parm)"
    
    bd <- paste0("\n{ \n", paste0(comb, collapse=";\n "), "\n}")
    bd
    ff <- paste0(fun_str, bd)
    fun <- eval(parse(text=ff))
    return(fun)
}


expr_to_mpfun <- function(e){
    nms <- all.vars(e)
    e_str <- lapply(e, deparse)

    fun_str <- paste0("function(", paste0(nms, collapse=", "), ")")
    
    
    bd <- paste0("\n{ \n", paste0(e_str, collapse=";\n "), "\n}")
    bd
    ff <- paste0(fun_str, bd)
    fun <- eval(parse(text=ff))
    return(fun)
}





doBy::expr_to_fun(e)



nms

aux <- sapply(1:length(nms),
       function(i) {
           nm <- nms[i]
           paste0(nm, " = parm[", i, "]")
       }
)

bd <- body(doBy::expr_to_fun(e))
bd <- deparse(bd)

s <- "function(parm)"

comb <- c(aux, bd)


ff <- paste0(s, "\n{\n", paste0(comb, collapse=";\n "), "\n}")
fun <- eval(parse(text=ff))
fun

fun(c(1,2,4))







arg <- vector("list", length(vn))
names(arg) <- vn


vn <- all.vars(e)
arg <- vector("list", length(vn))
names(arg) <- vn

function(fmls){

    
}



a <- function(){
    z <- 1
    u <- 10
    z+u
    
}

body(a)


fun <- f

as.function(c(formals(f), expression(e, body(f))))


ff <- function (fun, args, envir = parent.frame()) 
{
    body1 <- as.expression(body(fun))
    body2 <- do.call("substitute", list(body1[[1]], args))
    fmls <- formals(fun)
    idx <- match(names(args), names(fmls))
    idx <- idx[!is.na(idx)]
    if (length(idx) > 0) {
        fmls <- fmls[-idx]
    }
    out <- as.function(c(fmls, body2), envir = envir)
    environment(out) <- environment(fun)
    out
}



e <- expression({
x <- 123
y <- 321
z <- x+y
z
})

eval(e)

f <- function(x,y){
    x <- x^2
    x + y
}

fmls <- formals(f)
body(f) |> str()


nms <- fmls |> names()

aux <- sapply(1:length(nms),
       function(i) {
           nm <- nms[i]
           paste0(nm, " = parm[", i, "]")
       }
)

e <-

    e[2:(length(e)-1)]


e2 <- deparse(body(f))
e2 <- e2[2:(length(e2)-1)]

comb <- as.expression(c(aux, e2))

bd <- parse(text=comb)


f <- function(parm){
    eval(bd, list(parm=parm))
}

f(c(1,2))

out <- function(parm){
    
}

body(out) <- aux

as.function(list("parm", bd))


e2 <- body(f) |> as.expression()


e <- c("z <- x+y", "z")

bd <- c(aux,e)



parse(text=bd)









paste0("{", paste0(bd), "}")

f <- eval(parse(text=s))



arg_list <- 

g <- function(arg, e){
    ## print(arg)
    arg <- as.list(arg)
    names(arg) <- all.vars(e)
    arg
}

arg_list <- g(c(2,3,5), e)


fun <- function(arg){
    
}



expr2fun <- function (e) 
{
    vn <- all.vars(e)
    fmls <- vector("list", length(vn))
    names(fmls) <- vn
    out <- function() {
    }
    formals(out) <- fmls
    body(out) <- e
    return(out)
}



f

h <- function(arg){
    ## arg <- as.list(arg)
    
    p <- formals(f)
    p <- lapply(1:length(p), function(i) {p[[i]]=arg[[i]]})
    names(p) <- names(formals(f))
    do.call(f, p)
}

uu <- function(){
    out <- function(arg) {
        p <- formals(f)
    }

    body(out) <- {body(out); e}
    out    
}


uu()




h(c(1,2,3))


do.call(f, arg_list)
f(arg)


f <- expr2fun(e)
do.call(f, list(u=1, y=2, v=4))



expr2fun <- function (e) 
{
    vn <- all.vars(e)
    fmls <- vector("list", length(vn))
    names(fmls) <- vn
    out <- function() {
    }
    formals(out) <- fmls
    body(out) <- e
    return(out)
}











n <- 4
e <- as_sym(paste0("e", 1:n))
x <- as_sym(paste0("x", 1:n))

def_sym(v, a)

L <- diff_mat(n, "-a")
L[1,1] <- "sqrt(1-a^2)"




library(caracas)
def_sym(a)
m <- matrix_(c(1,2,3,1), nrow=2)
m[1,1] <- a^2 ## FAILS
m[1,1] <- as.character(a^2) ## WORKS
m

subs(m, "v11", a^2)



K <- crossprod_(L) / v
V <- inv(K) %>% simplify()

logL <- log(det(K)) - t(x) %*% K %*% x

xt <- c(0.1, -0.9, 0.4, .0)
logL. <- subs(logL, x, xt) 
logL.



## as_factor_list((1-a^2), simplify(V * (1-a^2)))






V <- v * diag_(c(1/(1-a^2), rep(1,n-1)))






xx <- as_sym(paste0("x", 0:3))

u <- vector_sym(3, "u")
y <- vector_sym(3, "y")
eu <- rbind(e, u)
xy <- rbind(x, y)
L <- diff_mat(3, "-a")
def_sym(v, a)
K <- crossprod_(L) / v

b <- as_sym(c("-a*x0", 0, 0))


logL <- log(det(K)) - t(x) %*% K %*% x


load_all("../caracas")
xt <- c(0.1, -0.9, 0.4, .0)
logL. <- subs(logL, x, xt) 
logL.


parm <- list(a, v)
S <- der(logL., parm) |> simplify()
sol <- solve_sys(S, parm)
sol

g <- doBy::expr_to_fun(as_expr(logL.))
g_wrap <- function(par){
    g(par[1], exp(par[2]))
}
par <- optim(c(0, 0), g_wrap, control=list(fnscale=-1))$par
par[2] <- exp(par[2])
names(par) <- names(formals(g))
par


a1 <- arima(xt, order = c(1, 0, 0), include.mean = FALSE, method = "ML")
a1




































DD <- cbind(as_sym(c("-a", 0, 0)), diff_mat(3, "-a"))

y <- vector_sym(4, "y")

DD %*% y

(t(DD) %*% DD)  %>% inv


n <- 4
x <- vector_sym(n, "x")
def_sym(a)

x[1:(n-1)]



r <- x[2:n] - a * x[1:(n-1)]

r %*% t(r)



def_sym(xx, yy, zz)
s <- xx + yy + zz
subs(s, c(xx,yy), c(2,3)) ## Slow
subs(s, c(xx,yy), c(2,xx)) ## Slow


x <- c(xx,yy)
v <- c(2,3)
v <- c(2,xx)

## FIXME: MIKKEL a hack because as_character_matrix gives space in names
x.name <- gsub(" *", "", c(as_character_matrix(x))) 

vv <- as.list(v)
if (!(length(x.name) == length(vv)))
    stop("'x' and 'v' do not have same length")


oo <- sapply(seq_along(vv), function(i){
    paste0("(", x.name[i], ", ", vv[[i]], ")")
})


st <- paste0("[", paste(oo, collapse=", "), "]")

r <- caracas:::eval_to_symbol(st)
r

v2 <- s$pyobj$subs(r$pyobj)
v2
y <- caracas:::construct_symbol_from_pyobj(v2)



## FORSØG FRA I GÅR

matrix_to_list <- function(v){
    if (ncol(v) == 1L) {
        vv <- lapply(seq_len(nrow(v)), function(i) v[i, ])
    } else if (nrow(v) == 1L) {
        vv <- lapply(seq_len(ncol(v)), function(i) v[, i])
    } else {
        stop("When v is a caracas matrix, one dimension must be 1")
    }
    vv
}

get_val <- function(v){
    if (inherits(v, "caracas_symbol")) {
        v$pyobj
    } else {
        v
    }
}

## Dette er for langsomt
subs_vec <- function(s, x, v){
    ## caracas::ensure_sympy()
    ## stopifnot_symbol(s)

    if (is.character(x)){
        x <- as_sym(matrix(x))
    }
    
    if (any(dim(x) > 1)){
        x <- as_sym(matrix(c(as_character_matrix(x))))
    }
    
    if (is.character(v) || is.numeric(v)){
        v <- as_sym(v)
    }
    
    if (any(dim(v) > 1)){
        v <- as_sym(c(as_character_matrix(v)))      
    }
    
    ## stopifnot_matrix(x)
    
    vv <- v
    
    if (inherits(v, "caracas_symbol") && symbol_is_matrix(v)) {
        vv <- matrix_to_list(v)
    } 
        

    py <- s$pyobj
    for (i in seq_along(vv)){
        sym <- as.character(x[i])
        val <- get_val(vv[[i]])
        py$subs(sym, val)
    }

    y <- caracas:::construct_symbol_from_pyobj(py)
    return(y)
}






