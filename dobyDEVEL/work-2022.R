library(doBy)

new_fun <- function(x,y) {}    
body(new_fun) <- bquote(
{
    sprintf("## use get_section(function_name) to see section")
    sprintf("## use get_fun(function_name) to see original function")
    x+10
})

new_fun
new_fun(10,1)

f <- function(x, y, z){
    x+y+z
}

section_fun(f, list(y=1:10,z=1:4))(1)


fv <- Vectorize(f, vectorize.args = c("y", "z"))

sf <- section_fun(fv, list(y=1:10), meth="env")

sf(1)


inv <- function(A){solve(A)}

nvar <- 300
V <- cov2cor(cor(matrix(rnorm(20000 * nvar), ncol=nvar)))

args <- list(A=V)
inv_sub <- section_fun(inv, args)
inv_def <- section_fun(inv, args, method="def")
inv_env <- section_fun(inv, args, method="env")

microbenchmark(
    sub=section_fun(inv, args),
    def=section_fun(inv, args, method="def"),
    env=section_fun(inv, args, method="env"),
    times=5
)

library(microbenchmark)
microbenchmark(
    sub=inv_sub(),
    def=inv_def(),
    env=inv_env(),
    times=5
)





(inv_sub() %*% V)  |> zapsmall()
(inv_def() %*% V)  |> zapsmall()
(inv_env() %*% V)  |> zapsmall()






library(doBy)


M<-matrix(rnorm(100),nrow = 10)







































f2 <- function(x, y, z){
  x <- x + 2 + y
  x
}

fff(f2, args)



args <- list(x = 1)
fmls

    
fmls  <- formals(fun)
idx <- match(names(args), names(fmls))
idx <- idx[!is.na(idx)]
if (length(idx) > 0){
    fmls  <- fmls[-idx]
}






fun2(10, 20)

section_fun(fun, args)(10, 20)



paste0("\n ## section\n ", , "\n ## section (end)")



section_fun_rpl <- function(fun, nms, vls, envir=parent.frame()){

    if (inherits(nms, "list")){
        if (!missing(vls)) {
            warning("vls ignored")
        }
        args <- nms
    } else {
        args <- nms_vls_to_list(nms, vls)
    }
    
    fmls  <- formals(fun)
    idx <- match(names(args), names(fmls))
    idx <- idx[!is.na(idx)]
    if (length(idx) > 0){
        fmls  <- fmls[-idx]
    }

    hd <- paste0("function(", paste0(names(fmls), collapse=", "), ")")
    hd
    
    aux <- sapply(1:length(args),
                  function(i){
                      nm <- names(args)[i]
                      paste0(nm, " = ", deparse(args[[i]]))
                  })
    
    
    bd1 <- paste0("\n ## section\n ", paste0(aux, collapse=";\n "), "\n ## section (end)")
    bd1
    
    bd2 <- deparse(body(fun))
    bd2 <- bd2[2:(length(bd2) - 1)]
    bd2 <- gsub("^ *", "", bd2) ## Remove leading whites
    bd2 <- paste0(paste0(bd2, collapse=";\n "))
    bd2
    
    bd <- paste0("\n{ ", paste0(c(bd1, bd2), collapse=";\n "), "\n}")
    
    ff <- paste0(c(hd, bd), collapse="")
    out <- eval(parse(text=ff))
    out
    environment(out) <- environment(fun)
    out
}


f4 <- function(A, B, D) {
  A + B + D
}
fun <- f4
args <- list(A = matrix(1:4, nrow=2), D=4)





(f4, list(A = matrix(1:4, nrow=2)), method="rpl")


















ee <- expression(b1 + (b0 - b1)*exp(-k*x) + b2*x)
ff <- expr_to_fun(ee)


ee2 <- expression(b1 <- exp(b1), b1 + (b0 - b1)*exp(-k*x) + b2*x)
ff2 <- expr_to_fun(ee2)


ee_text <- sapply(ee2, deparse)

header_text <- deparse(out)[1]

ff <- paste0(header_text, "\n{\n", paste0(ee_text, collapse=";\n "), "\n}")
fun <- eval(parse(text=ff))



deparse(ee2[[1]])



deparse(parse(text=ee2))


expr_to_fun2 <- function(ee){
    nms <- all.vars(ee)
    fmls <- vector("list", length(nms))
    names(fmls) <- nms
    
    out <- function(){}
    formals(out) <- fmls

    header_text <- deparse(out)[1] ## function(....)
    ee_text <- sapply(ee, deparse)
    ff <- paste0(header_text, "\n{\n ", paste0(ee_text, collapse=";\n "), "\n}")
    fun <- eval(parse(text=ff))
    return(fun)
}


expr_to_fun3 <- function(ee){
    nms <- all.vars(ee)
    fmls <- vector("list", length(nms))
    names(fmls) <- nms

    aux <- sapply(1:length(nms),
                  function(i) {
                      nm <- nms[i]
                      paste0(nm, " = parm[", i, "]")
                  }
                  )

    
    out <- function(parms){}


    header_text <- deparse(out)[1] ## function(....)
    ee_text <- sapply(ee, deparse)
    aux_ee <- c(aux, ee_text)
    ff <- paste0(header_text, "\n{\n ", paste0(aux_ee, collapse=";\n "), "\n}")
    fun <- eval(parse(text=ff))
    return(fun)
}


expr_to_fun2(ee2)










crime_rate <- doBy::crimeRate
rownames(crime_rate) <- crime_rate$State
crime_rate$State <- NULL
save(crime_rate, file="crime_rate.RData")


nir_milk <- doBy::NIRmilk
nir_milk$sample <- NULL

y <- nir_milk[,c("fat", "protein", "lactose", "dm")]
x <- nir_milk[,-match(c("fat", "protein", "lactose", "dm"), names(nir_milk))]

nir_milk <- list(x=x, y=y)
save(nir_milk, file="nir_milk.RData")














library(tibble)
library(doBy)
load_all()

x <- iris
y <- 10+(1:3)
names(y) <- letters[1:3]

x %>% sqb(1)
x %>% sqb(1:2)

x[[1]]

g"["(x, 2)



"["(x, "b")



gt(iris, "Sepal.Length")
gt(iris, 2)

gt(x, "a")

gt(x, 2)
