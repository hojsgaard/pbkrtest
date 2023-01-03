library(doBy)

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

dput(M) |> paste0() 

a<-"structure(c(1.29339876191411, -1.81546438032534, -0.0531616919995241, 
-0.268711079463816, -0.78809966270799, -0.758257164056775, 0.320461758972395, 
0.60718180278418, -0.468573756915408, 1.81686743162958, -0.38449538606822, 
0.380886889724529, 0.421503577064559, -1.29526215407802, 0.791266230499853, 
-0.207151352267576, 0.364764053389896, 0.0856228291949521, 1.43125070124485, 
-1.06080588259864, -1.50273103080406, -0.776735157046879, -0.348504643875237, 
-1.28739372257964, -0.96856129444401, 0.0318569763740708, -0.17162920808234, 
-0.733030285773063, -0.983911309366191, 0.499787119586348, 0.237752850606953, 
1.23450642163246, 0.47920405336186, -1.11475189975074, 0.384438242436412, 
-1.63358249312713, 0.43944392386847, 0.997663435985119, 1.13063902311667, 
-1.6290722467067, -0.00619449178359357, -0.372940074279609, -0.380250579331469, 
-0.257379937844891, 0.324982777514084, -1.77039807276043, 0.537757419871525, 
-1.96391917524118, -0.162639815760299, 0.654240787248413, -0.683624438499659, 
-0.126704229379784, 0.1847046505433, -1.86060690283218, -0.176994438758492, 
1.26594470186483, -0.671158134922846, 1.22148059118848, 0.606987149302044, 
-1.90160522551914, -1.25357112517732, 0.17045781193866, 0.424943781687776, 
0.578105844199386, 0.840471226048932, 0.637701428360776, -0.893835127478572, 
-0.436111181477396, 0.205845022424686, -0.740143102850157, -1.85828693720142, 
-1.77917426865762, -0.288322661831847, 0.598963104786103, -1.98306375615202, 
0.170515882917581, 0.453594901981834, 0.499380214369774, 1.3256302796512, 
1.49818485540525, -0.160080024683796, 1.13660785214363, 0.39005954450314, 
0.128524733207781, -1.81907826184467, -1.15354234028343, -1.67650935159631, 
0.0345195363969644, -0.309181878727179, -0.171294712420814, -0.154484889391947, 
-1.81900584810344, 0.12804075212672, -0.133770987623703, 0.875725300780063, 
-0.632985244190984, -1.49169721135088, -0.261231563078657, 1.46174771414603, 
1.77668108012506), dim = c(10L, 10L))"


f <-function (x1, x2, y, n, b1, b2) 
{
    matrix(c(x1 * (-n * exp(b1 * x1 + b2 * x2) + y * exp(b1 * 
        x1 + b2 * x2) + y)/(exp(b1 * x1 + b2 * x2) + 1), x2 * 
        (-n * exp(b1 * x1 + b2 * x2) + y * exp(b1 * x1 + b2 * 
            x2) + y)/(exp(b1 * x1 + b2 * x2) + 1)), nrow = 2)
}

set_default(f, nms, c(2,1,2,5))

nms <- c("x1", "x2", "y", "n")
vls <- c(1,2,3,4)
load_all("dobyDEVEL/_doby")




f <-function (xxxxxxxxxx1, xxxxxxxxxx2, y, n, b1, b2) 
{
    matrix(c(xxxxxxxxxx1 * (-n * exxxxxxxxxxp(b1 * xxxxxxxxxx1 + b2 * xxxxxxxxxx2) + y * exxxxxxxxxxp(b1 * 
        xxxxxxxxxx1 + b2 * xxxxxxxxxx2) + y)/(exxxxxxxxxxp(b1 * xxxxxxxxxx1 + b2 * xxxxxxxxxx2) + 1), xxxxxxxxxx2 * 
        (-n * exxxxxxxxxxp(b1 * xxxxxxxxxx1 + b2 * xxxxxxxxxx2) + y * exxxxxxxxxxp(b1 * xxxxxxxxxx1 + b2 * 
            xxxxxxxxxx2) + y)/(exxxxxxxxxxp(b1 * xxxxxxxxxx1 + b2 * xxxxxxxxxx2) + 1)), nrow = 2)
}

nms <- c("xxxxxxxxxx1", "xxxxxxxxxx2", "y", "n")
vls <- c(1,2,3,4); vls <- vls/100000
section_fun(f, nms, vls)()

load_all("dobyDEVEL/_doby")





set_default(f, nms, c(2,1,2,5))


fun <- f



set_default <- function(fun, nms, vls){
    fmls <- formals(fun)
    
    i <- match(nms, names(fmls))
    j <- c(setdiff(seq_along(fmls), i), i) # new order
    
    fmls <- fmls[j]
    fmls[nms] <- vls
    formals(fun) <- fmls
    fun    
}





1
















































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
