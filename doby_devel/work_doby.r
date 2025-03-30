library(doBy)
load_all()
## load_all("_doby")

dat <- Titanic
ci_test(dat, c(1,2,3))

ll

tt1 <- gRim:::fit2way_(ll[[1]], ll[[2]], ll[[3]], ll[[4]])
tt2 <- fit2way(ll[[1]], ll[[2]], ll[[3]], ll[[4]])


library(gRbase)


tabDiv(tabOp(ll[[1]], ll[[2]]), tabMarg(ll[[1]], ll[[3]]))

t1 <- tabOp(ll[[1]], ll[[2]])
t2 <- table_op(ll[[1]], ll[[2]])

table_op(t1, t2, `-`)


tt1
tt2 <- tt2 |> table_perm_(names(dimnames(tt1)))

tt1 - tt2

ciTest(dat)

ci_test(

    
tab1 <- ll[[1]]
tab2 <- ll[[2]]
R <- ll$R    
vn <- ll$vn

tab1
R


fit2way <- function(tab1, tab2, R=NULL, vn){
    if (length(R)>0){
        tab.R <- table_marg(tab1, R)
        tmp <- table_op(tab1, tab2, op=`*`)
        out <- table_op(tmp, tab.R, op=`/`)
        return(table_perm(out, vn))        
    } else {
        tmp <- table_op(tab1, tab2, op=`*`)
        out <- tmp / sum(s)
        return(table_perm(tmp, vn))        
    }
}

NumericVector fit2way_ (const NumericVector& tab1,
                        const NumericVector& tab2,
                        const CharacterVector& R,
                        const CharacterVector& vn) {

  if (R.length() > 0){
    NumericVector tab3 = tab_marg_(tab1, R);
    NumericVector out = tab_perm_(tab_div_(tab_mult_(tab1, tab2), tab3), vn);
    return out;
  } else {
    double s = sum(tab1);
    NumericVector out = tab_perm_(tab_mult_(tab1, tab2), vn) ;
    out = out / s;
    return out;
  }
}























dd <- iris |> split_by(~Species) ## FIXME :: Need !!


tochar(Treatment, Type)
tochar(~Treatment+Type)
tochar(c("Treatment", "Type"))

wrap <- function(...){
    dots <- match.call(expand.dots = FALSE)$...
    ## dots1 <- dots[[1]]
    ## print(dots1)    
    dots2char(dots)
}


wrap(Treatment, Type)
wrap(~Treatment+Type)
wrap(c("Treatment", "Type"))




split_by_worker <- function(data., ..., omit=TRUE){ 

    is.tib <- inherits(data., "tbl_df")
    if (is.tib)
        data. = as.data.frame(data.)

    dots <- match.call(expand.dots = FALSE)$...
    rhs <- dots2char(dots)
    
    grps <- apply(data.[,rhs], 1, paste0, collapse="|")
    out <- split(data., grps)
    
    if (omit){
        rhs.idx <- match(rhs, names(data.))
        out <- lapply(out, function(d) d[, -rhs.idx])
    }

    if (is.tib) {
        out <- lapply(out, function(d) as_tibble(d))
    }

            
    groupid <- unique(data.[,rhs])
    idxvec <- split(1:nrow(data.), grps)
    
    attr(out, "groupid") <- groupid
    attr(out, "idxvec")  <- idxvec
    attr(out, "grps")    <- grps

    class(out) <- c("splitByData", "list")    
    return(out)
}

rhs <- tochar(c("Treatment", "Type"))
data. <- CO2
split_by_worker(data., Treatment, Type)
split_by_worker(data., ~Treatment+Type)
split_by_worker(data., c("Treatment", "conc"))






grps <- apply(data.[,rhs], 1, paste0, collapse="|")
out <- split(data., grps)
idxvec <- split(1:nrow(data.), grps)

rhs.idx <- match(rhs, names(data.))
lapply(out, function(d) d[, -rhs.idx])






## tochar <- function(...){

##     dots <- match.call(expand.dots = FALSE)$...
##     dots1 <- dots[[1]]

##     if (inherits(dots1, "call")){
##         dots1 <- eval(dots1)
##     }
##     if (inherits(dots1, c("formula", "call"))){
##         varRHS <- all.vars(dots1[[2]])
##         return(varRHS)
##     }

##     if (is.character(dots1)){
##         varRHS <- dots1
##         return(varRHS)        
##     }
       
##     varRHS <- lapply(seq_along(dots),
##                      function(i) {
##                          y <- if (is.symbol(dots[[i]])) {
##                                   deparse(dots[[i]])
##                               }
##                               else {
##                                   dots[[i]]
##                               }
##                          return(y)
##                      })
##     varRHS <- unlist(varRHS)
##     return(varRHS)
## }





ff <- substitute(lm(Sepal.Length~Sepal.Width))


foo <- function(f){
    substitute(f)
}

foo(lm(Sepal.Length~Sepal.Width))

iris  |> 

1

    
    class(out_list) <- c("splitByData", "list")    
    out_list



x <- splitBy(~Treatment+Type, data=CO2)















lm_expr <- substitute(lm(Sepal.Length ~ Sepal.Width, data = df))

# Later, when data is available, evaluate it
df <- iris  # Example data
ff <- eval(lm_expr)
summary(ff)  # Now the model is fitted



fun <- \(x){
    substitute(x)
}


fc <- fun(lm(Sepal.Length ~ Sepal.Width))
fc$data <- quote(iris)
eval(fc)

dd <- iris |>  split_by(~Species)


bbb <- function(data., model.){
    fc <- substitute(model.)
    print(fc)
    lapply(data., function(d){
        fc$data <- quote(d)
        eval(fc)
    })    
    
}

fff <- bbb(dd, lm(Sepal.Length ~ Sepal.Width))

fff |> map(coef)




lapply(fff, coef)






1



dat <- doBy::beets



interaction_plot(dat, sugpct ~ sow + harvest)



# Example data frame
df <- data.frame(
  yield = rnorm(10),
  sow = factor(rep(1:2, 5)),
  harvest = factor(rep(1:3, length.out = 10)),
  block = factor(rep(1:2, length.out = 10))
)

# Define your formula
df <- .data
formula <- yield ~ sow + harvest:block  # or any formula with an interaction

# Function to check for interaction and create the column
check_and_create_interaction <- function(df, formula) {
  # Extract the terms from the formula
  rhs_terms <- all.vars(formula)[-1]  # Remove the 'yield' variable
  
  # Check if the interaction term exists in the formula
  interaction_term <- grep(":", rhs_terms, value = TRUE)
  
  # If interaction term is found, create the interaction column
  if (length(interaction_term) > 0) {
    # Split the interaction term into its components
    terms <- unlist(strsplit(interaction_term, ":"))
    
    in Create the interaction column
    df$interaction_column <- interaction(df[[terms[1]]], df[[terms[2]]])
    
    # Print the updated dataframe
    print(head(df))
  }
  
  return(df)
}

# Apply the function
df <- check_and_create_interaction(df, f)

df

# Check the updated dataframe
head(df)




















interaction_plot(dat, sugpct ~ harvest + interaction(sow:block))

interaction_plot(dat, sugpct ~ sow + harvest)
interaction_plot(dat, sugpct ~ harvest + block)
interaction_plot(dat, sugpct ~ sow + block)
interaction_plot(dat, yield ~ harvest + sow)
interaction_plot(dat, yield ~ sow + harvest)
interaction_plot(dat, yield ~ harvest + block)
interaction_plot(dat, yield ~ block + harvest)
interaction_plot(dat, yield ~ sow + block)



library(doBy)
library(ggplot2)
library(dplyr)
dat <- milkman

dat <- dat |> filter(ampm==1)
dd <- dat |> group_by(cowlact) |> summarise(n=n()) |> filter(n>300)
dat <- dat  |> filter(cowlact %in% dd$cowlact[1:4])

dat |> ggplot(aes(dfc, ecmy, group=cowlact)) +
    geom_point() + facet_grid(~cowlact)

lm(log(ecmy)~dfc + log(dfc))






library(tidyverse)
load_all()
BC <- read_delim("https://asta.math.aau.dk/datasets?file=BC0.dat",
                 col_types = cols(Class = col_factor()))
mainEffects <- glm(Class ~ ., data = BC, family = binomial)
ms <- model_stability_glm(BC, mainEffects, method = "resample", trace= 0)
ms <- model_stability_glm(BC, mainEffects, method = "subgroups", trace= 0)

model <- intEffects <- glm(Class ~ .^2, data = BC, family = binomial())
ms <- model_stability_glm(BC, intEffects, trace = 0, 
                          n.searches = 5, method = "subgroups")


ms 

library(doBy)
dat <- cbind(y1=c(1,2,3,4,5), z=c(1,2,3,4,5), g=factor(c(1,1,1,2,2))) |> 
as.data.frame()

dat <- cbind(y1=c(NA,2,3,4,5), z=c(1,2,NA,4,5), g=factor(c(1,1,1,2,2))) |> 
  as.data.frame()

dat |> 
  summaryBy(cbind(y1, z) ~ g , data=_, FUN=mean)

dat |> 
  aggregate(cbind(y1, z) ~ g , data=_, FUN=mean)



summary(ms)
ms


sumfun <- function(x, ...){
 c(m=mean(x, na.rm=TRUE, ...), v=var(x, na.rm=TRUE, ...), l=length(x))
}
summaryBy(cbind(Ozone, Temp) ~ Month, data=airquality, FUN=sumfun)
## Compare with
aggregate(cbind(Ozone, Temp) ~ Month, data=airquality, FUN=sumfun)

summaryBy(Ozone ~ Month, data=airquality, FUN=sumfun)
## Compare with
aggregate(Ozone ~ Month, data=airquality, FUN=sumfun)


library(purrr)


summaryBy(Ozone ~ Month, data = airquality, FUN=mean2)
summaryBy(cbind(Ozone, Solar.R) ~ Month, data = airquality, FUN=mean2)

mean2 <- function(x){mean(x, na.rm=TRUE)}

## All good
aggregate(Ozone ~ Month, data = airquality, FUN=mean2)
airquality |> group_by(Month) |> summarise(mean2(Ozone))
split(airquality, airquality$Month) |> 
  sapply(function(x) mean2(x$Ozone)) |> t() |> t()

## All not good error in aggregate ???
## There are missing values in Ozone and Solar.R
aggregate(cbind(Ozone, Solar.R) ~ Month, data = airquality, FUN=mean2)
airquality |> group_by(Month) |> 
  summarise(across(c(Ozone, Solar.R), mean2))
split(airquality, airquality$Month) |> 
  sapply(function(x) sapply(x[,c("Ozone", "Solar.R")], mean2)) |> t()

## No missing values in Wind and Temp
aggregate(cbind(Wind, Temp) ~ Month, data = airquality, FUN=mean2)
airquality |> group_by(Month) |> 
  summarise(across(c(Wind, Temp), mean2))
split(airquality, airquality$Month) |> 
  sapply(function(x) sapply(x[,c("Wind", "Temp")], mean2)) |> t()






library(doBy)
# Your list of right hand sides
rhs_list <- list(
  c("nuclei", "cromatin", "Size.low", "Size.medium", "Shape.low", "nuclei:Size.low", "nuclei:Size.medium", "cromatin:Size.medium", "Size.low:Shape.low"),
  c("nuclei", "cromatin", "Size.low", "Size.medium", "Shape.low", "nuclei:Size.low", "nuclei:Size.medium", "cromatin:Size.medium", "Size.low:Shape.low"),
  c("nuclei", "cromatin", "Size.low", "Size.medium", "Shape.low", "nuclei:Size.low", "nuclei:Size.medium", "cromatin:Size.medium", "Size.low:Shape.low")
)

# Get unique terms across all lists
unique_terms <- unique(unlist(rhs_list))

# Create an empty matrix M with 3 rows and columns equal to the number of unique terms
M <- matrix(0, nrow = length(rhs_list), ncol = length(unique_terms), dimnames = list(NULL, unique_terms))


# Iterate over each list and mark the presence of terms in the matrix
for (i in seq_along(rhs_list)) {
  M[i, rhs_list[[i]]] <- 1
}

# Print the resulting matrix
print(M)






load_all()

ee <- expression(matrix(c(b1 + (b0 - b1)*exp(-k*x) + b2*x, b0, b1 + (b0 - b1)*exp(-k*x) + b2*x, b0, b1 + (b0 - b1)*exp(-k*x) + b2*x, b0), nrow=2))
ff <- expr_to_fun(ee)

ff

nms <- all.vars(ee)


fun_str <- paste0("function(", paste0(nms, collapse=", "), ")")

ee_str <- expr_to_string(ee)


ee_str <- lapply(ee, deparse)


    ee_str <-
        lapply(ee_str,
               function(e_) {
                   paste0(e_, collapse="\n")               
               })
    ee_str



    bd <- paste0("\n{ \n", paste0(ee_str, collapse=";\n "), "\n}")
    ff <- paste0(fun_str, bd)
    fun <- eval(parse(text=ff))

fun


return(fun)









f <- function(x,y,z=2){x+y+z}

nms <- c("x", "y")
vls1 <- c(2, 4)

sf <- section_fun(f, nms, vls1, method="env")
sf

vls2 <- vls1
names(vls2) <- nms
sf <- section_fun(f, vls2)
sf

vls3 <- as.list(vls2)
sf <- section_fun(f, vls3)
sf

sf <- section_fun(f, nms, as.list(vls1))
sf

vls2 <- as.list(vls)
names(vls2) <- nms

section_fun(f, vls2)

vls3 <- list(c(1,3), c(2,4), c(3,5))

ff <- lapply(vls3, function(v) section_fun(f, nms, vls=v))

lapply(ff, do.call, list())

vls4 <- lapply(vls3, function(v) setNames(v, nms))
lapply(vls4, function(v) section_fun(f, v))

vls5 <- lapply(vls3, function(v) as.list(setNames(v, nms)))

vv <- lapply(vls4, function(v) section_fun(f, v))


cc <- bquote(.(vv[[1]])())


fun_to_call <- function(f){
    if (!inherits(f, "function")) stop("'f' must be function.\n")
    bquote(.(f)())
}

cl <- fun_to_call(f)

f2 <- vv[[1]]



## GOES TO DOBY
create_fun_list <- function(fun, list_of_arg_lists, method="def"){
    if (!inherits(list_of_arg_lists, "list"))
        stop("list_of_arg_lists must be a list\n")
    z <- sapply(arg_list, inherits, "list")
    if (!all(z))
        stop("not all elements in list_of_arg_lists are lists\n")
    
    ff <- lapply(list_of_arg_lists, function(a){
        doBy::section_fun(fit_ggm, list_of_arg_lists=a, method="def")
    })
    out <- bquote_fun_list(ff)
    out
}






out <- bquote_fun_list(ff)
lapply(out, eval)


is_list_of_lists <- function(x){
    inherits(x, "list") &&
        all(sapply(x, inherits, "list"))
}


vls4 <- lapply(vls3, function(v){names(v)<-nms;v})
ff <- lapply(vls4, function(v) section_fun(f, nms=v))
lapply(ff, eval)





























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
