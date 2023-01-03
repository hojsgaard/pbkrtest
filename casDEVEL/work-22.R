library(caracas)


nms_vls_to_list <- function(nms, vls=nms){
    vls <- as.list(vls)
    names(vls) <- nms
    vls
}

m1 <- matrix(c("a", "b", 1, 1), nrow=2)
s1 <- as_sym(m1)
s1

m2 <- "matrix(c(a, b, 1, 1), nrow=2)"

ee <- parse(text=m2)
nms <- nms_vls_to_list(all.vars(ee))

cl <- do.call("substitute", list(ee[[1]], nms))
as_sym(eval(cl))



def_sym(x, b1, b2)
m <- b1 + b2 * x

ee <- as_expr(m)
ss<-deparse(ee[[1]])




as.character(m)

deparse(m)


mf <- as_func(m)
ee <- body(mf)[[2]] |> as.expression()
ee <- parse(text=ee)
ee

nms <- nms_vls_to_list(all.vars(ee))
cl <- do.call("substitute", list(ee[[1]], nms))

as_sym(eval(cl))



a<- structure(c("x1*(-n*exp(b1*x1 + b2*x2) + y*exp(b1*x1 + b2*x2) + y)/(exp(b1*x1 + b2*x2) + 1)", 
"x2*(-n*exp(b1*x1 + b2*x2) + y*exp(b1*x1 + b2*x2) + y)/(exp(b1*x1 + b2*x2) + 1)"
), dim = 2:1)


as.character(a)

as <- as_sym(a) ## OK

nms <- c("x1", "x2", "n", "y")
vls <- c(1, 4, 20, 8)

system.time(for (i in 1:1000){
                as <- as_sym(a)
                subs(as, nms, vls)})

system.time(for (i in 1:1000){
                vv <- stri_replace_all_fixed(a, pattern = nms, replacement = vls, vectorize_all = FALSE)
                dim(vv) <- dim(a)
                as_sym(vv)
            })



args <- nms_vls_to_list(nms, vls)

gsubfn::gsubfn(nms, args, a[1,])

gsub("x1",1, a) |> as_sym()

library(stringi)    





## TASK: Fast version of subs

sym <- matrix_(letters[1:9], nrow=3)
sym <- sym + t(sym)

y <- caracas:::as_expr_worker(sym, as_character=TRUE)
eval(parse(text=y))

y |> parse(text=.) |> eval()


nms <- letters[1:4]
vls <- 1:4

system.time(for (i in 1:1000){
                subs(sym, nms, vls)
            })


sym_str <- as_character_matrix(sym)

Rprof()
system.time(for (i in 1:1000){
                ## vv <- stri_replace_all_fixed(sym_str, pattern = nms, replacement = vls, vectorize_all = FALSE)
                ## dim(vv) <- dim(sym)
                as_sym(vv)
            })
Rprof(NULL)
summaryRprof()


x <- 1:10

vsubs <- Vectorize(subs, vectorize.args = "vls", SIMPLIFY = FALSE)

a <- vsubs(m, "x", x)
do.call(rbind, a)


as_func(m)(1,1,x)

f <-function(a,b,x){
    matrix(c(a, b, x, a+b*x),nrow=2)
}


fr <- function(b1, b2) {
    Reduce(`+`, mapply(f, b1, b2, x, SIMPLIFY = FALSE))
}

fr(2,2)







ff <- Vectorize(as_func(m), vectorize.args = "x", SIMPLIFY = FALSE)



hh <- doBy::section_fun(ff, list(x=1:10))






subs(m, "x", x[1])
