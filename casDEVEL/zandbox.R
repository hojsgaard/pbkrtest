setwd("casDEVEL/caracas/")

load_all(".")

def_sym(xx, yy, zz)

uu <- xx + yy
subs(uu, cbind(xx, yy), c(123, xx^2 + yy))

#x1 <- list("xx"=123, "yy"=xx^2+yy)
#subs_lst(uu, x1)

x2 <- list("xx"=123, "yy"=(xx^2+yy)$pyobj)
subs_lst(uu, x2)

x2 <- subs_lst(uu, c(xx, yy), c(123, xx^2 + yy))

x <- c(xx, yy)
v <- c(123, xx^2 + yy)

l <- as.list(v)
names(l) <- all_vars(x)

subs_lst(uu, c())



      p <- vector_sym(3, "p")
      y <- vector_sym(3, "y")
      subs_lst(p, list("p1" = 1, "p2" = y[1], "p3" = 0))

def_sym(aa, bb)
cc <- aa + bb

# Short
subs_lst(cc, c(aa, bb), c(1122, aa^2+bb))

# Same as
l <- as.list(c(1122, aa^2+bb))
names(l) <- all_vars(c(aa, bb))
subs_lst(cc, l)

def_sym(a,b,c,x)
y <- a*x^2 + b*x + c
sol <- solve_sys(y, x)

subs(y, cbind(x), sol[[1]]$x) %>% simplify()

v <- c(123, xx^2 + yy)

nms <- gsub(" *", "", as_character_matrix(c(xx, yy)))

l <- as.list(v)
names(l) <- nms

subs_lst(v, l)

x1 <- as_sym("a")
x2 <- as_sym(c("u", "v"))
x3 <- as_sym("[u, v]")


x3 <- matrix_(c("a", "b", "c", "d"), 2, 2)
x2 <- as_sym(c("x", "y"))

to_vector(x1)
to_vector(x2)
to_vector(x3)
to_vector(x4)

symbol_class(x1)
symbol_class(x2)
symbol_class(x3)
symbol_class(x4)

z <- as.character(x2)

sympy_func(x4, "tolist")


to_list(x1)
to_list(x2)
to_list(x3)
to_list(x4)





x4 <-sympy_func(as_sym("[[u, v]]"), "tolist")
x5 <- sympy_func(x4, "tolist")

as_sym("[[u, v]]")
as_sym("[u, v]")

symbol_is_atomic(x1)
symbol_is_atomic(x2)
symbol_is_atomic(x3)
symbol_is_atomic(x4)

symbol_is_vector(x1)
symbol_is_vector(x2)
symbol_is_vector(x3)
symbol_is_vector(x4)

symbol_is_list(x1)
symbol_is_list(x2)
symbol_is_list(x3)
symbol_is_list(x4)

symbol_is_matrix(x1)
symbol_is_matrix(x2)
symbol_is_matrix(x3)
symbol_is_matrix(x4)






x4 <-sympy_func(as_sym("[[u, v]]"), "tolist")
z <- as.character(x4)

z <- as.character(x2)

grepl("^\\[[^\\[]", z)




z <- as.character(x1)



o_list(x1)
to_list(x2)
to_list(x3)







## Turn matrix into vector

tolist(t(x3)) %>% unbracket() %>% as.character()


#' Replicate elements  
rep_ <- function(x, times=1){
    if (!symbol_is_atomic(x)) stop("'x' must be atomic \n")
    out <- as_sym(rep(as.character(x), times))    
    return(out)
}


x1 %>% as.character()
x2 %>% as.character()
x3 %>% as.character()

matrify(x1)
matrify(x1) %>% as.character()
matrify(x2)
matrify(x2) %>% as.character()
matrify(x3)
matrify(x3) %>% as.character()


# rep_atomic

x = x1
r <- rep(x, 3) 
r
b <- sapply(r, as.character)
b
out <- as_sym(b)
out


x = x2
r <- rep(x, 3) 
r
b <- sapply(r, as.character)
b
out <- as_sym(b)
out

as_sym(sapply(listify(t(out)), as.character))

rep(c("u", "v"), 3)


as_sym(sapply(listify(t(x3)), as.character))

x <- x3
z2 <- remove_mat_prefix(x)
z3 <- unbracket(z2)







z3 %>% as.character
remove_mat_prefix(z)
grep("^Matrix\\(\\[.*]\\)", \\1, z)



x <- matrix_sym(2,2)
x %>% as.character

x2 <- unbracket(x) 
x2  %>% as.character
symbol_is_atomic(x)

x3 <- unbracket(x2)
x3  %>% as.character

z <- as.character(x)




elt_power <- function(x, power){

    ## If x is matrix and power is atomic things are ok now
    ## Below: the case where x is atomic and power is matrix.
    ## Need checks

    if (!symbol_is_atomic(x)) stop("'x' must be atomic\n")
    if (!symbol_is_matrix(power)) stop("'power' must be matrix\n")

    ## Issue with dim of matrix
    
    out <- vector_sym(nrow(power))
    for (i in 1:nrow(power)){
        z <- power[i]
        out <- subs(out, out[i], x^z)
    }
    out
}













document()
load_all()
install()

library(caracas)

# Create a symbol 'b1' corresponding to an entity called 'a' in SymPy:
b1 <- symbol("a"); str(b1)
# A new symbol can be created as:
b2 <- b1 + 1; str(b2)
# The \proglang{Python} entity 'a' in the symbol can be modified with:
b3 <- subs(b2, "a", "k"); str(b3)

m <- as_sym(matrix(4, nr=2, nc=2))

d <- 4
x <- matrix_sym(d,d)
cm <- t(sympy_func(x, "cofactor_matrix")) / det(x)

(cm %*% x) %>% simplify()

mmi <- inv(mm)

mmi[1,1] %>% denominator() 

x <- mmi

invcf <- function(x){
  return(t(sympy_func(x, "cofactor_matrix")) / det(x))
}

x3 <- matrix_sym(3, 3)
(inv(x3) - invcf(x3)) %>% simplify()

x4 <- matrix_sym(4, 4)
(inv(x4) - invcf(x4)) %>% simplify()

microbenchmark::microbenchmark(
  inv(x3),
  invcf(x3),
  inv(x4),
  invcf(x4),
  times=4
)

Unit: milliseconds
expr         min           lq         mean      median           uq          max neval cld
inv(x3)    863.4468    920.87706   1021.36414   1030.0806   1121.85123   1161.84857     4  a 
invcf(x3)     67.1244     72.71165     80.45269     83.0884     88.19373     88.50956     4  a 
inv(x4) 214497.4624 215083.69876 216498.63827 216698.9180 217913.57778 218099.25464     4   b
invcf(x4)    191.4365    273.96105    357.29291    397.9659    440.62476    441.80333     4  a 


inv2fl <- function(x){
  xi <- invcf(x)
  d <- denominator(xi[1,1])
  as_factor_list(1/d, d * xi)
}




m <- matrix_sym(d, d)
mi <- inv(m)
det_m <- det(m)
fl <- as_factor_list(1/det_m, det_m * mi)
tex(fl)

m <- matrix(1:4, nrow=2)
mi <- solve(m)
det_m <- det(m)
fl <- as_factor_list(paste0("1/", det_m), det_m * mi)
tex(fl)

as_sym()





z <- list(as_sym(4), m/4)
class(z) <- "foo"


as_factor_list <- function(...){
    lst <- list(...)
    out <- lapply(lst, as_sym)  
    class(out) <- "factor_list"
    out
}


x <- factorit(4, m/4)



tex.factor_list <- function(x){
    a<- lapply(x, tex)  |> unlist()
    paste(a, collapse="  ")
}

factorit(4, m/4) %>% tex()







factor


|> paste0(sep=" ")

diag(W)




caracas:::as_expr_worker(x)






x <- as_sym(c("a", "b"))

e <- as_expr(x)


s  <- as_sym("[[r1,r2,r3], [u1,u2,u3]]")
s2 <- apply(as_character_matrix(s), 2, function(x) (paste("1/(", x, ")")))
as_sym(s2)




  xx1 <- as.character(x)
  xx1 <- remove_mat_prefix(xx1)  
  xx1 ## "[[a, c, e], [b, d, f]]"
  
  
  xx2 <- gsub("^\\[(.*)\\]$", "\\1", xx1)
  xx2
  
  xx3 <- strsplit(xx2, "\\],")[[1]]
  for (i in 1:(length(xx3)-1))
    xx3[i] <- paste(xx3[i], "]")
  xx3 ## "[a, c, e ]" " [b, d, f]"
  
  xx4 <- gsub("[[:space:]]*\\[(.*)\\][[:space:]]*", "\\1", xx3)
  xx4 ##  "a, c, e " "b, d, f" 
  
  vv <- strsplit(xx4, ",")
  vv
  
  ww <- lapply(vv, function(v) paste("(", v, ")", sep="") )
  ww
  
  xx <- lapply(ww, function(w) paste0(num, "/", w))
  xx

  xx2 <- lapply(xx, function(x) paste(x, collapse=", "))
  xx2


  yy <- lapply(xx2, function(x) {paste("[", x, "]", collapse=", ")})
  yy

  zz <- unlist(yy) 
  zz

  out<- paste("Matrix([", paste0(zz, collapse=", "), "])")
out

  as_sym(out)






s <- "[[[r1],[r2],[r3]], [[u1],[u2],[u3]]]"



x <- g
s <- g$pyobj
s <- h$pyobj




identical(substring(s, 1, 3), "[[[")




s2 <- do_unbracket(s)
s2


do_unbracket(s2)

s3 <- do_split_rows(s2)

s4 <- list(unlist(s3[1:2]), unlist(s3[3:4]))

s4 %>% do_comma %>% do_bracket %>% do_comma %>% do_bracket %>% as_sym






rrr <- lapply(s3, function(r) paste("1/(",r,")"))

rr0 <- do_comma(rrr)
rr1 <- do_bracket(rr0)
rr2 <- do_comma(rr1)
rr3 <- do_bracket(rr2)
as_sym(rr3)









## strip outer (extra) []:
s2 <- gsub("\\[(.+?)\\]", "\\1", s) 
s2
# "[[r1,r2,r3], [u1,u2,u3]]"

# "[[r1,r2,r3 ]" " [u1,u2,u3]]"

## drop []



## drop blands
out <- lapply(out, function(o) gsub("[[:space:]]*", "", o))



rrr <- s2
rrr













out










rr0 <- lapply(rrr, paste, collapse=",")



rr2 <- paste0(rr1, collapse=", ")

as_sym(do_bracket(rr2))







  out<- paste0("Matrix(", paste0(rr2, collapse=", "), ")")
out

as_sym(out)


eval_to_symbol(rr2)

  







  xx1 <- as.character(x)
  xx1 <- remove_mat_prefix(xx1)  
  xx1 ## "[[a, c, e], [b, d, f]]"

  identical(substring(xx1, 1, 3), "[[[")



gsub("\\[(.+?)\\]", "\\1", xx1)





  gsub(xx1, "\\[{1}(.*)", "\\1")


  xx2 <- gsub("^\\[(.*)\\]$", "\\1", xx1)
  xx2
 
  xx3 <- strsplit(xx2, "\\]\\],")[[1]]
  for (i in 1:(length(xx3)-1))
    xx3[i] <- paste(xx3[i], "]]")
  xx3 ## "[a, c, e ]" " [b, d, f]"





  xx4 <- gsub("[[:space:]]*\\[(.*)\\][[:space:]]*", "\\1", xx3)
  xx4 ##  "a, c, e " "b, d, f" 





  vv <- strsplit(xx4, ",")
  vv
  
  ww <- lapply(vv, function(v) paste("(", v, ")", sep="") )
  ww
  
  xx <- lapply(ww, function(w) paste0(num, "/", w))
  xx

  xx2 <- lapply(xx, function(x) paste(x, collapse=", "))
  xx2


  yy <- lapply(xx2, function(x) {paste("[", x, "]", collapse=", ")})
  yy

  zz <- unlist(yy) 
  zz

  out<- paste("Matrix([", paste0(zz, collapse=", "), "])")
out

  as_sym(out)















x / (x+y)

load_all("caracas")

r <- reciprocal_matrix(den)

B <- as_sym("[[x, a], [a, x**2]]")
x <- B

num <- 1
x <- den











p <- as_sym(paste0("p", 1:3))
r <- 2 * p
r <- 0 / p


A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
B <- as_sym(A)


reciprocal_matrix(B)







[x^(-2)]] ])")

a<-expect_equal(Bchar, "Matrix([[[1/x], [1.00000000000000]], [[0.500000000000000], [x^(-2)]]])")






r <- as_sym(paste0("r", 1:3))





num <- 1






  
  expect_equal(as.character(det(B)), "x^3 - 2")




out <- paste("Matrix([",paste0("[", paste0("1/",uu), "]", collapse=", "),"])")
as_sym(out)







reciprocal_matrix(B)




as_sym("[[2], [2], [2]]") / as_sym("[[p1], [p2], [p3]]")





load_all("caracas"); p * r

load_all("caracas"); p / r


as.character(r)


symbol_is_matrix(r)


xstr <- "[[r1], [r2], [r3]]"

z<-as_sym(xstr)





reciprocal_matrix(r)





uu


zz <- strsplit(gsub("\\(\\[(.*)\\]\\)$", "\\1", xstr), ",")







grepl("^Matrix\\(\\[\\[.*\\]\\]\\)$", xstr)



inv(as_sym("Matrix([[r1], [r2]])"))


