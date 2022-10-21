library(caracas)

load_all("../caracas")

def_sym(b0, b1, x, x0, y)

f <- b0 / (1 + exp(b1*(x-x0)))

f <- (b1*(x-b0)) 
rss <- (y-f)^2

p <- c(b0, b1)

S <- score(rss, p)
solve_sys(t(S), cbind(0,0), vars=list(b0, b1))



x <- symbol("x")
y <- symbol("y")
def_sym(z)

lhs <- cbind(3*x*y - y, x + z)
rhs <- cbind(-5*x, y+4)
sol <- solve_sys(lhs, rhs, list(x, y))
sol

lhs <- cbind(3*x*y - y, x)
rhs <- cbind(-5*x, y+4)
sol <- solve_sys(lhs, rhs, list(x, y))
sol



lhs <- 3 * x + 4 * y 
sol <- solve_sys(lhs, 0, x)
sol








S <- score(rss, c(b0, b1))
H <- hessian(rss, c(b0, b1))

jacobian(f, c(b0, b1)) %>% symbol_class

x <- paste0("x", seq_len(3))
def_sym_vec(x)
y1 <- x1 + x2
y2 <- x1^2 + x3
y <- c(y1, y2)

try(score(y, c(x1, x2)))
Hessian(y, c(x1, x2))


subs(S, c(x, x0), c(10, 12))





#################

m <- as_sym("[[1,2],[1,4]]")

l <- to_list(m)
to_matrix(l)







u<-as_sym("u")
z<-as_sym("z")

x <- 2 + 4 * u^2

d <- der(x, u)

dim(c(d))

d$pyobj

listify(d)

z <- as.character(d$pyobj)


 x <- paste0("x", seq_len(3))
 def_sym_vec(x)
 y1 <- x1 + x2
 y2 <- x1^2 + x3
 y <- c(y1, y2)
 jacobian(y, x)
u <- 2 + 4*x1^2
jacobian(u, x1)
jacobian(y1, x)

sympy_func(d, "islist")

dx <- c(u,z)

matrify(x)

z <- paste0("Matrix(", paste0(x, collapse = ", "), ")")

caracas:::r_strings_to_python(z)


y <- eval_to_symbol(z)


def_sym(x, y, z)
expr <- x*y + x - 3 + 2*x^2 - z*x^2 + x^3

tuplify(expr)

v <- caracas:::vectorfy(expr)

v %>% matrify()

load_all("../../caracas")


library(caracas)
## A singleton
s0 <- as_sym("x1")
s0 |> print.default()
s0 |> unbracket() 

## A vector / list
s1 <- as_sym("[x1]")
s1 |> print.default()
s1 |> unbracket() 

## A matrix
s2 <- as_sym("[[x1]]")
s2 |> print.default()
s2 |> unbracket() 
s2[1,1]

s3 <- as_sym("[[[x1]]]")
s3 |> print.default()
s3 |> unbracket()


|> print.default()


## x1 is a 3-by-2 matrix whose elements are lists:
   x1 <- as_sym("[[[x1/a], 
                  [x2/a], 
                  [x3/a]], 
                 [[b*x1/a], 
                  [b*x2/a], 
                  [b*x3/a]]]")
x1
x1 |> print.default()

## x2 is a 3-by-2 matrix whose elements are 'atoms'
x2 <- x1 |> unbracket()
x2
x2 |> print.default()

## x3 is a 6-by-1 matrix
x3 <- x2 |> unbracket() 
x3
x3 |> print.default()

## No effect, x4 is identical to x3
x4 <- x3 |> unbracket()
x4


z <- as.character(x1)
zz <- gsub("\\[([^]]+)\\]", "\\1", z)
zz
x2 <- eval_to_symbol(zz)
x2

z <- as.character(x2)

x2 <- zz
z <- x2


as_sym("[x1/a, x2/a, x3/a, b*x1/a, b*x2/a, b*x3/a]")

x <- "Matrix([[x1/a, x2/a, x3/a, b*x1/a, b*x2/a, b*x3/a]])"

as_sym(x) %>% symbol_is_matrix()

grepl("^Matrix\\(\\[", x)

drop_matrix <- function(x){
    z <- as.character(x)
    zz <- gsub("^Matrix\\(\\[(.*)\\]\\)$", "\\1", z)
    eval_to_symbol(zz)
}

## A matrix

   x <- as_sym("[[x1/a, 
                  x2/a, 
                  x3/a], 
                 [[b*x1/a], 
                  [b*x2/a], 
                  [b*x3/a]]]")



## A matrix


   y <- as_sym("Matrix([
                        [[x1/a], 
                         [x2/a], 
                         [x3/a]], 
                        [[b*x1/a], 
                         [b*x2/a], 
                         [b*x3/a]]
                       ])")


dim(x)
print.default(x)


unbracket(x)


s2$pyobj



%>% unbracket()


   x <- as_sym("[[[x1/(b2 + x1)], 
                  [x2/(b2 + x2)], 
                  [x3/(b2 + x3)]], 
                 [[-b1*x1/(b2 + x1)^2], 
                  [-b1*x2/(b2 + x2)^2], 
                  [-b1*x3/(b2 + x3)^2]]]")

dim(x)

unbracket(x)

x %>% unbracket()  %>% unbracket()


matrify(expr)



caracas:::eval_to_symbol(z)


d <- der(y,x)

print.default(d)


                                        # Sørens Sandkasse

library(caracas)

x <- symbol("x")

gamma(x+3)

g3 <- gamma(x+3)

expand_func(g3)

g3  %>% sympy_func("expand_func")


sympy_func("gamma", x+3)



X <- matrix_(1, nr=3,nc=2)
X
def_sym(eps)
eps

X[2,2] <- 1+eps
X


#' # `caracas` sandbox
#' ### `date()`

library(caracas)
options(caracas.print.rowvec = FALSE)

options(caracas.print.prettyascii= TRUE) 
options("width"=160)


W <- nxm_matrix(4, 2, "eta")
P <- as_sym(paste0("psi_"),1:4)

S <- nxn_matrix(3, "s", T)



der(log(det(A)), "a11")

der(det(A), "a11")


diag_(A %*% S)


p2 <- sum(diag(A %*% S))

der(p2, "a11")







#' ## Miscellaneous

<def_sym(x)
gamma(x+3)
g3 <- gamma(x+3)
g3  %>% expand_func()
#' Alternative
g3  %>% sympy_func("expand_func")

