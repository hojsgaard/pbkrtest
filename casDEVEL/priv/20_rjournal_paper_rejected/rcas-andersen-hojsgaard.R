## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache = FALSE, message = FALSE,
                      fig.height=3, fig.width=5)

library(caracas)
library(magrittr)


## ---- echo=FALSE--------------------------------------------------------------
options(caracas.print.prettyascii = TRUE)
options("digits" = 3)


## -----------------------------------------------------------------------------
def_sym(s1, s2) # Define symbols and assign in global environment
s1
s3 <- s1 + s2; s3 # Also a symbol
## From symbol to expression
e3 <- as_expr(s3); e3
## From expression to symbol
as_sym(e3) # == s3
s3 %>% 
  subs("s1", "r1") %>% 
  subs("s2", "r2")


## -----------------------------------------------------------------------------
def_sym(n)
f <- (1 + 1/n)^n


## -----------------------------------------------------------------------------
lim_f <- limf(f, n, Inf)
lim_f
as_expr(lim_f)


## -----------------------------------------------------------------------------
lim_f_sym <- limf(f, n, Inf, doit = FALSE)
lim_f_sym


## -----------------------------------------------------------------------------
lim_f <- doit(lim_f_sym) # Evaluate the limit as above


## -----------------------------------------------------------------------------
f
lim_f_sym
lim_f


## -----------------------------------------------------------------------------
tex(lim_f_sym)


## ---- eval=FALSE--------------------------------------------------------------
#> \[
#> `r tex(f)`, \quad `r tex(lim_f_sym)`, \quad `r tex(lim_f)`
#> \]


## -----------------------------------------------------------------------------
def_sym(x)
f <- exp(x^2)
subs(f, x, "1/3")
subs(f, x, 1/3)


## -----------------------------------------------------------------------------
subs(f, x, "1/3 + 1/4")
subs(f, x, 1/3 + 1/4)


## -----------------------------------------------------------------------------
subs(f, x, "1/3 + 1/4") %>% as_expr()
subs(f, x, "1/3 + 1/4") %>% N() # Exact decimal representation
as_sym("1/3") %>% N(30)         # ...with a given number of decimals


## -----------------------------------------------------------------------------
f %>% as_expr()
f %>% as_expr() %>% eval(list(x = 1/3))


## -----------------------------------------------------------------------------
A <- matrix(c("a", "b", "c", "d"), 2, 2) %>% as_sym()
A
det(A)
inv(A)
A2 <- A %*% A; A2


## -----------------------------------------------------------------------------
c1 <- as_sym(c("a", "b"))
c2 <- as_sym(c("c", "d"))
cbind(c1, c2)


## -----------------------------------------------------------------------------
evec <- eigenvec(A)
evec1 <- evec[[1]]$eigvec
evec1
eval <- eigenval(A)
simplify(eval[[1]]$eigval)


## -----------------------------------------------------------------------------
A <- matrix(c("a", "0", "0", "1"), 2, 2) %>% as_sym()
qr_res <- QRdecomposition(A)
qr_res$Q
qr_res$R


## -----------------------------------------------------------------------------
ngrp <- 3   # number of groups
spg  <- 2   # number of subjects per group
g <- seq_len(ngrp)
f <- factor(rep(g, each = spg))
y <- as_sym(matrix(paste0("y", seq_along(f))))
X <- as_sym(model.matrix(~ f))


## -----------------------------------------------------------------------------
XtX <- t(X) %*% X
XtXinv <- inv(XtX)


## -----------------------------------------------------------------------------
Xty <- t(X) %*% y
beta_hat <- XtXinv %*% Xty


## -----------------------------------------------------------------------------
N <- 3
y <- as_sym(matrix(paste0("y", 1:N)))
x <- as_sym(matrix(paste0("x", 1:N)))
b <- as_sym(paste0("b", 1:2))
y
x
b


## -----------------------------------------------------------------------------
r <- y - b[1] - exp(x*b[2])
d <- -sum(r*r)/2


## -----------------------------------------------------------------------------
gr <- der(d, b) %>% simplify()
gr <- as(gr, "matrix")


## -----------------------------------------------------------------------------
H <- der2(d, b) %>% simplify()
Hmat <- as(H, "matrix")
dim(Hmat)
#Hmat <- matrify(H) # Convert to matrix
vec <- function(x){
  is_matrix <- function(x){length(dim(x)) == 2} 
  if (!is_matrix(x)) stop("'x' is not a matrix\n")
  x2 <- do.call(rbind, lapply(seq_len(ncol(x)), function(j) x[, j]))
}
Hvec <- vec(Hmat)
#Hvec <- do.call(rbind, lapply(seq_len(ncol(Hmat)), function(j) Hmat[, j]))


## -----------------------------------------------------------------------------
N <- 3
y <- as_sym(matrix(paste0("y", 1:N)))
x <- as_sym(matrix(paste0("x", 1:N)))
b <- as_sym(paste0("b", 1:2))
y
b
x


## -----------------------------------------------------------------------------
num <- b[1] * x
den <- b[2] + x
num
den
mu <- num / den
mu %>% simplify()
##eta <- 1/mu #FIXME
##eta
eta <- den/num
eta %>% simplify()
res <- y - num / den
res
d <- -sum(res * res) / 2
d


## -----------------------------------------------------------------------------
gr <- der(d, b)  %>% simplify()  %>% as("matrix")
gr


## -----------------------------------------------------------------------------
def_sym(x, nu)
h <- (1 + x^2 / nu)^(-(nu + 1) / 2)
lim_h <- limf(h, var = nu, val = Inf)
lim_h


## -----------------------------------------------------------------------------
int_h <- intf(lim_h, x, -Inf, Inf)
int_h


## -----------------------------------------------------------------------------
p <- as_sym(paste0("p", 1:3))
y <- as_sym(paste0("y", 1:3))
def_sym(a) 
l <- sum(y * log(p))
L <- -l + a * (sum(p) - 1)
L


## -----------------------------------------------------------------------------
gL <- der(L, list(p, a))


## -----------------------------------------------------------------------------
sols <- solve_sys(gL, list(p, a)) # takes an RHS argument which defaults to zero
sols


## -----------------------------------------------------------------------------
H <- der2(l, p) # der2(...) is shorthand for calling der() twice
H_sol <- subs_lst(H, sols[[1]]) # substitute solution into H


## \usetikzlibrary{arrows}

## \usetikzlibrary{arrows.meta}

## \begin{tikzpicture}[node distance=2cm,auto,-{Latex[length=3mm, width=3mm]},thick,scale=1]

## \begin{scope}[every node/.style={circle,thick,draw,line width=1pt,scale=1}]

## \node (x1) {$x_1$};

## \node (x2) [right of=x1] {$x_2$};

## \node (x3) [right of=x2] {$x_3$};

## \node (z) [above of=x2] {$z$};

## \end{scope}

## \draw[] (z) to node {} (x1);

## \draw[] (z) to node {} (x2);

## \draw[] (z) to node {} (x3);

## \end{tikzpicture}


## ----ar1, echo=F--------------------------------------------------------------
N <- 3
L <- as_sym(diag(N + 1))
L[cbind(1 + (1:N), 1)] <- "-a"


## ----vue, echo=FALSE----------------------------------------------------------
Vue <- matrix(0, N + 1, N + 1) %>% as_sym()
diag(Vue) <- c("1", rep("v2", N))


## ---- echo=F------------------------------------------------------------------
e <- as_sym(paste0("e", 1:N))
x <- as_sym(paste0("x", 1:N))
def_sym(u, z)
ue <- rbind(u,e)
zx <- rbind(z,x)


## -----------------------------------------------------------------------------
N <- 3
L <- as_sym(diag(N + 1))
L[cbind(1 + (1:N), 1)] <- "-a"
Vue <- matrix(0, N + 1, N + 1) %>% as_sym()
diag(Vue) <- c("1", rep("v2", N))


## -----------------------------------------------------------------------------
V <- inv(L) %*% Vue %*% t(inv(L))
K <- t(L) %*% inv(Vue) %*% L


## -----------------------------------------------------------------------------
Vxx <- V[-1, -1]


## -----------------------------------------------------------------------------
def_sym(a)
a <- symbol("a")
a <- as_sym("a")


## -----------------------------------------------------------------------------
M <- matrix(-1, 1, 1)
M


## -----------------------------------------------------------------------------
as_sym(M)
dim(M)


## ---- error = TRUE------------------------------------------------------------
M <- symbol(M)  


## -----------------------------------------------------------------------------
M
def_sym(M)
M


## -----------------------------------------------------------------------------
x <- symbol("x")
sol <- solve_sys(x^2 + 1, x)
sol
print(sol, simplify = FALSE) # simplify can be set by 'caracas.print.sol.simplify' option
length(sol)
sol[[1]]$x %>% as_expr()


## -----------------------------------------------------------------------------
x <- symbol("x", real = TRUE)
ask(x, 'real')
sol <- solve_sys(x^2 + 1, x)
sol
print(sol, simplify = FALSE)
length(sol)


## -----------------------------------------------------------------------------
x <- symbol("x", positive = TRUE)
ask(x, 'positive')
sol <- solve_sys(x^2 - 1, x)
sol
sol[[1]]$x


## ---- include=FALSE-----------------------------------------------------------
# 
# <!-- ```{r} -->
# <!-- b <- as_sym(paste0("b", 1:3)) -->
# <!-- r  <- y - X %*% b -->
# <!-- d <- -sum(r*r)/2 -->
# <!-- ``` -->
# 
# <!-- \[ -->
# <!--  r = `r tex(r)` -->
# <!-- \] -->
# 
# <!-- ```{r} -->
# <!-- Hess <- der2(d, b) # Call matrify() to convert to a matrix -->
# <!-- ``` -->
# 
# <!-- $$ -->
# <!--   Hess = `r tex(Hess)` -->
# <!-- $$ -->
# 
# <!-- ```{r} -->
# <!-- r <- y - exp(X %*% b) -->
# <!-- d <- -sum(r*r)/2 -->
# <!-- H <- der2(d, b) %>% simplify() -->
# <!-- Hmat <- matrify(H) # Convert to matrix -->
# <!-- Hvec <- do.call(rbind, lapply(seq_len(ncol(Hmat)), function(j) Hmat[, j])) -->
# <!-- ``` -->
# 
# <!-- <!-- FIXME --> -->
# <!-- We use the frowned-upon `vec` operator that stacks the columns of the matrix: -->
# 
# <!-- $$ -->
# <!--  H_{\text{vec}} = `r tex(Hvec)` -->
# <!-- $$ -->

