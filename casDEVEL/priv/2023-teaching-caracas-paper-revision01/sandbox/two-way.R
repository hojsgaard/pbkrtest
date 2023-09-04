library(caracas)

## Didn't we have a nicer fn for this
factor_out <- function(object, fact){
    as_factor_list(fact, object * fact)
}


nr <- 2
nc <- 2

ym <- matrix_sym(nr, nc, "y")
ym

y <- to_matrix(to_vector(ym)) ## Kludgy; can we do better?
y

dat <- expand.grid(r=factor(1:nr), s=factor(1:nc))

X <- model.matrix(~r+s, data=dat) |> as_sym()

X <- model.matrix(~r+C(s, "contr.sum"), data=dat) |> as_sym()

## Alternative models
## X <- model.matrix(~r, data=dat) |> as_sym()
## X <- model.matrix(~r*s, data=dat) |> as_sym()

XtX <- t(X) %*% X
XtXinv <- inv(XtX)
Xty <- t(X) %*% y
b_hat <- XtXinv %*% Xty
y_hat <- X %*% b_hat

factor_out(b_hat, 4)
factor_out(y_hat, 4)


## SMALL TRICK

subs_symbol <- function(sym, nms, vls, op="-"){
    op2 <- switch(op,
                  "-"="+",
                  "+"="-",
                  "*"="/",
                  "/"="*")
    sym_chr  <- as_character(sym)
    vls_chr  <- as_character(vls)
    sym2_chr <- paste0(sym_chr, op, "(", vls_chr , ")")
    sym2 <- as_sym(sym2_chr)
    
    sym3_str <- paste0(as_character(sym2), op2, nms)
    as_sym(sym3_str)    
}


s   <- sum(vector_sym(6, "x"))/3
sym <- rbind(s, s)
nms <- "x_s"
vls <- sum(as_sym(c("x5", "x1", "x3"))) / 3


##



def_sym(v)
n <- 3
vave <- function(n){
    V <- v * as_sym(toeplitz(paste0("r^", 0:(n-1))))
    one <- as_sym(rep(1, n))
    v_ave <- t(one) %*% V %*% one# / n^2
    v_ave
}

vave(3)
vave(4)
vv <- vave(5)
vv <- as_sym(c(as_character(vv)))
collect(vv, r)
