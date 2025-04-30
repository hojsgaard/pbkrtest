library(caracas)

e1 <- matrix_sym(2,2, "v")
e2 <- matrix_sym(2,2, "w")

e1 <- matrix_sym(2,1, "v")
e2 <- matrix_sym(2,1, "v")

e1 <- matrix_sym(1,3, "v")
e2 <- matrix_sym(1,3, "w")

e1 <- matrix_sym(2,3, "v")
e2 <- matrix_sym(2,3, "v")

## load_all()
load_all("_caracas")
e1 * e2
e1 == e2

def_sym(r)
r^2 == r*r

(r+1)^2 == (r^2+1 +2*r)

rbind(e1, e2)



a <- (r+1)^2
b <- r^2+1 +2*r

are_equal(a, r^2+1 +2*r)

are_equal(e1, "v")




document("_caracas")




library(caracas)
def_sym(y1, y2, b, v, m, mu, c)









mvn <- function(m, V, X=NULL, sym="y"){
    nms <- vector_sym(nrow(V), sym)
    list(m.=m, V.=V, X.=X, sym=nms)
}


## (y1, y2) | b
R <- diag_(v, 2)
X <- rep_(1,2)
C <- matrix_("c")


## y = Xb + N(R)

yy_b <- mvn(3*b, R, X, "y")
yy_b

## b= m + N(C)

bb <- mvn(mu, C, sym="b")
bb

## (y,b)
yy.bb <- joint(yy_b, bb)
yy.bb ## Ingen model matrix....


## b = Zu + N(V)
## y = m + N(C)

def_sym(u)

bb.u <- mvn(3*u, matrix_("w"), X=matrix_(1), sym="b") ## 1.dim
uu <- mvn(5*m, C, sym="u") # 1 dim

bb.uu <- joint(bb.u, uu)
bb.uu  ## Ingen model matrix

yy.u <- joint(yy.b, bb.u)
yy.u

joint(yy.u, uu)




cond <- yy.u
marg <- uu


## cond <- bb.u
## marg <- uu

cond <- yy.b
marg <- bb.u


joint <- function(cond, marg){
    sy <- c(cond$sym, marg$sym)
    X  <- cond$X

    ## subs(yy.b$m., b, bb$m.)     

    mm <- marg$m
    m   <- rbind(X %*% mm, mm)    
    Vy  <- cond$X %*% marg$V %*% t(cond$X) + cond$V
    Cyb <- cond$X %*% marg$V
    V   <- rbind(cbind(Vy,     Cyb ),
                 cbind(t(Cyb), marg$V))

    X2 <- NULL
    if (!is.null(marg$X.))
        X2 <- X %*% marg$X.
    list(m.=m, V.=V, X.=X2, sym=sy)
}


yy.bb <- joint(yy.b, bb)
yy.bb

marg <- function(yb, idx){
    list(##X=yb$X[idx,, drop=F],
         b=yb$m[idx,  drop=F],
         V=yb$V[idx, idx, drop=F])
}


m <- yy.bb$m
V <- yy.bb$V
ii <- 1


V[ii, ii] - V[ii, -ii, drop=F] %*%  solve(V[-ii, -ii, drop=F]) %*% V[-ii, ii, drop=F] |> simplify()

A <- m[ii] - V[ii, -ii, drop=F] %*%  solve(V[-ii, -ii, drop=F]) %*% m[-ii] |> simplify()

B <- V[ii, -ii, drop=F] %*%  solve(V[-ii, -ii, drop=F]) |> simplify()

A
B










