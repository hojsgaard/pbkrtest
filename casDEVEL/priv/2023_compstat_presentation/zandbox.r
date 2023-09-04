library(caracas)

def_sym(x, y)
lhs <- cbind(3 * x * y - y, x)
rhs <- cbind(-5 * x, y + 4)

sol <- solve_sys(lhs, rhs, list(x, y))
sol

sol[[1]]  |> as.numeric()

str(sol)


def_sym(x,y)
p <- (x-1)*(x+1)*(y+1)^2

gp <- der(p, c(x, y))  |> cbind() #YES
gp <- der(p, c(x, y)) ## NO
sol <- solve_sys(t(gp), as_sym(rbind(0,0)), list(x,y))

lapply(sol, lapply, as_expr)






set.seed(2023)
N <- 5
x_ <- 1:N
y_ <- 2 + 3*x_ + rnorm(N)
plot(x_, y_)

def_sym(z)
X_ <- cbind(1, x_)
X  <- as_sym(X_)
y  <- as_sym(y_)

Xa <- X
Xa[1,2] <- Xa[1,2] + z
Xa

ba <- inv(t(Xa) %*% Xa) %*% t(Xa) %*% y |> simplify() 
ba |> N(3) |> t()  |> texshow()

ba_fn <- Vectorize(as_func(ba))

## If no error in x-value; z=0
ba_fn(0)


## How do regression coefficients change with error
xx_ <- seq(-0.5,0.5, by=0.1)

yy_ <- ba_fn(xx_)

dat <- data.frame(xx_, t(yy_))
names(dat) <- c("x", "b1", "b2")
dat2 <- gather(dat, key="b", value="estimate", b1, b2)

library(ggplot2)
dat2 |> ggplot(aes(x, estimate, gropu=b)) +
    geom_line() +
    facet_wrap(~b) + geom_vline(xintercept = 0)


db <- der(ba, "z")
db_fn <- Vectorize(as_func(db))
zz_ <- db_fn(xx_)

dat <- data.frame(xx_, t(zz_))
names(dat) <- c("x", "db1", "db2")
dat2 <- gather(dat, key="db", value="estimate", db1, db2)

library(ggplot2)
dat2 |> ggplot(aes(x, estimate, group=db)) +
    geom_line() +
    facet_wrap(~db) + geom_vline(xintercept = 0)







