document("pkg")
load_all("pkg")



f1  <- function(x, y){x + y}
f1_ <- restrict_fun(f1, list(y=10))
f1_

summary(f1_)

get_fun(f1_)
get_restrictions(f1_)
f1_(x=1)

f2 <- function(x, y){
  x <- x + 2 + y
  x
}
f2_ <- restrict_fun(f2, list(x=1)) 
f2_(y=1)

# Notice that this is absurd:
f2__ <- restrict_fun(f2, list(x=10), method="sub")
f2__






x  <- splitBy(~Treatment, CO2)


lapply(x, head)


head.splitByData  <- function(x, n=6L, ...){
    lapply(x, head, n=n, ...)
}

head(x,4)
