load_all("doBy")

x  <- splitBy(~Treatment, CO2)


lapply(x, head)


head.splitByData  <- function(x, n=6L, ...){
    lapply(x, head, n=n, ...)
}

head(x,4)
