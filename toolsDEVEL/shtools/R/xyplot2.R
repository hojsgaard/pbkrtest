
xyplot2 <- function(x,data,...){
  cl <- match.call()
  cl[[1]]<- as.name("xyplot")
  print(eval(cl))
}
