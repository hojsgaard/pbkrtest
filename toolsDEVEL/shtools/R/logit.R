
logit<-function(x,eps=1e-3){
    sapply(x,function(a){#print(a);
            if (a==0) log((a+eps)/(1-(a+eps))) else log(a /(1-a))})
}


logit2 <- function (x, eps = 0.001) {
  x <- eps + (1-2*eps)*x
  log(x/(1-x))
}
