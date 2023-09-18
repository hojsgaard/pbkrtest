## doBy extensions : funBy

mydata <- data.frame(
y=rnorm(32), x=rnorm(32),
g1=factor(rep(c(1,2), each=16)), 
g2=factor(rep(c(1,2), each=8)),
g3=factor(rep(c(1,2), each=4))) 


sapply(flist,source) 

t.testBy1 <- function(formula, group, data, ...){
  formulaFunBy(formula, group, data, FUN=t.test, class="t.testBy", ...)
}

t.testBy2 <- function(formula, group, data, ...){
  xyFunBy(formula, group, data, FUN=t.test, class="t.testBy", ...)
}


t.testBy1(y~g1, ~g2+g3, data=mydata)
t.testBy2(y~x, ~g2+g3, data=mydata)

