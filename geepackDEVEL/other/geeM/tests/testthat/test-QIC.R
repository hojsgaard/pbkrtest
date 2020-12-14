
if(require(geepack) & require(MuMIn)){
  #`getQIC.geem` <-
  #  function(x, typeR = FALSE) {
  #    
  #    fam <- tryCatch(family(x)$family, error = function(e){return("quasi")})
  #    
  #    xi <- if(x$corr != "independence")
  ##      update(x, corstr = "independence") else x
  #    MuMIn:::.qic2(x$y,  fitted(x), as.matrix(x$var),
  #                  fitted(xi), as.matrix(xi$naiv.var), fam,
  #                  typeR = typeR)
  #  }
  
  
  
  data(ohio, package="geepack")
  
  LinkFun <- function(arg){qcauchy(arg)}
  InvLink <- function(arg){pcauchy(arg)}
  InvLinkDeriv <- function(arg){dcauchy(arg)}
  VarFun <- function(arg){arg*(1-arg)}
  FunList <- list(LinkFun, VarFun, InvLink, InvLinkDeriv)
  
  fit <- geem(resp ~ age + smoke + age:smoke, id=id, data=ohio, family=FunList, corstr="exch", scale.fix=TRUE)
  expect_error(QIC(fit))
  
  fit2 <- geem(resp ~ age + smoke + age:smoke, id=id, data=ohio, family=binomial, corstr="exch", scale.fix=TRUE)
  
  expect_true(is.numeric(QIC(fit2)))
  
  
  
  fit3 <- geem(resp ~ age + smoke + age:smoke, id=id, data=ohio, family=quasibinomial, corstr="exch", scale.fix=TRUE)
  expect_error(QIC(fit3))
}