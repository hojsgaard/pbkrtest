
### EXPECT ALL WILL GIVE THE SAME THING

test_that("Three forms of poisson family are equivalent",{
    skip_on_cran()
    generatedata <- function(beta,alpha,gamma,X,T,n)  {
    mean.vec <- exp(crossprod(t(X),beta))
    y <- matrix(0,nrow=n,ncol=T)
    y[,1] <- rnbinom(n,mu = mean.vec[1],size=mean.vec[1]/gamma)
    for (i in 1:n)  {
      for (t in 2:T)  {
        innovation.mean <- mean.vec[t] - alpha*(sqrt(mean.vec[t]*mean.vec[t-1]))
        I <- rnbinom(1,mu= innovation.mean,size= innovation.mean/gamma)           
        first.shape <- alpha*sqrt(mean.vec[t]*mean.vec[t-1])/gamma
        second.shape <- mean.vec[t-1]/gamma - first.shape
        u <- rbeta(1,shape1 = first.shape,shape2=second.shape)
        a <- rbinom(1,size=y[i,t-1],prob=u)
        y[i,t] = a + I
      }
    }
    longform <- c(t(y))
    simdata <- data.frame(count = longform, time = rep(X[,2],times=n),subject=rep(c(1:n),each=T))
    return(simdata)
  }
  
  X <- cbind(rep(1,5),c(-.5,-.25,0,.25,.5))
  testdat <- generatedata(beta=c(1,.5),alpha=.2,gamma=.5,X=X,T=5,n=3000)
  far1 <- geem(count~ time, id=subject ,data = testdat, family=poisson(), corstr="ar1")
  f1 <- family(far1)
  
  far1 <- geem(count~ time, id=subject ,data = testdat, family=poisson, corstr="ar1")
  f2 <- family(far1)
  
  far1 <- geem(count~ time, id=subject ,data = testdat, family="poisson", corstr="ar1")
  f3 <- family(far1)

  expect_equivalent(f1,f2)
  expect_equivalent(f1,f3)
})






test_that("Default family comes up as gaussian",{
  dat <- data.frame(y = rnorm(1000), x=rnorm(1000), id = rep(1:5, each=200))
  far1 <- geem(y~ x, id= id,data = dat, corstr="ar1")
  f4 <- family(far1)
  
  
  far1 <- geem(y~ x, id=id ,family="gaussian",data = dat, corstr="ar1")
  f5 <- family(far1)
  
  far1 <- geem(y~ x, id=id ,family=gaussian,data = dat, corstr="ar1")
  f6 <- family(far1)
  
  far1 <- geem(y~ x, id=id ,family=gaussian(),data = dat, corstr="ar1")
  f7 <- family(far1)
  expect_equivalent(f4,f5)
  expect_equivalent(f4,f6)
  expect_equivalent(f4,f7)
})

if(require(geepack)){
  data("ohio", package="geepack")
  resplogit <- geem(resp ~ age + smoke + age:smoke, id=id, data = ohio, family = binomial,
                    corstr = "m-dep" , Mv=1)
  f8 <- family(resplogit)
  
  resplogit <- geem(resp ~ age + smoke + age:smoke, id=id, data = ohio, family = "binomial",
                    corstr = "m-dep" , Mv=1)
  f9 <- family(resplogit)
  
  resplogit <- geem(resp ~ age + smoke + age:smoke, id=id, data = ohio, family = binomial(),
                    corstr = "m-dep" , Mv=1)
  f10 <- family(resplogit)
  
  test_that("Binomial always comes up as binomial",{
    expect_equivalent(f8, f9)
    expect_equivalent(f8, f10)
  })
  
  LinkFun <- function(arg){qcauchy(arg)}
  InvLink <- function(arg){pcauchy(arg)}
  InvLinkDeriv <- function(arg){dcauchy(arg)}
  VarFun <- function(arg){arg*(1-arg)}
  FunList <- list(LinkFun, VarFun, InvLink, InvLinkDeriv)
  
  respcauchit <- geem(resp ~ age + smoke + age:smoke, id=id, data = ohio, family = FunList, corstr = "m-dep" , Mv=1)
  
  test_that("user-defined link comes up as custom",{
    expect_equivalent(family(respcauchit)$family, "custom")
  })
}
