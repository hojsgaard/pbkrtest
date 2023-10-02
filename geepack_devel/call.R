library(geepack)

outer_fun <- function(){
    d <- data.frame(dist=cars$dist, speed=cars$speed, id=rep(1:10, each=5))
    object <- geeglm(dist~speed, id=id, data=d)
    ## object <- lm(dist~speed, data=d)
    ## AIC(object)
    ## QIC(object)
    inner_fun(object)
}

inner_fun <- function(object,...,env=parent.frame()){

    ee <<- env
    
    doit <- function(object){
        cl <- object$call #match.call()
        print(cl)
        eval(cl, env)        
    }
    
    doit(object)
}

outer_fun()

object <- m

object$formula
formula(object)


library(ELCIC)

data(geesimdata)
x<-geesimdata$x
y<-geesimdata$y
id<-geesimdata$id
r<-rep(1,nrow(x))
time<-3
candidate.sets<-list(c(1,2),c(1,2,3))
candidate.cor.sets<-c("exchangeable")
dist="poisson"

## load_all("geepack")

library(geepack)
criterion.qic<-QICc.gee(x=x,y=y,id=id,dist=dist,candidate.sets=candidate.sets, name.var.sets=NULL, candidate.cor.sets=candidate.cor.sets)
criterion.qic
