odds <- function(p){
    p / (1-p)
}
logit <- function(p){
    log(odds(p))
}
expit <- function(x) { ## inverse of logit
  exp(x) / (1 + exp(x))
}



library(tidyverse)
dat <- lme4::cbpp |> as_tibble()
head(dat, 10)
glm1 <- glm(cbind(incidence, size - incidence) ~ period, family = binomial, data = dat)
source("deltaMethod.R")
deltaMethod(glm1, "exp(b2)", parameterNames= paste("b", 0:3, sep=""))
deltaMethod(glm1, "expit(b2)", parameterNames= paste("b", 0:3, sep=""))

exp(b2)






glm2 <- update(glm1, .~. - period)
summary(glm1)
summary(glm2)
anova(glm1, glm2, test="Chisq")

getVcov <- function(v, mod, ...){
  if(missing(v)) return(vcov(mod, ...)) 
  if(inherits(v, "matrix")) return(v)
  if(is.function(v)) return(v(mod, ...)) 
  if(is.null(v)) return(vcov(mod, ...))
  v <- try(as.matrix(v), silent=TRUE)
  if (is.matrix(v)) return(v)
  stop("vcov. must be a matrix or a function")
}

exists.method <- function(generic, object, default=TRUE, strict=FALSE){
	classes <- class(object)
	if (default) classes <- c(classes, "default")
	if (strict) classes <- classes[1]
	any(paste(generic, ".", classes, sep="") %in%
					as.character(methods(generic)))
}


source("deltaMethod.R")
deltaMethod(glm1, "exp(b2)", parameterNames= paste("b", 0:3, sep=""))
deltaMethod(glm1, "expit(b2)", parameterNames= paste("b", 0:3, sep=""))


D(ll$g., ll$para.names[3])

eval(Deriv::Deriv(ll$g., ll$para.names[3]), envir=ll$env)


def_sym(x)
der(expit(x), x) |> simplify()




     m1 <- lm(time ~ t1 + t2, data = Transact) 
     deltaMethod(m1, "b1/b2", parameterNames= paste("b", 0:2, sep="")) 
     deltaMethod(m1, "t1/t2", rhs=1) # use names of preds. rather than coefs.
     deltaMethod(m1, "t1/t2", vcov=hccm) # use hccm function to est. vars.
     deltaMethod(m1, "1/(Intercept)")
     # The next example invokes the default method by extracting the
     # vector of estimates and covariance matrix explicitly
     deltaMethod(coef(m1), "t1/t2", vcov.=vcov(m1))

library(caracas)

