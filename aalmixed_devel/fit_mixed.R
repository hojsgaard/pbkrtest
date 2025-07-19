
## logL for constrained parameters

##Requires X, y, Vfun as globals
logL0 <- function(parm, X, y, Vfun){
#  cat("parm: ", parm, "\n")
    Vfun. <- Vfun(parm)
    Vi.   <- solve(Vfun.)
    bb    <- solve(t(X) %*% Vi. %*% X, t(X) %*% Vi. %*% y)
    res   <- y - X %*% bb
    Q     <- t(res) %*% Vi. %*% res
    out <- as.numeric(-0.5*(-log(det(Vi.)) + Q))
    -out
}


## Wrappers for constrained parameters
## Wrapper for constrained variance parameters
wrapper_vv <- function(parm){
    parm <- transf_vv(parm)
    print(parm)
    logL0(parm)
}

transf_vv <- function(parm){
    parm[1] <- exp(parm[1])
    parm[2] <- exp(parm[2])
    return(parm)
}

wrapper_cv <- function(parm){
    parm <- transf_cv(parm)
    logL0(parm)
}

transf_cv <- function(parm){
    parm[1] <- tanh(parm[1])
    parm[2] <- exp(parm[2])
    return(parm)
}

wrapper_vc <- function(parm){
    parm <- transf_vc(parm)
    logL0(parm)
}

transf_vc <- function(parm){
    parm[1] <- exp(parm[1])
    parm[2] <- tanh(parm[2])
    return(parm)
}

wrapper_vcv <- function(parm){
    parm <- transf_vcv(parm)
    logL0(parm)
}

transf_vcv <- function(parm){
    parm[1] <- exp(parm[1])
    parm[2] <- tanh(parm[2])
    parm[3] <- exp(parm[3])
    return(parm)
}

