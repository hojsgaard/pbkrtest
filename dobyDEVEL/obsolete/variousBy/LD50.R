.ratioVar <- function(x, num,den,numval){
  m1 <- x
  beta <- coef(m1)
  numvec <- rep(0,length(beta))
  denvec <- rep(0,length(beta))
  numvec[num] <- numval
  denvec[den] <- 1
  M <- rbind(numvec,denvec)
  vcv <- summary(m1)$cov.scale
  beta2 <- M %*% beta
  vcv2  <- M %*% vcv %*% t(M)
  muvec <- c(1/beta2[2], -beta2[1]/(beta2[2]^2))
  ratiovar <- t(muvec) %*% vcv2 %*% muvec
  return(ratiovar)
}

.ratio <- function(x,num,den,numval,sign=-1){
  m1 <- x
  beta <- coef(m1)
  numvec <- rep(0,length(beta))
  denvec <- rep(0,length(beta))
  numvec[num] <- numval
  denvec[den] <- 1
  M <- rbind(numvec,denvec)
  beta2 <- M %*% beta
  ratio <- sign*beta2[1,1]/beta2[2,1]
  return(ratio)
}

.ld50 <- function(x,num,den,numval){
 est  <- .ratio(x,num,den,numval)
 vare <- .ratioVar(x,num,den,numval)
 result <- c("ld50"=est, lower=est-1.96*sqrt(vare), upper=est+1.96*sqrt(vare))
 return(result)
}

.dose.LD50 <- function(x,lambda){
 if(length(which(is.na(lambda))) !=1){
   stop("lambda must contain exactly one entry which is NA")
 } else {
  den <-which(is.na(lambda))
  num <-which(!is.na(lambda))
  numval <- lambda[num]
  value <- .ld50(x,num,den,numval)
  return(value)
 }
}
