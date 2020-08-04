require(MASS)

esticon <- function(obj, cm, beta0, conf.int = "wald", level=0.95, joint.test=FALSE) UseMethod("esticon")

esticon.lm <- function (obj, cm, beta0, conf.int = NULL,joint.test=FALSE){
  if (joint.test==TRUE){
    .wald(obj, cm, beta0)
  } else { 
    stat.name <- "t.stat"
    cf <- summary.lm(obj)$coefficients
    vcv <- summary.lm(obj)$cov.unscaled * summary.lm(obj)$sigma^2
    df <- obj$df.residual
    esticon.core(obj, cm, beta0, conf.int=conf.int,cf,vcv,df,stat.name)
  }
}


esticon.gee <- function (obj, cm, beta0, conf.int = NULL,joint.test=FALSE){
  if (joint.test==TRUE){
    .wald(obj, cm, beta0)
  } else { 
    stat.name <- "X2.stat"
    cf <- summary(obj)$coef
    vcv <- obj$robust.variance
    df <- 1
    esticon.core(obj, cm, beta0, conf.int=conf.int,cf,vcv,df,stat.name)
  }
}



.wald <- function (obj, cm,beta0)
{
    if (!is.matrix(cm) && !is.data.frame(cm)) 
        cm <- matrix(cm, nrow = 1)

    if (missing(beta0))
      beta0 <- rep(0,nrow(cm))

    df <- nrow(cm)
    if ("geese" %in% class(obj)) {
        cf <- obj$beta
        vcv <- obj$vbeta
    } else if ("geeglm" %in% class(obj)) {
      cf <- obj$coef
      vcv <- summary(obj)$cov.scaled
    }
    else if ("gee" %in% class(obj)) {
        cf <- obj$coef
        vcv <- obj$robust.variance
    }
    else if ("lm" %in% class(obj)) {
        cf <- summary.lm(obj)$coefficients[, 1]
        vcv <- summary.lm(obj)$cov.unscaled * summary.lm(obj)$sigma^2
        if ("glm" %in% class(obj)) {
            vcv <- summary(obj)$cov.scaled
        }
    }
    else stop("obj must be of class 'lm', 'glm', 'aov', 'gee' or 'geese'")
    u <- (cm %*% cf)-beta0
    vcv.u <- cm %*% vcv %*% t(cm)
    W <- t(u) %*% solve(vcv.u) %*% u
    prob <- 1 - pchisq(W, df = df)
    retval <- as.data.frame(cbind(W, df, prob))
    names(retval) <- c("X2.stat", "DF", "Pr(>|X^2|)")
    return(as.data.frame(retval))
}

esticon.geeglm <- function (obj, cm, beta0, conf.int = NULL,joint.test=FALSE){
  #print("esticon.geeglm")
  if (joint.test==TRUE){
    .wald(obj, cm, beta0)
  } else { 
    stat.name <- "X2.stat"
    cf  <- summary(obj)$coef
    vcv <- summary(obj)$cov.scaled
    df <- 1
    #print(cf); print(vcv)
    esticon.core(obj, cm, beta0, conf.int=conf.int,cf,vcv,df,stat.name)
  }
}


esticon.lme <- function (obj, cm, beta0, conf.int = NULL,joint.test=FALSE){
  if (joint.test==TRUE){
    .wald(obj, cm, beta0)
  } else { 
    stat.name <- "t.stat"
    cf <- summary(obj)$tTable
    rho <- summary(obj)$cor
    vcv <- rho * outer(cf[, 2], cf[, 2])
    tmp <- cm
    tmp[tmp == 0] <- NA
    df.all <- t(abs(t(tmp) * obj$fixDF$X))
    df <- apply(df.all, 1, min, na.rm = TRUE)
    #print(df)
    problem <- apply(df.all != df, 1, any, na.rm = TRUE)
    #print(problem)
    if (any(problem)) 
      warning(paste("Degrees of freedom vary among parameters used to ", 
                    "construct linear contrast(s): ",
                    paste((1:nrow(tmp))[problem], 
                          collapse = ","),
                    ". Using the smallest df among the set of parameters.", 
                    sep = ""))
    df <- min(df)
    esticon.core(obj, cm, beta0, conf.int=conf.int,cf,vcv,df,stat.name)
  }    
}


esticon.glm <- function (obj, cm, beta0, conf.int = "wald",level=0.95,joint.test=FALSE){
  if (joint.test==TRUE){
    .wald(obj, cm, beta0)
  } else { 
    cf <- summary.lm(obj)$coefficients
    vcv <- summary(obj)$cov.scaled
    if(family(obj)[1] %in% c("poisson","binomial")){
      stat.name <- "X2.stat"            
      df <- 1
    } else {              
      stat.name <- "t.stat"
      df <- obj$df.residual
    }
    esticon2.core(obj, cm, beta0, conf.int=conf.int,level=level, cf,vcv,df,stat.name)
  }
}

esticon2.core <- function(obj, cm, beta0, conf.int = "wald",level=0.95,cf,vcv,df,stat.name)
{
  #if (is.null(cm)) ????
  #  cm <- diag(dim(cf)[1])
  if (!is.matrix(cm) && !is.data.frame(cm)) 
    cm <- matrix(cm, nrow = 1)

  if (missing(beta0))
    beta0 <- rep(0,nrow(cm))

  if (is.null(rownames(cm))) 
    rn <- paste("(", apply(cm, 1, paste, collapse = " "), ")", sep = "")
  else
    rn <- rownames(cm)

        
    ## Generelt
  ct <- cm %*% cf[, 1] 
  ct.diff <- cm %*% cf[, 1] - beta0      
  vc <- sqrt(diag(cm %*% vcv %*% t(cm)))
  
  
  if (level <= 0 || level >= 1) 
    stop("conf.int should be between 0 and 1. Usual values are 0.95, 0.90")
  alpha <- 1 - level
  
  switch(stat.name,
         "X2.stat"={
           df <- 1
           quant <- qt(1 - alpha/2, 1000)
           prob <- 1 - pchisq((ct.diff/vc)^2, df = 1)
           retval <- cbind(hyp=beta0, est = ct, stderr = vc, t.value = (ct.diff/vc)^2, 
                           df = df, prob = prob )
           dimnames(retval) <-
               list(rn, c("beta0","Estimate","Std.Error","X2.value","DF","Pr(>|X^2|)"))    
         },
         "t.stat"={
           df <- obj$df.residual
           quant <- qt(1 - alpha/2, df  )  
           prob <- 2 * (1 - pt(abs(ct.diff/vc), df))
           retval <- cbind(hyp=beta0, est = ct, stderr = vc, t.value = ct.diff/vc, 
                           df = df, prob = prob)
           dimnames(retval) <-
             list(rn, c("beta0","Estimate","Std.Error","t.value","DF","Pr(>|t|)"))    
         })
  
  if(!(tolower(conf.int) %in% c("wald","lr")))
    stop("conf.int must be either 'wald' or 'lr'",call.=FALSE)
  if(tolower(conf.int)=="lr" & class(obj)[1]!="glm"){
    cat("Note: LR confidence intervals only implemented for 'glm' objects\n")
    cat(" Calculating Wald confidence intervals instead\n")
    conf.int <- "wald"
  }
  
  switch(tolower(conf.int),
         "wald"= {vvv <- cbind(ct.diff-vc*quant, ct.diff+vc*quant)},
         "lr"  = {vvv <- NULL
                  for(i in 1:nrow(cm))
                    vvv <- rbind(vvv, .cilambda(cm[i,],obj)-beta0[i] )
                })
  colnames(vvv) <- c("Lower.CI", "Upper.CI")  
  retval <- cbind(retval, vvv)
  return(retval)
}



esticon.core <- function (obj, cm, beta0, conf.int = NULL, cf,vcv,df,stat.name ) {

  if (!is.matrix(cm) && !is.data.frame(cm)) 
    cm <- matrix(cm, nrow = 1)
   if (missing(beta0))
    beta0 <- rep(0,nrow(cm))   

  if (is.null(cm)) 
    cm <- diag(dim(cf)[1])
  if (!dim(cm)[2] == dim(cf)[1]) 
    stop(paste("\n Dimension of ",
               deparse(substitute(cm)), 
               ": ", paste(dim(cm), collapse = "x"),
               ", not compatible with no of parameters in ", 
               deparse(substitute(obj)), ": ", dim(cf)[1], sep = ""))
  ct <- cm %*% cf[, 1] 
  ct.diff <- cm %*% cf[, 1] - beta0      
  vc <- sqrt(diag(cm %*% vcv %*% t(cm)))
  if (is.null(rownames(cm))) 
    rn <- paste("(", apply(cm, 1, paste, collapse = " "), ")", sep = "")
  else rn <- rownames(cm)
  switch(stat.name,
         t.stat = {
           prob <- 2 * (1 - pt(abs(ct.diff/vc), df))
         },
         X2.stat = {
           prob <- 1 - pchisq((ct.diff/vc)^2, df = 1)
         })
  
  if (stat.name == "X2.stat") {
    retval <- cbind(hyp=beta0, est = ct, stderr = vc, t.value = (ct.diff/vc)^2, 
                    df = df, prob = prob )
    dimnames(retval) <-
      list(rn, c("beta0","Estimate","Std.Error","X2.value","DF","Pr(>|X^2|)"))
  }
  else if (stat.name == "t.stat") {
    retval <- cbind(hyp=beta0, est = ct, stderr = vc, t.value = ct.diff/vc, 
                    df = df, prob = prob)
    dimnames(retval) <-
      list(rn, c("beta0","Estimate","Std.Error","t.value","DF","Pr(>|t|)"))
  }
  
  if (!is.null(conf.int)) {
    if (conf.int <= 0 || conf.int >= 1) 
      stop("conf.int should be betweeon 0 and 1. Usual values are 0.95, 0.90")
    alpha <- 1 - conf.int
    switch(stat.name,
           t.stat  = { quant <- qt(1 - alpha/2, df  )  },
           X2.stat = { quant <- qt(1 - alpha/2, 1000) })
    nm <- c(colnames(retval), "Lower.CI", "Upper.CI")
    retval <- cbind(retval, lower = ct.diff - vc * quant, upper = ct.diff + 
                    vc * quant)
    colnames(retval) <- nm
  }
  return(as.data.frame(retval))
}




esticon.default <- function (obj, cm, beta0, conf.int = NULL,joint.test=FALSE) 
{
    if (!is.matrix(cm) && !is.data.frame(cm)) 
        cm <- matrix(cm, nrow = 1)

    if (missing(beta0))
      beta0 <- rep(0,nrow(cm))
    
    
    if (joint.test==TRUE){
      .wald(obj, cm, beta0)
    } else {
      if ("lme" %in% class(obj)) {
        stat.name <- "t.stat"
        cf <- summary(obj)$tTable
        rho <- summary(obj)$cor
        vcv <- rho * outer(cf[, 2], cf[, 2])
        tmp <- cm
        tmp[tmp == 0] <- NA
        df.all <- t(abs(t(tmp) * obj$fixDF$X))
        df <- apply(df.all, 1, min, na.rm = TRUE)
        problem <- apply(df.all != df, 1, any, na.rm = TRUE)
        if (any(problem)) 
          warning(paste("Degrees of freedom vary among parameters used to ", 
                        "construct linear contrast(s): ",
                        paste((1:nrow(tmp))[problem], 
                              collapse = ","),
                        ". Using the smallest df among the set of parameters.", 
                        sep = ""))
      }
      else if ("lm" %in% class(obj)) {
        stat.name <- "t.stat"
        cf <- summary.lm(obj)$coefficients
        vcv <- summary.lm(obj)$cov.unscaled * summary.lm(obj)$sigma^2
        df <- obj$df.residual
        if ("glm" %in% class(obj)) {
          vcv <- summary(obj)$cov.scaled
          if(family(obj)[1] %in% c("poisson","binomial")){
            stat.name <- "X2.stat"            
            df <- 1
          } else {              
            stat.name <- "t.stat"
            df <- obj$df.residual
          }
        }
      }
      else if ("geese" %in% class(obj)) {
        stat.name <- "X2.stat"
        cf <- summary(obj)$mean
        vcv <- obj$vbeta
        df <- 1
      }
      else if ("geew" %in% class(obj)) {
        stat.name <- "X2.stat"
        cf  <- summary(obj)$coef
        vcv <- summary(obj)$cov.scaled
        df <- 1
      }
      else if ("gee" %in% class(obj)) {
        stat.name <- "X2.stat"
        cf <- summary(obj)$coef
        vcv <- obj$robust.variance
        df <- 1
      }
      else {
        stop("obj must be of class 'lm', 'glm', 'aov', 'lme', 'gee', 'geese' or 'nlme'")
      }


      if (is.null(cm)) 
        cm <- diag(dim(cf)[1])
      if (!dim(cm)[2] == dim(cf)[1]) 
        stop(paste("\n Dimension of ", deparse(substitute(cm)), 
                   ": ", paste(dim(cm), collapse = "x"), ", not compatible with no of parameters in ", 
                   deparse(substitute(obj)), ": ", dim(cf)[1], sep = ""))
      ct <- cm %*% cf[, 1] 
      ct.diff <- cm %*% cf[, 1] - beta0      
      vc <- sqrt(diag(cm %*% vcv %*% t(cm)))
      if (is.null(rownames(cm))) 
        rn <- paste("(", apply(cm, 1, paste, collapse = " "), ")", sep = "")
      else rn <- rownames(cm)
      switch(stat.name,
             t.stat = {
               prob <- 2 * (1 - pt(abs(ct.diff/vc), df))
             },
             X2.stat = {
               prob <- 1 - pchisq((ct.diff/vc)^2, df = 1)
             })
      
      if (stat.name == "X2.stat") {
        retval <- cbind(hyp=beta0, est = ct, stderr = vc, t.value = (ct.diff/vc)^2, 
                        df = df, prob = prob )
        dimnames(retval) <-
          list(rn, c("beta0","Estimate","Std.Error","X2.value","DF","Pr(>|X^2|)"))
      }
      else if (stat.name == "t.stat") {
        retval <- cbind(hyp=beta0, est = ct, stderr = vc, t.value = ct.diff/vc, 
                        df = df, prob = prob)
        dimnames(retval) <-
          list(rn, c("beta0","Estimate","Std.Error","t.value","DF","Pr(>|t|)"))
      }
      
      if (!is.null(conf.int)) {
        if (conf.int <= 0 || conf.int >= 1) 
          stop("conf.int should be betweeon 0 and 1. Usual values are 0.95, 0.90")
        alpha <- 1 - conf.int
        switch(stat.name,
               t.stat  = { quant <- qt(1 - alpha/2, df  )  },
               X2.stat = { quant <- qt(1 - alpha/2, 1000) })
        nm <- c(colnames(retval), "Lower.CI", "Upper.CI")
        retval <- cbind(retval, lower = ct.diff - vc * quant, upper = ct.diff + 
                        vc * quant)
        colnames(retval) <- nm
      }
      return(as.data.frame(retval))
    }
  }




.cilambda <- function(lambda,m,level=0.95,...){

  if (!is.matrix(lambda) && !is.data.frame(lambda)) 
    lambda <- matrix(lambda, nrow = 1)
  
  mnew  <- m
  mm <- model.matrix(m)
  A  <- diag(rep(1,ncol(mm)))
  A <- rbind(lambda,A)
  qA <- qr(t(A))
  v <- qr.Q(qA)
  A <- t(v)
  A[1,]<-lambda

  mmnew <- mm %*% solve(A)
  mfit <- glm.fit(mmnew,m$y,weights=m$prior.weights,family=family(m),
                  mustart=fitted(m),offset=model.offset(m))
  names(mfit$coefficients) <- names(m$coefficients) 
  for(i in 1:length(mfit))
    mnew[[i]] <- mfit[[i]]

  mnew$model.matrix <- mmnew
  value <- .SHconfint.glm(mnew,1,level=level)
  return(value)
}


############### FRA MASS #######################
.SHconfint.glm <- function(object, parm, level = 0.95, trace = FALSE, ...)
{
  pnames <- names(coef(object)); 
  if(missing(parm))
    parm <- seq(along=pnames)
  else
    if(is.character(parm))
      parm <- match(parm, pnames, nomatch = 0)
    object <- .SHprofile.glm(object, which = parm, alpha = (1. - level)/4.,
                      trace = trace)
  value <- confint(object, parm=parm, level=level, trace=trace, ...)
  return(value)
}


.SHprofile.glm <- function(fitted, which = 1:p, alpha = 0.01,
			maxsteps = 10, del = zmax/5, trace = FALSE, ...)
{
  Pnames <- names(B0 <- coefficients(fitted))
  pv0 <- t(as.matrix(B0))
  p <- length(Pnames)
  if(is.character(which)) which <- match(which, Pnames)
  summ <- summary(fitted)
  std.err <- summ$coefficients[, "Std. Error"]
  mf <- update(fitted, method = "model.frame")
  n <- length(Y <- model.response(mf))
  O <- model.offset(mf)
  if(!length(O)) O <- rep(0, n)
  W <- model.weights(mf)
  if(length(W) == 0) W <- rep(1, n)
  OriginalDeviance <- deviance(fitted)
  DispersionParameter <- summ$dispersion
  X <- model.matrix(fitted)
  X <- fitted$model.matrix     ### SHD : IMPORTANT........
                                        #  print(X)
  fam <- family(fitted)
  switch(fam$family,
         binomial = {
           if(!is.null(dim(Y))) {
             n <- n/2
             O <- O[1:n]
             Y <- Y[, 1]/(W <- drop(Y %*% c(1, 1)))
           }
           zmax <- sqrt(qchisq(1 - alpha/2, p))
           profName <- "z"
         },
         poisson = ,
         "Negative Binomial" = {
           zmax <- sqrt(qchisq(1 - alpha/2, p))
           profName <- "z"
         }
         ,
         gaussian = ,
         quasi = ,
         "inverse.gaussian" = ,
         quasibinomial = ,
         quasipoisson = ,
         {
	   zmax <- sqrt(p * qf(1 - alpha/2, p, n - p))
	   profName <- "tau"
         }
         )
  prof <- vector("list", length=length(which))
  names(prof) <- Pnames[which]
  for(i in which) {
    zi <- 0
    pvi <- pv0
    Xi <- X[,  - i, drop = FALSE]
    pi <- Pnames[i]
    for(sgn in c(-1, 1)) {
      if(trace) cat("\nParameter:", pi, c("down", "up")[(sgn + 1)/2 + 1], "\n")
      step <- 0
      z <- 0
      ## LP is the linear predictor including offset.
      LP <- X %*% fitted$coef + O
      LP <- NULL
                                        #print(X)
      muhat <- fitted(fitted)
      ##print("zmax"); print(zmax); print(abs(z))
      while((step <- step + 1) < maxsteps && abs(z) < zmax) {
        bi <- B0[i] + sgn * step * del * std.err[i]
                                        #print(bi)
        o <- O + X[, i] * bi
                                        #print(o)
                                        #print(Xi)
        ## call to glm.fit.null not needed from 1.4.1 on
        fm <- glm.fit(x = Xi, y = Y, weights = W,
                      mustart = muhat,
                      etastart = LP,
                      offset = o, family = fam,
                      control = fitted$control)
        
        LP <- Xi %*% fm$coef + o
        muhat <- NULL
        ri <- pv0
        ri[, names(coef(fm))] <- coef(fm)
        
        ri[, pi] <- bi
        
        pvi <- rbind(pvi, ri)
        zz <- (fm$deviance - OriginalDeviance)/DispersionParameter
        
        if(zz > - 1e-3) zz <- max(zz, 0)
        else stop("profiling has found a better solution, so original fit had not converged")
        z <- sgn * sqrt(zz)
        zi <- c(zi, z)
      }
    }
    si <- order(zi)
    prof[[pi]] <- structure(data.frame(zi[si]), names = profName)
    prof[[pi]]$par.vals <- pvi[si, ]
  }
  val <- structure(prof, original.fit = fitted, summary = summ)
  
  class(val) <- c("profile.glm", "profile")
  val
}




### .SHconfint.profile.glm only for testing...
.SHconfint.profile.glm <-
  function(object, parm = seq(along=pnames), level = 0.95, ...)
{
  #print("confint.profile.glm")
  of <- attr(object, "original.fit")
  #print(of); print("parm"); print(parm)

  pnames <- names(coef(of))
  #print(pnames)
  if(is.character(parm))
    parm <- match(parm, pnames, nomatch = 0)
  a <- (1-level)/2
  a <- c(a, 1-a)
  pct <- paste(round(100*a, 1), "%")
  ci <- array(NA, dim = c(length(parm), 2),
              dimnames = list(pnames[parm], pct))
  cutoff <- qnorm(a)
  for(pm in parm) {
    pro <- object[[ pnames[pm] ]]
    if(length(pnames) > 1)
      sp <- spline(x = pro[, "par.vals"][, pm], y = pro[, 1])
    else{
      #print("HHHHH")
      sp <- spline(x = pro[, "par.vals"], y = pro[, 1])
    }
    ci[pnames[pm], ] <- approx(sp$y, sp$x, xout = cutoff)$y
  }
  drop(ci)
}




##########################################################################################




esticon2ORIG <- function(obj, cm, beta0, conf.int = "wald",level=0.95,joint.test=FALSE)
{
  #if (is.null(cm)) ????
  #  cm <- diag(dim(cf)[1])
  if (!is.matrix(cm) && !is.data.frame(cm)) 
    cm <- matrix(cm, nrow = 1)

  if (missing(beta0))
    beta0 <- rep(0,nrow(cm))


  if (joint.test==TRUE){
    .wald(obj, cm, beta0)
  } else {
    if (is.null(rownames(cm))) 
      rn <- paste("(", apply(cm, 1, paste, collapse = " "), ")", sep = "")
    else
      rn <- rownames(cm)
    
    if ("lme" %in% class(obj)) {
      stat.name <- "t.stat"
      cf <- summary(obj)$tTable
      rho <- summary(obj)$cor
      vcv <- rho * outer(cf[, 2], cf[, 2])
      tmp <- cm
      tmp[tmp == 0] <- NA
      df.all <- t(abs(t(tmp) * obj$fixDF$X))
      df <- apply(df.all, 1, min, na.rm = TRUE)
      problem <- apply(df.all != df, 1, any, na.rm = TRUE)
      if (any(problem)) 
        warning(paste("Degrees of freedom vary among parameters used to ", 
                      "construct linear contrast(s): ",
                      paste((1:nrow(tmp))[problem], 
                            collapse = ","), ". Using the smallest df among the set of parameters.", 
                      sep = ""))
    }
    else if ("lm" %in% class(obj)) {
      stat.name <- "t.stat"
      cf <- summary.lm(obj)$coefficients
      vcv <- summary.lm(obj)$cov.unscaled * summary.lm(obj)$sigma^2
      df <- obj$df.residual
      if ("glm" %in% class(obj)) {
        vcv <- summary(obj)$cov.scaled
        if(family(obj)[1] %in% c("poisson","binomial")){
          stat.name <- "X2.stat"            
          df <- 1
        } 
      }
    }
    else if ("geese" %in% class(obj)) {
      stat.name <- "X2.stat"
      cf <- summary(obj)$mean
      vcv <- obj$vbeta
      df <- 1
    }
    else if ("geew" %in% class(obj)) {
      stat.name <- "X2.stat"
      cf  <- summary(obj)$coef
      vcv <- summary(obj)$cov.scaled
      df <- 1
    }
    else if ("gee" %in% class(obj)) {
      stat.name <- "X2.stat"
      cf <- summary(obj)$coef
      vcv <- obj$robust.variance
      df <- 1
    }
    else {
      stop("obj must be of class 'lm', 'glm', 'aov', 'lme', 'gee', 'geese' or 'nlme'")
    }
        
    ## Generelt
    ct <- cm %*% cf[, 1] 
    ct.diff <- cm %*% cf[, 1] - beta0      
    vc <- sqrt(diag(cm %*% vcv %*% t(cm)))
    
    
    if (level <= 0 || level >= 1) 
      stop("conf.int should be between 0 and 1. Usual values are 0.95, 0.90")
    alpha <- 1 - level
    
    switch(stat.name,
           "X2.stat"={
             df <- 1
             quant <- qt(1 - alpha/2, 1000)
             prob <- 1 - pchisq((ct.diff/vc)^2, df = 1)
             retval <- cbind(hyp=beta0, est = ct, stderr = vc, t.value = (ct.diff/vc)^2, 
                             df = df, prob = prob )
             dimnames(retval) <-
               list(rn, c("beta0","Estimate","Std.Error","X2.value","DF","Pr(>|X^2|)"))    
           },
           "t.stat"={
             df <- obj$df.residual
             quant <- qt(1 - alpha/2, df  )  
             prob <- 2 * (1 - pt(abs(ct.diff/vc), df))
             retval <- cbind(hyp=beta0, est = ct, stderr = vc, t.value = ct.diff/vc, 
                             df = df, prob = prob)
             dimnames(retval) <-
               list(rn, c("beta0","Estimate","Std.Error","t.value","DF","Pr(>|t|)"))    
           })
    
    if(!(tolower(conf.int) %in% c("wald","lr")))
      stop("conf.int must be either 'wald' or 'lr'",call.=FALSE)
    if(tolower(conf.int)=="lr" & class(obj)[1]!="glm"){
      cat("Note: LR confidence intervals only implemented for 'glm' objects\n")
      cat(" Calculating Wald confidence intervals instead\n")
      conf.int <- "wald"
    }
      
    switch(tolower(conf.int),
           "wald"= {vvv <- cbind(ct.diff-vc*quant, ct.diff+vc*quant)},
           "lr"  = {vvv <- NULL
                    for(i in 1:nrow(cm))
                      vvv <- rbind(vvv, .cilambda(cm[i,],obj)-beta0[i] )
                })
    colnames(vvv) <- c("Lower.CI", "Upper.CI")  
    retval <- cbind(retval, vvv)
    return(retval)
  }
}


esticon.defaultORIG <- function (obj, cm, beta0, conf.int = NULL,joint.test=FALSE) 
{
    if (!is.matrix(cm) && !is.data.frame(cm)) 
        cm <- matrix(cm, nrow = 1)

    if (missing(beta0))
      beta0 <- rep(0,nrow(cm))
    
    
    if (joint.test==TRUE){
      .wald(obj, cm, beta0)
    } else {
      if ("lme" %in% class(obj)) {
        stat.name <- "t.stat"
        cf <- summary(obj)$tTable
        rho <- summary(obj)$cor
        vcv <- rho * outer(cf[, 2], cf[, 2])
        tmp <- cm
        tmp[tmp == 0] <- NA
        df.all <- t(abs(t(tmp) * obj$fixDF$X))
        df <- apply(df.all, 1, min, na.rm = TRUE)
        problem <- apply(df.all != df, 1, any, na.rm = TRUE)
        if (any(problem)) 
          warning(paste("Degrees of freedom vary among parameters used to ", 
                        "construct linear contrast(s): ",
                        paste((1:nrow(tmp))[problem], 
                              collapse = ","),
                        ". Using the smallest df among the set of parameters.", 
                        sep = ""))
      }
      else if ("lm" %in% class(obj)) {
        stat.name <- "t.stat"
        cf <- summary.lm(obj)$coefficients
        vcv <- summary.lm(obj)$cov.unscaled * summary.lm(obj)$sigma^2
        df <- obj$df.residual
        if ("glm" %in% class(obj)) {
          vcv <- summary(obj)$cov.scaled
          if(family(obj)[1] %in% c("poisson","binomial")){
            stat.name <- "X2.stat"            
            df <- 1
          } else {              
            stat.name <- "t.stat"
            df <- obj$df.residual
          }
        }
      }
      else if ("geese" %in% class(obj)) {
        stat.name <- "X2.stat"
        cf <- summary(obj)$mean
        vcv <- obj$vbeta
        df <- 1
      }
      else if ("geew" %in% class(obj)) {
        stat.name <- "X2.stat"
        cf  <- summary(obj)$coef
        vcv <- summary(obj)$cov.scaled
        df <- 1
      }
      else if ("gee" %in% class(obj)) {
        stat.name <- "X2.stat"
        cf <- summary(obj)$coef
        vcv <- obj$robust.variance
        df <- 1
      }
      else {
        stop("obj must be of class 'lm', 'glm', 'aov', 'lme', 'gee', 'geese' or 'nlme'")
      }


      if (is.null(cm)) 
        cm <- diag(dim(cf)[1])
      if (!dim(cm)[2] == dim(cf)[1]) 
        stop(paste("\n Dimension of ", deparse(substitute(cm)), 
                   ": ", paste(dim(cm), collapse = "x"), ", not compatible with no of parameters in ", 
                   deparse(substitute(obj)), ": ", dim(cf)[1], sep = ""))
      ct <- cm %*% cf[, 1] 
      ct.diff <- cm %*% cf[, 1] - beta0      
      vc <- sqrt(diag(cm %*% vcv %*% t(cm)))
      if (is.null(rownames(cm))) 
        rn <- paste("(", apply(cm, 1, paste, collapse = " "), ")", sep = "")
      else rn <- rownames(cm)
      switch(stat.name,
             t.stat = {
               prob <- 2 * (1 - pt(abs(ct.diff/vc), df))
             },
             X2.stat = {
               prob <- 1 - pchisq((ct.diff/vc)^2, df = 1)
             })
      
      if (stat.name == "X2.stat") {
        retval <- cbind(hyp=beta0, est = ct, stderr = vc, t.value = (ct.diff/vc)^2, 
                        df = df, prob = prob )
        dimnames(retval) <-
          list(rn, c("beta0","Estimate","Std.Error","X2.value","DF","Pr(>|X^2|)"))
      }
      else if (stat.name == "t.stat") {
        retval <- cbind(hyp=beta0, est = ct, stderr = vc, t.value = ct.diff/vc, 
                        df = df, prob = prob)
        dimnames(retval) <-
          list(rn, c("beta0","Estimate","Std.Error","t.value","DF","Pr(>|t|)"))
      }
      
      if (!is.null(conf.int)) {
        if (conf.int <= 0 || conf.int >= 1) 
          stop("conf.int should be betweeon 0 and 1. Usual values are 0.95, 0.90")
        alpha <- 1 - conf.int
        switch(stat.name,
               t.stat  = { quant <- qt(1 - alpha/2, df  )  },
               X2.stat = { quant <- qt(1 - alpha/2, 1000) })
        nm <- c(colnames(retval), "Lower.CI", "Upper.CI")
        retval <- cbind(retval, lower = ct.diff - vc * quant, upper = ct.diff + 
                        vc * quant)
        colnames(retval) <- nm
      }
      return(as.data.frame(retval))
    }
  }




