anova.geeglm <- function(object, ...){
  anovaPgee (object, ...)
}


anovaPgee <- function(object, ...){
  #cat("anova.gee\n")
  m1 <- object
  objects <- list(object,...)
  if (length(objects)>1)
    m2 <- objects[[2]]
  else 
    m2 <- NULL
  
  if (is.null(m2)){
    term <- attr(object$terms,"term.labels")
    #print(term)
    resp <- paste(formula(object))[2]
    rhs  <- lapply(1:length(term), function(i) paste(term[1:i],collapse=" + "))
    model.list <- c(paste(resp,"~ 1"), paste(resp,"~", rhs))
    #print(model.list)
    
    value <- NULL
    for (i in 2:length(model.list)){
      if (i==2){
        mf1 <- model.list[i-1]
        mf2 <- model.list[i]
        ##print(mf1); print(mf2)

        m1 <- update(object,formula=as.formula(mf1))
        m2 <- update(object,formula=as.formula(mf2))
      } else {
        m1 <- m2
        
        m2 <- update(object,formula=as.formula(model.list[i]))
        ##print(formula(m1)[1:3]); print(formula(m2)[1:3])
      }
      value <- rbind(value,anovageePrim(m1,m2))
    }
    rownames(value) <- term
    attr(value,"model1") <- NULL
    attr(value,"model2") <- NULL
  } else {
    value <- anovageePrim(object,m2)
  }
  value[,3] <- round(value[,3],5)
  return(value)
}


anovageePrim <- function(m1, m2,...){
  mm1 <- model.matrix(m1)
  mm2 <- model.matrix(m2)
  P1 <- mm1 %*% solve(t(mm1)%*%mm1) %*% t(mm1) 
  P2 <- mm2 %*% solve(t(mm2)%*%mm2) %*% t(mm2)
  e2 <- mm2 - P1 %*% mm2
  e1 <- mm1 - P2 %*% mm1

  #print(mm1[c(1:5,100:105),]); print(mm2[c(1:5,100:105),])
  m2inm1 <- all(apply(e2,2,var) < 1e-10)
  m1inm2 <- all(apply(e1,2,var) < 1e-10)

  #print(apply(e2,2,var))
  #print(apply(e1,2,var))
  #print(m2inm1)
  #print(m1inm2)
  if (!any(c(m2inm1,m1inm2)))
    cat("Models not nested\n")
  else 
    if (all(c(m2inm1,m1inm2)))
      cat("Models are identical\n")
    else {
      if (m1inm2){
        tmp <- m1
        m1 <- m2
        m2 <- tmp
      }
      
      mm1 <- model.matrix(m1)
      mm2 <- model.matrix(m2)
      
      mf1 <- paste(paste(formula(m1))[c(2,1,3)],collapse=" ")
      mf2 <- paste(paste(formula(m2))[c(2,1,3)],collapse=" ")

      mm <- cbind(mm2,mm1)
      qmm <- qr(mm)
      qmmq <- qr.Q(qmm)
      nymm1 <- as.data.frame(qmmq[,1:qmm$rank])
      colnames(nymm1) <- paste("parm",1:ncol(nymm1),sep=".")
      nymm2 <- nymm1[,1:ncol(mm2),drop=FALSE]

      dimDiff <- ncol(nymm1)-ncol(nymm2)

      D <- diag(dimDiff)
      L <- cbind(matrix(0,ncol=ncol(nymm2),nrow=nrow(D)),D)

      formula1 <- formula(paste(formula(m1)[[2]],formula(m1)[[1]],
                                paste(c("-1",colnames(nymm1)),collapse="+"),collapse=""))
      

      
      m1call <- m1$call
      #print(formula(m1call)[[2]])
      #print(nymm1[1:10,])
      #print(paste(m1call$formula[[2]]))
      #nymm1[,paste(m1call$formula[[2]])] <- m1$y
      nymm1[,paste(formula(m1call)[[2]])] <- m1$y
      nymm1[,paste(m1call$id)] <- m1$id
      m1call$offset <- m1$offset
      m1call$weights <- m1$weights
      m1call$formula <- formula1

      m1call$data <- nymm1
      m1ny <- eval(m1call)
      #print(class(m1ny))

      val <- esticon(m1ny,L,joint.test=TRUE)

      rownames(val)<-""
      class(val) <- c("anova.gee","data.frame")
      attr(val,"model1") <- mf1
      attr(val,"model2") <- mf2
      return(val)
    }
}

print.anova.geeglm <- function(x,...){
  cat("Analysis table for GEE models\n\n")
  if (!is.null(attr(x,"model1"))){
    cat("Model 1: "); cat(attr(x,"model1"), "\n")
    cat("Model 2: "); cat(attr(x,"model2"), "\n\n")
  }
  print.data.frame(x)
  
}
