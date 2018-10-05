KRmodcomp_init <- function(m1, m2, matrixOK=FALSE){
    UseMethod("KRmodcomp_init")
}

KRmodcomp_init.lmerMod <-
    KRmodcomp_init.mer <-
    function(m1, m2, matrixOK=FALSE) {
        ##comparison of the mean structures of the models
        ## it is  tested for that (1) m1 is mer and (2) m2 is either mer or a matrix
        mers<- if ( .is.lmm(m1) &
                    (.is.lmm(m2) | is.matrix(m2) ) )
                   TRUE
               else
                   FALSE
        
        if (!mers) {
            cat("Error in modcomp_init\n")
            cat(paste("either model ",substitute(m1), 
                      "\n is not a linear mixed of class mer(CRAN) or lmerMod (GitHub)\n \n",sep=' '))
            cat(paste("or model ", substitute(m2),"\n is neither of that class nor a matrix",sep=''))
            stop()
        }
    
        ##checking matrixcOK is FALSE but m2 is a matrix
        if (!matrixOK & is.matrix(m2)) {
            cat ('Error in modcomp_init \n')
            cat (paste('matrixOK =FALSE but the second model: ', substitute(m2),
                       '\n is  specified via a restriction matrix \n \n',sep=''))
            stop()
        }
        
        Xlarge <- getME(m1, "X")
        rlarge <- rankMatrix(Xlarge)
        ##code <- if ('mer' %in% class(m2)) {
        ##code <- if ('lmerMod' %in% class(m2)) {
        code <- if (.is.lmm(m2)){
            Xsmall <- getME(m2,"X")
            rsmall <- rankMatrix(Xsmall)
            rboth  <- rankMatrix(cbind(Xlarge,Xsmall))
            if (rboth == pmax(rlarge,rsmall)) {
                if (rsmall< rlarge) {
                    1
                } else {
                    if (rsmall > rlarge) {
                        0
                    } else {
                        -1
                    }
                }
            } else {
                -1
            }
        } else {
            ##now model m2  is a restriction matrix
            if (rankMatrix(rbind(Xlarge,m2)) > rlarge) {
                -1
            } else {
                1
            }
        }
        code
    }

KRmodcomp_init.lme <-
    function(m1, m2, matrixOK=FALSE) {
        ##comparison of the mean structures of the models
        ## it is  tested for that (1) m1 is mer and (2) m2 is either mer or a matrix
        mers<- if ( .is.lmm(m1) &
                    (.is.lmm(m2) | is.matrix(m2) ) )
                   TRUE
               else
                   FALSE
        
        if (!mers) {
            cat("Error in modcomp_init\n")
            cat(paste("either model ",substitute(m1), 
                      "\n is not a linear mixed of class mer(CRAN) or lmerMod (GitHub)\n \n",sep=' '))
            cat(paste("or model ", substitute(m2),"\n is neither of that class nor a matrix",sep=''))
            stop()
        }
    
        ##checking matrixcOK is FALSE but m2 is a matrix
        if (!matrixOK & is.matrix(m2)) {
            cat ('Error in modcomp_init \n')
            cat (paste('matrixOK =FALSE but the second model: ', substitute(m2),
                       '\n is  specified via a restriction matrix \n \n',sep=''))
            stop()
        }
        
        Xlarge <- getME.lme(m1, "X")
        rlarge <- rankMatrix(Xlarge)
        ##code <- if ('mer' %in% class(m2)) {
        ##code <- if ('lmerMod' %in% class(m2)) {
        code <- if (.is.lmm(m2)){
            Xsmall <- getME(m2,"X")
            rsmall <- rankMatrix(Xsmall)
            rboth  <- rankMatrix(cbind(Xlarge,Xsmall))
            if (rboth == pmax(rlarge,rsmall)) {
                if (rsmall< rlarge) {
                    1
                } else {
                    if (rsmall > rlarge) {
                        0
                    } else {
                        -1
                    }
                }
            } else {
                -1
            }
        } else {
            ##now model m2  is a restriction matrix
            if (rankMatrix(rbind(Xlarge,m2)) > rlarge) {
                -1
            } else {
                1
            }
        }
        code
    }

getME.lme <- function(object, name){
  if(length(object$group)>1){
    stop('Only one group level is currently supported.')
  }
  if(name=='X'){
    return(model.matrix(formula(object), data=object$data))
  }
  if(name=='is_REML'){
    return(object$method=='REML')
  }
  if(name=='Gp'){
    return(c(0,length(unlist(ranef(object)))))
  }
  if(name=='flist'){
    return(as.list(object$groups))
  }
  if(name=='n_rtrms'){
    return(length(object$group))
  }
  if(name=='cnms'){
    return(list(grp1 = colnames(ranef(object))))
  }
  if(name=='Zt'){
    # Zt is (n_reff x n_subjects) rows by N cols, e.g. 1940 x 4790
    z <- model.matrix(formula(object$modelStruct$reStr)[[1]],data=object$data) # 4790 x 2
    n_reff <- ncol(z) # 2
    N <- nrow(z) # 4790
    groups <- getME.lme(object, "flist")[[1]] # 4790 (970 unique)
    groups.idx <- as.numeric(groups)
    groups.unique <- levels(groups) # 970
    groups.idx.ext <- rep(unique(groups.idx), each=n_reff)
    ii <- jj <- xx <- NULL    
    for(j in 1:N ){ # loop through N columns of Zt
        x <- z[j, ] 
        xx <- c(xx, x)
        jj <- c(jj, rep(j, n_reff))
        ii <- c(ii, which(groups.idx.ext==groups.idx[j])) 
    }
    return(sparseMatrix(i=ii, j=jj, dimnames=list(rep(groups.unique, each=n_reff), groups), x=xx))
  }
}

KRmodcomp_init.gls <-
    function(m1, m2, matrixOK=FALSE) {
        ##comparison of the mean structures of the models
        ## it is  tested for that (1) m1 is mer and (2) m2 is either mer or a matrix
        mers<- if ( .is.lmm(m1) &
                    (.is.lmm(m2) | is.matrix(m2) ) )
                   TRUE
               else
                   FALSE
        
        if (!mers) {
            cat("Error in modcomp_init\n")
            cat(paste("either model ",substitute(m1), 
                      "\n is not a linear mixed of class mer(CRAN) or lmerMod (GitHub)\n \n",sep=' '))
            cat(paste("or model ", substitute(m2),"\n is neither of that class nor a matrix",sep=''))
            stop()
        }
    
        ##checking matrixcOK is FALSE but m2 is a matrix
        if (!matrixOK & is.matrix(m2)) {
            cat ('Error in modcomp_init \n')
            cat (paste('matrixOK =FALSE but the second model: ', substitute(m2),
                       '\n is  specified via a restriction matrix \n \n',sep=''))
            stop()
        }
        
        Xlarge <- getME.gls(m1, "X")
        rlarge <- rankMatrix(Xlarge)
        ##code <- if ('mer' %in% class(m2)) {
        ##code <- if ('lmerMod' %in% class(m2)) {
        code <- if (.is.lmm(m2)){
            Xsmall <- getME(m2,"X")
            rsmall <- rankMatrix(Xsmall)
            rboth  <- rankMatrix(cbind(Xlarge,Xsmall))
            if (rboth == pmax(rlarge,rsmall)) {
                if (rsmall< rlarge) {
                    1
                } else {
                    if (rsmall > rlarge) {
                        0
                    } else {
                        -1
                    }
                }
            } else {
                -1
            }
        } else {
            ##now model m2  is a restriction matrix
            if (rankMatrix(rbind(Xlarge,m2)) > rlarge) {
                -1
            } else {
                1
            }
        }
        code
    }

getME.gls <- function(object, name){
  if(name=='X'){
    return(model.matrix(formula(object), data=getData(object)))
  }
  if(name=='is_REML'){
    return(object$method=='REML')
  }
  if(name=='Zt'){
    # Zt is (n_reff x n_subjects) rows by N cols, e.g. 1940 x 4790
    groups <- object$groups
    ugroups <- unique(groups)  
    X        <- model.matrix(formula(object), data=getData(object))
    # Pinheiro & Bates p 202
    Zt <- matrix(0, nrow=length(ugroups), ncol=nrow(X))
    for(i in 1:length(ugroups)){
      Zt[i, groups==ugroups[i]] <- t(as.matrix(rep(1, sum(groups==ugroups[i]))))
    }
    return(Matrix(Zt, sparse=TRUE))
  }
  if(name=='X_star'){
    # Pinheiro & Bates p 202
    groups <- object$groups
    ugroups <- unique(groups)  
    invsqrtLambda <- lapply(ugroups, function(i) solve(.sqrtMat(getVarCov(object, individual = i)/(sigma( object )^2))))
    X        <- model.matrix(formula(object), data=getData(object))
    X_star   <- matrix(0, nrow=nrow(X), ncol=ncol(X))
    for(i in 1:length(ugroups)){
      X_star[groups==ugroups[i], ] <- t(invsqrtLambda[[i]]) %*% X[groups==ugroups[i],]
    }
    return(Matrix(X_star, sparse=TRUE))
  }
  if(name=='Zt_star'){
    # Pinheiro & Bates p 202
    # Zt is (n_reff x n_subjects) rows by N cols, e.g. 1940 x 4790
    groups <- object$groups
    ugroups <- unique(groups)  
    invsqrtLambda <- lapply(ugroups, function(i) solve(.sqrtMat(getVarCov(object, individual = i)/(sigma( object )^2))))
    X        <- model.matrix(formula(object), data=getData(object))
    # Pinheiro & Bates p 202
    Zt_star <- matrix(0, nrow=length(ugroups), ncol=nrow(X))
    for(i in 1:length(ugroups)){
      Zt_star[i, groups==ugroups[i]] <- t(as.matrix(rep(1, sum(groups==ugroups[i])))) %*% invsqrtLambda[[i]]
    }
    return(Matrix(Zt_star, sparse=TRUE))
  }
}