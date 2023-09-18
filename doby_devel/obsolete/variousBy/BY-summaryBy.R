
.summaryBy <- 
  function (formula, data=parent.frame(), id=NULL, FUN=mean, keep.names=FALSE,
            p2d=FALSE, order=TRUE, full.dimension=FALSE, ...)
  {    
    debug.info <- 0
    data.var  <- names(data)
    
    lhs      <- formula[[2]]
    lh.var   <- .lhsParse(lhs)
    
    rh.var   <- .rhsVar(formula)
    id.var   <- .rhsVar(id)

    cls <- lapply(data, class)
    
    num.var <- data.var[  cls %in% c("numeric","integer")]
    fac.var <- data.var[!(cls %in% c("numeric","integer"))]

    if (debug.info>=1){
      cat("status:\n")
      cat("  data.var : "); print(data.var)
      cat("  num.var  : "); print(num.var)
      cat("  fac.var  : "); print(fac.var)
      cat("  id.var   : "); print(id.var)
    }
       
    if (length(lh.var)==1)
      lh.var <- list(lh.var)
    
    if (paste(rh.var)[1]=='.')
      rh.var <- NULL
    
    if ("." %in% lh.var){
      lh.var <- setdiff(lh.var, ".")
      v  <- setdiff(num.var, c(lh.var, intersect(rh.var,num.var), intersect(id.var, num.var)))
      isSpecial <- rep(NA,length(v))
      for (j in 1:length(v)){
        isSpecial[j]<- (class(data[,v[j]])[1] %in% c("POSIXt", "factor", "character"))
      }      
      extralh.var<-v[!isSpecial]
      lh.var <- union(lh.var, extralh.var)
      if (debug.info>=1)
        cat("lh.var (new) :", paste(lh.var),"\n")
    }
    
    lh.string <- paste(lh.var, collapse='+')
    if (debug.info>=1)
      cat("lh.string:", lh.string, "\n")

### If no rh.var, set rh.var to those factors in the dataframe which
### do not appear as lh.var or id.var
###
    if (is.null(rh.var) | "." %in% rh.var){
      rh.var <- setdiff(fac.var, c(lh.var, id.var))
      
      if (length(rh.var)==0){
        stop("No factors are identified for grouping...")
      }
          
      if (debug.info>=1)
        {cat(".rh.var: "); print(rh.var)}
    }
    
    rhs.string  <- paste (rh.var, collapse='+')
    str     <- paste(paste(lh.string, "~", rhs.string, collape=''))
    formula <- as.formula(str)
    
    if (debug.info>=1){
      cat("status:\n")
      cat("  rh.var     : ", paste(rh.var),"\n")
      cat("  id.var     : ", paste(id.var),"\n")
      cat("  formula    : ");print(formula)
    }
    
### Function names
###
    
    if (!is.list(FUN)) 
      fun.names <- paste(deparse(substitute(FUN)), collapse = " ")
    else
      fun.names <- unlist(lapply(substitute(FUN)[-1], function(a) paste(a)))
    
    if (!is.list(FUN)) 
      FUN <- list(FUN)


### Get rh data
###

    rh.trivial <- identical(rh.var, "1") ## Is the rhs just 1 ??
    
    if (rh.trivial){
      rh.string <- rep.int(1, nrow(data))
    } else {
    
      rh.data <- data[, rh.var, drop=FALSE]
      rh.string <- apply(rh.data, 1, paste, collapse="@")
      names(rh.string) <- NULL

      #rh.string <<- rh.string
      
      rh.unique <- unique(rh.string)
      rh.idx    <- match(rh.unique, rh.string)
      if (debug.info>=1){
        cat("  rh.var (1) :", paste(rh.var),"\n")
        cat("  rh.string  : "); print (rh.string)
        cat("  rh.idx     : "); print (rh.idx)
      }
    }
    
### Get string for id.vars
###
    if (!is.null(id.var)){
      id.data <-  data[,id.var,drop=FALSE]
      id.string <-  apply(id.data, 1, paste, collapse="@")
      if (debug.info>=1){
        cat("  id.string : "); print(id.string)
      }
      id.data <- id.data[rh.idx,,drop=FALSE]
    }
    
### Get lh data
###
    
    lh.var <- paste(unlist(lh.var))    
    lh.data <- do.call(cbind,lapply(paste(lh.var), function(x)eval(parse(text=x), data)))
    colnames(lh.data) <- lh.var

    
### Calculate groupwise statistics
###
    ans <- NULL
    rh.string.factor <- factor(rh.string, levels=unique(rh.string)) ## This is important
    #print(rh.string.factor)
    for (ff in 1:length(FUN)) {  ## loop over functions
      for (vv in 1:length(lh.var)) {  ## loop over variables
        currFUN <- FUN[[ff]]
        ##         zzz <- tapply(lh.data[,lh.var[vv]], rh.string,
        ##                       function(x){  currFUN(x,...)},
        ##                       simplify=FALSE)
        zzz <- tapply(lh.data[,lh.var[vv]], rh.string.factor,
                      function(x){  currFUN(x,...)},
                      simplify=FALSE)

        zzz  <- do.call(rbind, zzz)
        ans  <- cbind(ans, zzz)
      }
    }

    
### Names for new variables
###  
    if (keep.names){
      if (ncol(ans)==length(lh.var)){
        newnames <- lh.var
      } else {
        keep.names <- FALSE
      }
    }

    ## Dim of response (per variable on LHS)
    dimr <- (ncol(ans))/length(lh.var) 
    oldnames <- colnames(ans)

    if (is.null(oldnames))
      hasNames <- 0
    else {
      hasNames <- 1*(prod(nchar(oldnames))>0)
    }
    
    if (!keep.names){
      if (hasNames>0){
        funNames <- colnames(ans)[1:dimr]         
        newnames <- unlist(lapply(lh.var, function(v){paste(v, funNames, sep='.')}))

      } else {
        if (length(fun.names) != dimr){
          fun.names <- paste("FUN", 1:dimr,sep='')
          newnames <- unlist(lapply(lh.var, function(v){paste(v, fun.names, sep='.')}))
        } else {
          newnames <- unlist(lapply(fun.names, function(x) paste(lh.var, x, sep='.')))
        }
        if (length(newnames)!=ncol(ans)){
          fun.names <- paste(fun.names, 1:dimr, sep=".")
          newnames <- unlist(lapply(fun.names, function(x) paste(lh.var, x, sep='.')))
        }
      }
    }

    colnames(ans) <- newnames
    ans <- as.data.frame(ans)

### Pad the rhs data to the result
###
    if (!rh.trivial){ 
      ##       print("ans:");print(ans)
      ##       print("rh.idx:");print(rh.idx)
      ##       print("rh.data:");print(rh.data)
      ##       print(rh.data[rh.idx,,drop=FALSE])
      ans <- cbind(rh.data[rh.idx,,drop=FALSE], ans)
                                        #print("ans (with rh.data):");print(ans)
    }

    
### Pad id.data to result
###
    if (!is.null(id.var)){
      ans <- cbind(ans, id.data)
                                        #print("ans (pad):");print(ans)
    }

    if (full.dimension){
      rrr <-as.numeric(rh.string.factor)
      ans <- ans[rrr,,drop=FALSE]
    }

### Order the result by the rhs
###
    if (order==TRUE){
      if (!rh.trivial)
        ans <- orderBy(as.formula(paste("~", rhs.string)), data=ans)
                                        #print("ans (ordered):");print(ans)
    }

### Replace '('s and ')'s with '.'s
###
    if (p2d)
      names(ans) <-  gsub("\\)","\\.", gsub("\\(","\\.",names(ans)))

    
    rownames(ans) <- 1:nrow(ans)

    if (length(unique(names(ans))) != length(names(ans)))
      warning("dataframe contains replicate names \n", call.=FALSE)


    
    return(ans)
  }


### -------------------------------------------------------------------




.lhsParse <- function(x){
  ##cat(".lhsParse:"); print(x); print(class(x))
  if (class(x)=='name'){
    value <- x
  } else {
    s <- paste(x[[1]])
    value <- switch(s,
                    '+'={  c(.lhsParse(x[[2]]),.lhsParse(x[[3]]))},
                    'I'={  x[[2]]},
                    {  deparse(x)})
  }
  return(value)
}



.lhsVar <- function(formula) {
  if (!is.null(formula))
    if (length(formula)>=3){
      .xxx. <- formula[[2]]
      unlist(strsplit(paste(deparse(.xxx.), collapse="")," *\\+ *"))
    }
}
.rhsVar <- function(formula) {
  if (!is.null(formula)){
    .xxx. <- formula[[length(formula)]]
    unlist(strsplit(paste(deparse(.xxx.), collapse="")," *\\+ *"))
  }
}







