
splitByp<-function (formula, data = parent.frame(),drop=TRUE, return.matrix=FALSE)
{
    mf <- match.call(expand.dots = FALSE)
    
    m <- match(c("formula", "data", "subset", "weights", "na.action",
                 "offset"), names(mf), 0)
    ff <- as.formula(eval.parent(mf[[2]]))
    if (ff[[2]]==1){
      groupData <- list(data)
      attr(groupData,"groupid") <- 1
    } else {
      
      ff    <- terms(ff, data = data)
      m     <- match.call(expand.dots = TRUE)
      group <- attr(terms(ff), "term.labels")
      
      ## workinggroup: Those variables in 'group' which are not constant.
      nonconstgroup <- rep(TRUE, length(group))
      
      names(nonconstgroup) <- group
      for (i in 1:length(group)){
        nunique <- length(unique(data[,group[i]]))
        ##cat("Variable:", group[i], "unique values:", nunique, "\n")
        if (nunique==1)
          nonconstgroup[i] <- FALSE
      }
      workinggroup <- group[nonconstgroup]            
      
      ## grps: Recode groups into one vector
      grps <- data[, workinggroup,drop=FALSE]
      grpsvec<-paste(grps[,1])
      if (ncol(grps)>1){
        for (j in 2:ncol(grps)){
          grpsvec <- paste(grpsvec,paste(grps[,j]),sep='|')
        }}
      grps <- grpsvec

      grps   <<- grps
      grpsid <<- grpsid <- unique(grps)
      group  <<- group

#       idv <- rep(0, length(grpsid))
#       for (ii in 1:length(grpsid)){
#         idv[ii] <- which(grpsid[ii]==grps)[1]
#       }

      aaa <-  which(as.logical(diff(as.numeric(factor(grps)))))
      startv <- c(1, 1+aaa)
      endv   <- c(aaa, length(grps))

      groupid <- data[startv, group]

      
      val <- structure(list(data=data, grps=grps, grpsid=grpsid, groupid=groupid,
                            length=length(grpsid)), class="jensfup")

      return(val)
    }

    #return(groupData)
}


print.jensfup <- function(x, ...){
  print(x$groupid)
}

select <- function(val, i) UseMethod("select")
select.jensfup <- function(val,i){
  val$data[which(grpsid[i]==grps), , drop=FALSE]
}
