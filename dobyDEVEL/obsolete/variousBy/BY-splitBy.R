.splitBy <-function (formula, data = parent.frame(),drop=TRUE, return.matrix=FALSE)
{
    mf <- match.call(expand.dots = FALSE)

    ## If formula is a character vector then transform into a formula.

    form <- eval.parent(mf[[2]])
    if (is.character(form)){
      form <- as.formula(paste("~",paste(form, collapse='+')))
      mf[[2]] <- form
    }

    ff <- as.formula(eval.parent(mf[[2]]))
  
    if (ff[[2]]==1)
      {
        ##cat(" The trivial split\n")
        groupData <- list(data)
        names(groupData) <- "1"
        groupid <- as.data.frame(1)
        idxvec  <- list(1:nrow(data))
        grps    <- "1"
      }
    else
      { 
        ff    <- terms(ff, data = data)
        group <- attr(terms(ff), "term.labels")
        
        ## workinggroup: Those variables in 'group' which are **not** constant.
        ##
        varying.group <- rep(TRUE, length(group))      
        names(varying.group) <- group
        for (i in 1:length(group)){
          nunique <- length(unique(data[,group[i]]))
          if (nunique==1)
            varying.group[i] <- FALSE
        }
        workinggroup <- group[varying.group]            
      
        if (length(workinggroup)==0)
          {
            groupData <- list(data)
            names(groupData) <- "1"
            groupid <- data[1,group,drop=FALSE]
            idxvec  <- list(1:nrow(data))
            grps    <- "1"
          }
        else
          {
            ## grps: Recode groups into one vector; aa|b|xx etc...
            ##
            #cat("The general case: non-trivial grouping...\n")
            grps <- data[, workinggroup, drop=FALSE]
            
            grpsvec<-paste(grps[,1])
            if (ncol(grps)>1){
              for (jj in 2:ncol(grps)){
                #grpsvec <- paste(paste(grps[,j]),grpsvec,sep='|')
                grpsvec <- paste(grpsvec,paste(grps[,jj]),sep='|')
              }
            }
            ## Same as:
            ##grpsvec <- apply(grps,1, paste, collapse='|')
            
            grps <- grpsvec ## 
            #print(grps)
            
            dataMatrix <- .asNumericMatrix2(data)

#            grps <<- grps
#            dataMatrix <<- dataMatrix
            
            at    <- .subsAttr2(data)
#            at2<<-at
            #alist <- mApply(dataMatrix, grps, function(x){x}, simplify=FALSE)
            alist <- .splitMatrix(dataMatrix,grps)
#            aaa<<-alist
            if (drop==TRUE)
              alist <- alist[lapply(alist,nrow)>0]
            
            if (return.matrix==TRUE){
              groupData <- alist
            } else {
              groupData <- lapply(alist, .matrix2dataFrame2, at=at, restoreAll=FALSE)
            }

            groupid        <- lapply(groupData, function(x) x[1,group,drop=FALSE])            
            names(groupid) <- NULL
            groupid        <- as.data.frame(do.call('rbind',groupid))
            names(groupid) <- group
            rownames(groupid) <- 1:nrow(groupid)     

            not.dup <- !duplicated(grps)            
            uniq.grps <- grps[not.dup]
            #uniq.grps <- unique(grps)
            #print(uniq.grps)
            #print(grps[!duplicated(grps)])

            idxvec <- vector("list", length(uniq.grps))
                                        #print(uniq.grps)
            names(idxvec) <- uniq.grps
            for (ii in 1:length(uniq.grps)){
              idxvec[[ii]] <- which(grps==uniq.grps[ii])
            }      
            
          }
      }
    
    attr(groupData,"groupid") <- groupid
    attr(groupData,"idxvec")  <- idxvec
    attr(groupData,"grps")    <- grps
    
    class(groupData) <- c("splitByData", "list")
    return(groupData)
  }


.splitMatrix <- function(dataMatrix, grps){
  idx <- 1:nrow(dataMatrix)
  ggg <- split(idx, grps)
  lapply(ggg, function(iii) dataMatrix[iii,,drop=FALSE])
}



print.splitByData <- function(x,...){
#  print(attr(x,"groupid"))
  print(cbind(listentry=names(x), attr(x,"groupid")))
  return(invisible(x))
}



