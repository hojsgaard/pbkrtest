###
### Printing with names as columns - nprint (narrow print)
### 

nprint<-function (x, ..., digits = NULL, quote = FALSE, right = TRUE) 
{
    if (length(x) == 0) {
        cat("NULL data frame with", length(row.names(x)), "rows\n")
    }
    else if (length(row.names(x)) == 0) {
        print.default(names(x), quote = FALSE)
        cat("<0 rows> (or 0-length row.names)\n")
    }
    else {
        d3 <- .getinshape(x,...,digits=digits,quote=quote,right=right)
        print(d3, ..., quote = quote, right = right)
    }
    invisible(x)
}

nhead<-function (x, n = 6, ...) {
  dd<-.getinshape(x,...)
  dd[seq(len = min(n+attr(dd,'headlength'), nrow(x))), , drop = FALSE]
}

ntail<-function (x, n = 6, ...) 
{
    dd<-.getinshape(x,...)
    nrx <- nrow(dd)
    dd[c(1:attr(dd,'headlength'),  seq(to = nrx, length = min(n, nrx))), , drop = FALSE]
}


.getinshape <- function(x,...,digits=NULL,quote=FALSE,right=TRUE){
        nam   <- names(x)
        nam3  <- strsplit(nam,"")
        nchar <- max(unlist(lapply(nam3,length)))
        for (i in 1:length(nam3))
            nam3[[i]] <- c(rep("", nchar-length(nam3[[i]])),nam3[[i]])
        nam3<-as.data.frame(nam3)
        rownames(nam3) <- paste("-",rownames(nam3),sep='')
        d2 <- as.matrix(format.data.frame(x, digits = digits, 
              na.encode = FALSE))
        newnam <- paste("V",1:ncol(d2),sep='')
        colnames(d2)<- names(nam3) <- newnam
        d3 <- rbind(nam3,d2)
        attr(d3,'headlength') <- nrow(nam3)
    return(d3)
}
