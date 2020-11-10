shweave <- function(File){
  f1 <- paste(File, ".Rnw", sep='')
  Sweave(f1)
  Stangle(f1)

  f2 <- paste("latex ", File, ".tex", sep='')
  cat("Running latex on file", f2, "\n") 
  system(f2, invisible=TRUE)
}


logit<-function(x,eps=1e-3){
    sapply(x,function(a){#print(a);
            if (a==0) log((a+eps)/(1-(a+eps))) else log(a /(1-a))})
}

expit<-function(x){
    sapply(x,function(a){exp(a)/(1+exp(a))})
}


shread.table <- function(filename,path="",header=T,na.strings="*",tolower=T){
    full.path   <- paste(path,filename,sep="")
    data <- read.table(full.path, header=header,na.strings=na.strings)
    if (tolower==T)
        names(data) <- tolower(names(data))
    return(data)
    }


mysavePlot  <- function(path="",filename,type="epspdf"){
  full.path   <- paste(path,filename,sep="")
  if (type=="epspdf"){
    R.type1 <- "ps"
    R.type2 <- "pdf"
    savePlot(full.path,type=R.type1)
    savePlot(full.path,type=R.type2)
  }
  else{
    if (type=="eps" || type=="ps")
      R.type  <- "ps"
    else
      R.type  <- type;
    savePlot(full.path,type=R.type)
  }
  if (type=="eps" || type=="epspdf"){
    print(paste(full.path,".ps",sep=''))
    str <- paste("move ", paste(full.path,".ps",sep=''), paste(full.path,".eps",sep=''))
    print (str)
    shell(str)
  }
}

shsavePlot  <- function(filename="Rfigure", path=".\\",type="epspdf"){
  full.path   <- paste(path,filename,sep="")
  if (type=="epspdf"){
    R.type1 <- "ps"
    R.type2 <- "pdf"
    savePlot(full.path,type=R.type1)
    savePlot(full.path,type=R.type2)
  }
  else{
    if (type=="eps" || type=="ps")
      R.type  <- "ps"
    else
      R.type  <- type;
    savePlot(full.path,type=R.type)
  }
  if (type=="eps" || type=="epspdf"){
    print(paste(full.path,".ps",sep=''))
    str <- paste("move ", paste(full.path,".ps",sep=''), paste(full.path,".eps",sep=''))
    print (str)
    shell(str)
  }
}

wrapPlot <- function(expr, file="Rfigure", path=".\\",type="epspdf",width=4,height=4){
  full.path      <- paste(path,file,sep="")
  full.path.eps   <- paste(path,file,".eps",sep="")
  full.path.pdf  <- paste(path,file,".pdf",sep="")
  ##print(full.path.ps)
  postscript(full.path.eps, width=width,height=height,horizontal=F)
  eval(expr)
  dev.off()
  pdf(full.path.pdf, width=width,height=height,horizontal=F)
  eval(expr)
  invisible(dev.off())
}

df2bugs <- function(df, extra=F,file=F){
#Example:
#df      <- data.frame(cbind(c("a","a", "b","b"), c(1,2,3,4)))
#df$X2   <- as.numeric(df$X2)
#df2bugs(df,extra="N=6",file="d:\\tst.txt")
result  <- NULL
for(j in 1:dim(df)[2]){
    item        <- df[,j]
    item.name   <- names(df)[j] 
    item.str    <- paste(
        paste(item.name, " = c("),
        paste(as.numeric(item),collapse=", "),
        paste(") "))
    if (j<dim(df)[2])    
        result  <- paste(result, item.str,", " )
    else 
        result  <- paste(result, item.str)    
    }    
    if (extra!=F)
        result  <- paste("list(",result, ",", extra, ")")
    else
        result  <- paste("list(",result, ")")
    if (file!=F)
        write(result, file=file,ncolumns=50)
    return(result)
}



df.round    <-function(df,digits){
    for (j in 1:ncol(df))
        if (is.numeric(df[,j]))
            df[,j]  <- round(df[,j],digits)
    return(df)
}


### General utility

list2df <- function(lll, add.id=FALSE){
  value <- NULL
  for (l in 1:length(lll)){
    v <- lll[[l]];
    if (add.id==TRUE) v["id"] <- l
    value <- rbind(value, v)
    }
  if (add.id==TRUE){
    names(value) <- c(names(lll[[1]]), ".listid")
    value$.listid <- factor(value$.listid)
  } else {
    names(value) <- names(lll[[1]])
  }
  return(value)
  
}



sel.col <-function(col, data, exclude=F){
    ind <- which.index(col,names(data)) ; 
    col.in.data <- which(ind!='NA')
    col.not.in.data <- setdiff(1:length(col), col.in.data)
    #print(col.in.data);    print(col.not.in.data);
    if (length(col.not.in.data)>0){
        cat("Warning: Variables ", col[col.not.in.data],"not in data frame",fill=T)
        ind <- ind[which(ind!='NA')]
        }
    if (exclude==T)
         data[,-ind]
    else
         data[,ind]
}



which.index <- function(str, names){
  as.numeric(sapply(str, function(a){which(a==names)}))
}


extract.path.file   <-  function(str){
  newstr      <- str
  head.length <- 0
  first1      <- regexpr("/",newstr)
  first2      <- regexpr("\\\\",newstr)
  i           <- max(c(first1,first2))
  if (i > 0)
    while( i > 0 ){
      head.length <- head.length + i;
      newstr   <- substring(newstr,i+1)    
      first1   <- regexpr("/",newstr)
      first2   <- regexpr("\\\\",newstr)
      prev.i   <- i
      i        <- max(c(first1,first2))
      print(newstr)
    }
  if (head.length>0)
    c(newstr,substr(str,1,head.length))    
  else
    c(newstr)    
}

sh.scatter  <- function(data, x, y){
    par(mfrow=c(length(x),length(y)))
    x.i <- get.index( data, x );
    y.i <- get.index( data, y );
    for (i in 1:length(x.i))
        for (j in 1:length(y.i))
            plot(data[,x.i[i]], data[,y.i[j]], xlab=paste(names(data)[x.i[i]]),
                 ylab=paste(names(data)[y.i[j]]))
}  
  
get.index   <-  function(item, all.names){
    if (is.data.frame(item)==T)
        names.list <- names(item)
    else
        names.list   <- item;
    n.i     <- rep(F, length(all.names));
    for (i in 1:length(all.names)){
        n.i[ i ] <- which( all.names[i]==names.list )}
    n.i
}
  
