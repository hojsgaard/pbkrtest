

histBy <- function(x, group=NULL, lines=TRUE, title="",
  group.col=FALSE, data=parent.frame(), smooth=FALSE, ... ){

    ep      <- FALSE;
    sh.eprint<- function(x){
      if (ep==TRUE){str <- deparse(substitute(x)); cat(paste("E>>", str," =", x),fill=TRUE)}}

    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
      m$data <- as.data.frame(data)
    x.name <- deparse(m$x)
    x      <- data[,x.name]
    
    if (is.null(m$group)){
      by.names <- ".default.by.name"
      by.data <- rep(".by.",length(x))
      group <- by.data
      several.groups <- FALSE
    }
    else{

      if (length(m$group) > 1)
        sssby <- m$group[2:length(m$group)]
      else
        sssby <- m$group

      by.names <- paste(sssby);
      by.data <- data[, by.names,FALSE]
      for(j in 1:ncol(by.data))
        by.data[,j] <- as.factor(by.data[,j])


      mmm <- by.data[,by.names[1]];
      if (length(by.names)>1)
        for (i in 2:length(by.names)) mmm <- mmm:by.data[,by.names[i]]
      mmm   <- ordered(mmm)
      group <- mmm
      several.groups <- TRUE
    }
    group.name  <- paste(by.names,collapse=":")
    new.data        <- data.frame(x, group); 
    names(new.data) <- c(x.name, group.name)
    
    group.data  <- by( new.data, group, function(x){x})
    
    for (grp.id in 1:length(group.data)){
      
      curr.group    <-  group.data[[grp.id]];
      curr.group.id <-  paste(curr.group[,group.name][1])
      title.str <- ifelse(!is.null(m$group), paste(title, group.name,"=", curr.group.id,sep=''), title)
      cur.col <-  ifelse(group.col !=FALSE,   group.col[grp.id], FALSE) 

      if (smooth==FALSE){
        h <-hist(curr.group[,x.name], main=title.str,xlab=x.name,col=cur.col);
      }
      else {
        f <- ash1(bin1(curr.group[,x.name],nbin=smooth),5) # compute ash estimate
        h<-hist(curr.group[,x.name],prob=TRUE,plot=FALSE)
        max.y<-round(max(c(h$density, max(f$y))),2)
        hist(curr.group[,x.name], main=title.str,xlab=x.name,col=cur.col,prob=TRUE,ylim=c(0,max.y))
        lines( f , type="l" )    # line plot of estimate
      }      
    }
    value <- NULL
  }



  
###########################################################################################
