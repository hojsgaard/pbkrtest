
require("ash")

###########################################################################################
###########################################################################################
###########################################################################################
plot.by   <-
  function(x, y=NULL,  subject=NULL, group=NULL,
           data = parent.frame(),
           gFun=NULL,  
           Fun=NULL,        
           title="PLOT", lines=FALSE,
           same.axis=TRUE,
           group.col=NULL, group.pch=NULL, silent=TRUE
           , xlim=range(x), ylim=range(y),...
           ){

    ep        <- FALSE;
    sh.eprint <- function(x){
      if (ep==TRUE){str <- deparse(substitute(x)); cat(paste("E>>", str," =", x),fill=TRUE)}}

    m <- match.call(expand.dots = TRUE)
    if (is.matrix(eval(m$data, parent.frame()))) 
      m$data <- as.data.frame(data)

    lwd <- eval(m$lwd)
    col <- eval(m$col); 
    pch <- eval(m$pch)
    ##gFun <- (m$gFun)
    xlab<- m$xlab
    ylab<- m$ylab
    xlim<- eval(m$xlim)
    ylim<- eval(m$ylim)

    lty <- eval(m$lty)

    sym    <- pch
    group.sym <- group.pch
    ##sh.eprint(lty); sh.eprint(group.sym)
    only.x         <- FALSE
    several.groups <- ifelse(is.null(by), FALSE, TRUE)

    x.name <- deparse(m$x)
    y.name <- deparse(m$y)

    x <- eval(m$x, data)
    y <- eval(m$y, data)

    if (is.null(m$group)){
      by.names <- ".default.by.name"
      by.data <- rep(".by.",length(x))
      group.id.data <- by.data
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
      group.id.data <- mmm
    }
    group.name  <- paste(by.names,collapse="")

    

    ### Subjects
    if (is.null(m$sub)){
      sub.names <- ".default.sub.name"
      sub.group <- rep(".sub.",length(x))
    }
    else{
      if (length(m$sub)>1)
        ssssub <- m$sub[2:length(m$sub)]
      else
        ssssub <- m$sub
      
      sub.names <- paste(ssssub);
      ##sh.eprint(sub.names)
      sub.data <- data[, sub.names, drop=FALSE]

      mmm <- sub.data[,sub.names[1]];
      if (length(sub.names)>1)
        for (i in 2:length(sub.names)) mmm <- mmm:sub.data[,sub.names[i]]
      mmm   <- ordered(mmm)
      sub.group <- mmm

    }
    sub.group.name  <- paste(sub.names,collapse="")


    if ( is.null(y)) {
      ##eprint("NO Y")
      only.x  <- TRUE;
      y  <- x;
      x.index <- unlist(by (x, list(sub.group, group.id.data), function(a){1:length(a)}))
      x  <- x.index;
    }

    ##print(y)
    new.data        <- data.frame(x, y, sub.group, group.id.data); ##print(data)
    names(new.data) <- c(x.name, y.name, sub.group.name, group.name)
    ##eprint(c(x.name, y.name, sub.group.name, group.name))

    ##print(names(new.data))

    
    xlabel <- if (only.x) "index" else x.name ;
    ylabel <- if (only.x) x.name else y.name

    if (!is.null(group.id.data[1])){   
      group.data <-  by( new.data, group.id.data, function(x){x})
    }
    else{
      group.data <-  by( data, rep(1, dim(data)[1]), function(x){x})
    }

    ## Avoid overwriting globals...    
    g.x.exists  <- ifelse (exists("group.x"),   {proc.plot.tmp...g.x <- group.x;   TRUE}, FALSE)
    g.y.exists  <- ifelse (exists("group.y"),   {proc.plot.tmp...g.y <- group.y;   TRUE}, FALSE)
    x.exists    <- ifelse (exists("subject.x"), {proc.plot.tmp...x   <- subject.x; TRUE}, FALSE)
    y.exists    <- ifelse (exists("subject.y"), {proc.plot.tmp...y   <- subject.y; TRUE}, FALSE)


    
    ## Work through the groups
    ## ###########################################
    for (grp.id in 1:length(group.data)){       
      curr.group    <-  group.data[[grp.id]];
      curr.group.id <-  paste(curr.group[,group.name][1])
      ## Group symbols
      ##print(curr.group)
      cur.sym <- ifelse(group.sym!=FALSE, 
                        ifelse(group.sym !=FALSE,   group.sym,  group.sym[grp.id]),
                        ifelse(lines==TRUE,    " ",    "o"))
      
      ## Group colors
      sh.eprint(group.col)
      ## cur.col <-  ifelse(!is.null(group.col),   group.col,  group.col[grp.id])
      cur.col <-  ifelse(!is.null(group.col),
                         ifelse(length(group.col)>1, group.col[grp.id], group.col), 1)
      
      ## Line type
      #cur.lty <- ifelse (lty!=1, ifelse(length(lty)==1,  lty,    lty[grp.id]), 1)

      cur.lty <- ifelse (!is.null(lty),
                         ifelse(length(lty)>1, lty[grp.id], lty), 1)
      
      sub.list <- if (!is.null(sub.group.name)) {
        by(curr.group, factor(curr.group[,sub.group.name]), function(d){d})
      }
      else{
        list(curr.group)
      }

      ## Make empty plot with desired dimensions
      xlab <- ifelse(!is.null(xlab), xlab, xlabel)
      ylab <- ifelse(!is.null(ylab), ylab, ylabel)
      xaxt <- ifelse(is.factor(x), "n", "s")
      ##cat("xlim=",paste(xlim),"ylim=",paste(ylim),      "......\n")
      if (same.axis==TRUE){
        ##print(1111111); print(ylim)
        plot(as.numeric(x), y,                       pch=" ", xlab=xlab, ylab=ylab, xaxt=xaxt, xlim=xlim, ylim=ylim)
      }
     else{
  ##      plot(as.numeric(curr.group$x), curr.group$y, pch=" ", xlab=xlab, ylab=ylab, xaxt=xaxt)#, xlim=xlim, ylim=ylim)

        plot(as.numeric(curr.group[,1]), curr.group[,2], pch=" ", xlab=xlab, ylab=ylab, xaxt=xaxt, xlim=xlim, ylim=ylim) 
        ##print(curr.group)
        ##print(as.numeric(curr.group[,1])); print(curr.group[,2])
      }
      if (is.factor(x)) axis(1,1:length(levels(x)), levels(x))

      ## Handle Titles
      title.str <- ifelse(several.groups != F, paste(title, curr.group.id,sep=''), title)
      title( title.str )

      group.x <<- curr.group[,x.name]; group.y <<- curr.group[,y.name];
      group.id<<- grp.id; cur.col <<- cur.col; cur.lty <<- cur.lty
      sh.eprint(group.args)

      
      if(!is.null(gFun)) do.call(gFun,NULL)


      
    ## Work through the subjects within groups
    #############################################
      for (sub.id in 1:length(sub.list)){
        sub.data   <- sub.list[[sub.id]];
        sub.id.str <- paste(sub.data[,sub.group.name][1]); ##print(sub.id.str)
        if (only.x == TRUE){
          sub.data[,x.name] <- 1:length(sub.data[,y.name]);
        }

        ##print(group.col); print(sub.id); print(col); print(length(col))
        cur.col <- ifelse(!is.null(group.col), cur.col,
                          ifelse(!is.null(col),
                                 ifelse(length(col)==1, col, col[sub.id]),  1))[1]
        ##print(cur.col)
        cur.sym <- ifelse(!is.null(group.sym),
                          ifelse(length(group.sym)==1, group.sym, group.sym[grp.id]),
                          ifelse(!is.null(sym), ifelse(length(sym)==1, sym, sym[sub.id]),  1))[1]
        ##sh.eprint(group.sym);        sh.eprint(cur.sym);        sh.eprint(sym);        print(sym)
        
        ##cur.lty <- ifelse(!is.null(lty), ifelse(length(lty)==1, lty, lty[sub.id]), 1)[1]

        ##print(lty); print(grp.id)
        cur.lty <- ifelse (!is.null(lty),
                           ifelse(length(lty)>1, lty[sub.id], lty), 1)


        ##sh.eprint(lty);         sh.eprint(cur.lty)
        if (silent==FALSE)
          cat("Symbol:", cur.sym, "Color:", cur.col, "By:", curr.group.id, 
              "Subject:", sub.id.str, "Line:", cur.lty, sep=' ', fill=TRUE)
        ##sh.eprint(cur.sym);        sh.eprint(cur.lty)

        points(sub.data[,x.name], sub.data[,y.name], pch=cur.sym, col=cur.col,lwd=lwd)
        if (lines == TRUE){
          lines (sub.data[,x.name], sub.data[,y.name], col=cur.col,lwd=lwd,lty=cur.lty)  
        }
        subject.x <<- sub.data[,x.name]; subject.y <<- sub.data[,y.name];
        subject.id<<- sub.id; cur.col <<- cur.col; cur.lty <<- cur.lty
        if(!is.null(Fun)) do.call(Fun,NULL)
      }   
    }
    remove(list=c("subject.x","subject.y","group.x","group.y"), pos=1);
    if (g.x.exists) group.x   <<- proc.plot.tmp...g.x
    if (g.y.exists) group.y   <<- proc.plot.tmp...g.y
    if (  x.exists) subject.x <<- proc.plot.tmp...x
    if (  y.exists) subject.y <<- proc.plot.tmp...y
    value <- TRUE
  }
    


###########################################################################################
###########################################################################################
###########################################################################################


qqnorm.by <-
  function(x, group=NULL, lines=TRUE, title="", data=parent.frame(), ... ){

    ep      <- FALSE;
    sh.eprint<- function(x){
      if (ep==T){str <- deparse(substitute(x)); cat(paste("E>>", str," =", x),fill=T)}}

    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
      m$data <- as.data.frame(data)
    ##print(m)

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
      ##print(curr.group)
      curr.group.id <-  paste(curr.group[,group.name][1])
      ##print(curr.group.id)
      title.str <- ifelse(!is.null(m$group), paste(title, group.name,"=", curr.group.id,sep=''), title)
      
      qqnorm(curr.group[,x.name], main=title.str);
      ##print(curr.group[,x.name])
      if (lines==TRUE) qqline(curr.group[,x.name]);
    }
    value <- NULL
  }

###########################################################################################
###########################################################################################
###########################################################################################


hist.by <- function(x, group=NULL, lines=TRUE, title="",
  group.col=FALSE, data=parent.frame(), smooth=FALSE, ... ){

    ep      <- FALSE;
    sh.eprint<- function(x){
      if (ep==T){str <- deparse(substitute(x)); cat(paste("E>>", str," =", x),fill=T)}}

    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
      m$data <- as.data.frame(data)
    ##print(m)
    
    x.name <- deparse(m$x)
    x      <- data[,x.name]


    
    if (is.null(m$group)){
      by.names <- ".default.by.name"
      by.data <- rep(".by.",length(x))
      group <- by.data
      several.groups <- F
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
      several.groups <- T
    }
    group.name  <- paste(by.names,collapse=":")
    new.data        <- data.frame(x, group); 
    names(new.data) <- c(x.name, group.name)
    
    group.data  <- by( new.data, group, function(x){x})
    
    for (grp.id in 1:length(group.data)){
      
      curr.group    <-  group.data[[grp.id]];
      curr.group.id <-  paste(curr.group[,group.name][1])
      title.str <- ifelse(!is.null(m$group), paste(title, group.name,"=", curr.group.id,sep=''), title)
      cur.col <-  ifelse(group.col !=F,   group.col[grp.id], F) 

      if (smooth==FALSE){
        h <-hist(curr.group[,x.name], main=title.str,xlab=x.name,col=cur.col);
      }
      else {
        f <- ash1(bin1(curr.group[,x.name],nbin=smooth),5) # compute ash estimate
        h<-hist(curr.group[,x.name],prob=T,plot=F)
        max.y<-round(max(c(h$density, max(f$y))),2)
        hist(curr.group[,x.name], main=title.str,xlab=x.name,col=cur.col,prob=T,ylim=c(0,max.y))
        lines( f , type="l" )    # line plot of estimate
      }      
    }
    value <- NULL
  }



  
###########################################################################################
###########################################################################################
###########################################################################################
  
  
summary.by <- function(formula = ~., data = parent.frame(), subset, na.action, 
                       FUN=sum,...){

  ep      <- FALSE;
  sh.eprint<- function(x){
    if (ep==T){str <- deparse(substitute(x)); cat(paste("E>>", str," =", x),fill=T)}}
  
  nfac        <- length(attr(terms(formula),"order"))
  fun.names   <- if (!is.list(FUN))
    paste(deparse(substitute(FUN)))
  else
    unlist(lapply( substitute(FUN)[-1], function(a)paste(a)))
  if (!is.list(FUN)) FUN <- list(FUN);
  
  if (!missing(formula) && !inherits(formula, "formula")) 
    stop("formula is incorrect")
  if (any(attr(terms(formula), "order") > 1)) 
    stop("interactions are not allowed")
  if (missing(na.action)) 
    na.action <- getOption("na.action")
  
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m$... <- m$exclude <- m$drop.unused.levels <- NULL
  m$... <- m$exclude <- m$FUN <- NULL
  m[[1]] <- as.name("model.frame")
  mf <- eval(m, parent.frame())

  y.names     <- dimnames(mf[,1])[[2]]

  a <- lapply(FUN,function(f) doby.xtabs(formula,FUN=f,data=data) )
  adf         <- as.data.frame(a[[1]])

  if (length(y.names) > 1){
    fac.frame   <- adf[adf[,(nfac+1)]==levels(adf[,(nfac+1)])[1],][,1:nfac,drop=FALSE]
    b <- lapply(a, function(a2)
                apply(a2,(nfac+1),function(a) a) )
    for (i in 1:length(b)){
      var.name <- unlist(dimnames(b[[i]])[2])
      new.names <- paste(fun.names[i], var.name,sep='.')
      dimnames(b[[i]])[2][[1]] <- new.names
    }
    r <- fac.frame;
    for (i in 1:length(b))  r <- cbind(r,b[[i]])
  }
  else{
    fac.frame   <- adf[,1:nfac,drop=FALSE]
    b <- lapply(a,function(a2) as.data.frame(a2)[,(nfac+1),drop=F])
    b <- as.data.frame(b)
    if (is.null(y.names)) y.names <- names(mf)[1]
    
    new.names <- paste(fun.names,y.names,sep='.')
    names(b) <- new.names
    r <- cbind(fac.frame, b)
  }


  gm <- paste(formula[[3]])
  print(gm)
  var<-gm[setdiff(1:length(gm), grep("\\+",gm))]
  print(var)
  
  for (v in var){
    print(v)
    print(data[,v])
  }
  value <- r
  return(value)
}




###########################################################################################
###########################################################################################
###########################################################################################

doby.xtabs <-
function (formula = ~., data = parent.frame(),
          subset, na.action, exclude = c(NA, NaN), drop.unused.levels = FALSE,
          FUN=sum) 
{
  ep      <- FALSE;
  sh.eprint<- function(x){
    if (ep==T){str <- deparse(substitute(x)); cat(paste("E>>", str," =", x),fill=T)}}
  if (!missing(formula) && !inherits(formula, "formula")) 
    stop("formula is incorrect")
  if (any(attr(terms(formula), "order") > 1)) 
    stop("interactions are not allowed")
  if (missing(na.action)) 
    na.action <- getOption("na.action")
  
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m$... <- m$exclude <- m$drop.unused.levels <- NULL
  m$... <- m$exclude <- m$FUN <- NULL
  m[[1]] <- as.name("model.frame")
  mf <- eval(m, parent.frame())
  if (length(formula) == 2) {
    by <- mf
    y <- NULL
  }
  else {
    i <- attr(attr(mf, "terms"), "response")
    by <- mf[-i]
    y <- mf[[i]]
  }
  by <- lapply(by, function(u) {
    if (!is.factor(u)) 
      u <- factor(u, exclude = exclude)
    u[, drop = drop.unused.levels]
  })
  x <- if (is.null(y)){ 
    do.call("table", by)
  } else {
    if (NCOL(y) == 1){
      tapply(y, by, FUN) 
    } else {
      z <- lapply(as.data.frame(y), tapply, by, FUN)
      array(unlist(z), dim = c(dim(z[[1]]), length(z)), dimnames = c(dimnames(z[[1]]), 
                                                        list(names(z))))
    }
  }
  
  x[is.na(x)] <- 0
  class(x) <- c("xtabs", "table")
  attr(x, "call") <- match.call()

  value <- x
  print(value)
  return(value)
}


doby.xtabs <-
function (formula = ~., data = parent.frame(),
          subset, na.action, exclude = c(NA, NaN), drop.unused.levels = FALSE,
          FUN=sum) 
{
  ep      <- FALSE;
  sh.eprint<- function(x){
    if (ep==T){str <- deparse(substitute(x)); cat(paste("E>>", str," =", x),fill=T)}}
  
  
  if (!missing(formula) && !inherits(formula, "formula")) 
    stop("formula is incorrect")
  if (any(attr(terms(formula), "order") > 1)) 
    stop("interactions are not allowed")
  if (missing(na.action)) 
    na.action <- getOption("na.action")
  
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m$... <- m$exclude <- m$drop.unused.levels <- NULL
  m$... <- m$exclude <- m$FUN <- NULL
  m[[1]] <- as.name("model.frame")
  mf <- eval(m, parent.frame())
  if (length(formula) == 2) {
    by <- mf
    y <- NULL
  }
  else {
    i <- attr(attr(mf, "terms"), "response")
    by <- mf[-i]
    y <- mf[[i]]
  }

  by <- lapply(by, function(u) {
    if (!is.factor(u)) 
      u <- factor(u, exclude = exclude)
    u[, drop = drop.unused.levels]
  })
  x <- if (is.null(y)) 
    do.call("table", by)
  else if (NCOL(y) == 1){
                                        #print("HERE1") 
                                        #tapply(y, by, mean)
        tapply(y, by, FUN) 
      }
  else {
                                        #print("HERE2")
                                        #z <- lapply(as.data.frame(y), tapply, by, mean)
    z <- lapply(as.data.frame(y), tapply, by, FUN)
    
    array(unlist(z), dim = c(dim(z[[1]]), length(z)), dimnames = c(dimnames(z[[1]]), 
                                                        list(names(z))))
  }
  x[is.na(x)] <- 0
  class(x) <- c("xtabs", "table")
  attr(x, "call") <- match.call()
  x
  value <- x
  return(value)
}

######################################################################


power.by <- function(formula = ~., data = parent.frame()){

  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m[[1]] <- as.name("model.frame")
  mf <- eval(m, parent.frame())
  y.names     <- dimnames(mf[,1])[[2]]

  sby <- summary.by(formula, data=data,FUN=c(mean,var))
  
  l <- lapply(y.names, function(a)
              c(paste("mean.",a,sep=''),paste("var.",a,sep=''))) 
  
  var.index <- match(unlist(l), names(sby))
  for (j in var.index){
    sby[,j] <- log(sby[,j])
  }

  l2 <- lapply(y.names, function(a)
              c(a,paste("mean.",a,sep=''),paste("var.",a,sep=''))) 

  lapply(l2, function(a)
         {
           d <- sby[,match(a[-1], names(sby))]
           plot(d[,1],d[,2],
                xlab=paste("log(",a[2],")",sep=''),
                ylab=paste("log(",a[3],")",sep=''))
           lm.fit <- lm(d[,2]~d[,1])
           abline(lm.fit)
           s <- summary(lm.fit)
           se <- sqrt(s$cov[2,2])*s$sigma
           title(paste(a[1], "- slope=", round(lm.fit$coef[2],2),
                       "(", round(se,2), ")")) 
         })
  return(sby)
}






importcsv <- function(file){
    value <- read.table(file,  header=TRUE, sep=",", na='.')
}

exportcsv <- function(data, file){ 
    write.table(data, file=file, na='.', sep = ",",row.names=FALSE)
}
