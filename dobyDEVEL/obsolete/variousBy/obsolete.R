plot.by   <-
  function(x, y=NULL,  subject=NULL, group=NULL, data = parent.frame(),
           gFun=NULL, Fun=NULL, title="PLOT", lines=FALSE, same.axis=TRUE,
           group.col=NULL, group.pch=NULL, silent=TRUE
           , xlim=range(x), ylim=range(y),...
           ){

    subject.count <- 1
    sh.eprint <- function(x){
      if (ep==TRUE){str <- deparse(substitute(x)); cat(paste("E>>", str," =", x),fill=TRUE)}}
    ep        <- FALSE;

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
      only.x  <- TRUE;
      y  <- x;
      x.index <- unlist(by (x, list(sub.group, group.id.data), function(a){1:length(a)}))
      x  <- x.index;
    }

    new.data        <- data.frame(x, y, sub.group, group.id.data); ##print(data)
    names(new.data) <- c(x.name, y.name, sub.group.name, group.name)
    
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
      cur.sym <- ifelse(group.sym!=FALSE, 
                        ifelse(group.sym !=FALSE,   group.sym,  group.sym[grp.id]),
                        ifelse(lines==TRUE,    " ",    "o"))
      
      ## Group colors
      cur.col <-  ifelse(!is.null(group.col),
                         ifelse(length(group.col)>1, group.col[grp.id], group.col), 1)
      
      ## Line type
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
      if (same.axis==TRUE){
        plot(as.numeric(x), y, pch=" ", xlab=xlab, ylab=ylab, xaxt=xaxt, xlim=xlim, ylim=ylim)
      } else {
        plot(as.numeric(curr.group[,1]), curr.group[,2], pch=" ", xlab=xlab, ylab=ylab, xaxt=xaxt, xlim=xlim, ylim=ylim) 
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
   
        cur.lty <- ifelse (!is.null(lty),
                           ifelse(length(lty)>1, lty[sub.id], lty), 1)
        if (silent==FALSE)
          cat("Symbol:", cur.sym, "Color:", cur.col, "By:", curr.group.id, 
              "Subject:", sub.id.str, "Line:", cur.lty, sep=' ', fill=TRUE)

        points(sub.data[,x.name], sub.data[,y.name], pch=cur.sym, col=cur.col,lwd=lwd)
        if (lines == TRUE){
          lines (sub.data[,x.name], sub.data[,y.name], col=cur.col,lwd=lwd,lty=cur.lty)  
        }
        subject.x <<- sub.data[,x.name]; subject.y <<- sub.data[,y.name];
        subject.id<<- sub.id; cur.col <<- cur.col; cur.lty <<- cur.lty
        if(!is.null(Fun)) do.call(Fun,NULL)
        subject.count <- subject.count + 1
      }   
    }
    remove(list=c("subject.x","subject.y","group.x","group.y"), pos=1);
    if (g.x.exists) group.x   <<- proc.plot.tmp...g.x
    if (g.y.exists) group.y   <<- proc.plot.tmp...g.y
    if (  x.exists) subject.x <<- proc.plot.tmp...x
    if (  y.exists) subject.y <<- proc.plot.tmp...y
    value <- TRUE
  }
    
