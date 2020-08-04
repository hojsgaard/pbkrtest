



####
#### A much simpler version of summaryBy
####


# summaryBy <- function(ff, data, FUN=mean){

#   mf <- match.call(expand.dots = FALSE)
#   m <- match(c("formula", "data", "subset", "weights", "na.action", 
#                "offset"), names(mf), 0)
#   ff <- as.formula(mf[[2]])
  
  
#   d <- data
#   m <- match.call(expand.dots = TRUE)

#   if (class(m$ff[[2]])=="call"){  ## This way of checking for a formula is "dirty"
#     y.name <- paste(m$ff[[2]][-1])
#   } else {
#     y.name <- paste(m$ff[[2]])
#   }

#   yy <- eval(m$ff[[2]], d)
  
#   if (is.null(dim(yy)))
#     dim(yy) <- c(length(yy),1)


  
#   if (!is.list(FUN))
#     fun.names   <-     paste(deparse(substitute(FUN)))
#   else
#     fun.names   <-   unlist(lapply( substitute(FUN)[-1], function(a)paste(a)))
#   if (!is.list(FUN)) FUN <- list(FUN);
#   group <- attr(terms(ff),"term.labels")


#   resp <- NULL
#   if (length(ff[[2]])>1){
#       for (j in 2:length(ff[[2]])) 
#         resp <- c(resp, ff[[2]][[j]])
#   } else {
#      resp <- ff[[2]]
#   }
#   resp <- as.character(resp)
#   colnames(yy) <- resp
#   groupList <- NULL
#   s <- unlist( lapply(fun.names, paste, resp, sep="."))
#   extra <- which(is.na(match(colnames(yy), names(d))))
#   d <- cbind(d,yy[,extra,drop=FALSE])

#   vv<- by(d, d[,group],function(x){
#     xf <- x[1,group]
#     xr <- x[,resp,drop=FALSE]
#     v <- NULL
#     for (j in 1:length(FUN)){
#       FF <- FUN[[j]]
#       vf <- apply(xr,2,FF)  
#       v <- c(v,vf)
#     }
#     v <- c(xf,v)
#   }
#           )
 
#   val <- unlist(vv)
#   val <- as.data.frame(matrix(val, ncol=length(vv[[1]]),byrow=TRUE))
#   names(val) <- c(group,s)  

#   grpi <- match(group,names(d))
#   for (j in 1:length(group)){
#     if (is.factor(d[,group[j]])){
#         l <- levels(d[,grpi[j]])[ val[,group[j]] ]
#         val[,group[j]] <- as.factor(l)    
#     }
#   }
#   return(val)  
# }



# summaryBy <- function(formula, data=parent.frame(), FUN=mean){

#   mf <- match.call(expand.dots = FALSE)
#   m <- match(c("formula", "data", "subset", "weights", "na.action", 
#                "offset"), names(mf), 0)
#   ff <- as.formula(mf[[2]])
  
#   d <- data
#   m <- match.call(expand.dots = TRUE)

#   if (class(m$ff[[2]])=="call"){  ## This way of checking for a formula is "dirty"
#     y.name <- paste(m$ff[[2]][-1])
#   } else {
#     y.name <- paste(m$ff[[2]])
#   }

#   yy <- eval(m$formula[[2]], d)
  
#   if (is.null(dim(yy)))
#     dim(yy) <- c(length(yy),1)

  
#   if (!is.list(FUN))
#     fun.names   <-     paste(deparse(substitute(FUN)))
#   else
#     fun.names   <-   unlist(lapply( substitute(FUN)[-1], function(a)paste(a)))
#   if (!is.list(FUN)) FUN <- list(FUN);
#   group <- attr(terms(ff),"term.labels")


#   resp <- NULL
#   if (length(ff[[2]])>1){
#       for (j in 2:length(ff[[2]])) 
#         resp <- c(resp, ff[[2]][[j]])
#   } else {
#      resp <- ff[[2]]
#   }
#   resp <- as.character(resp)
#   colnames(yy) <- resp
#   groupList <- NULL
#   s <- unlist( lapply(fun.names, paste, resp, sep="."))
#   extra <- which(is.na(match(colnames(yy), names(d))))
#   d <- cbind(d,yy[,extra,drop=FALSE])

#   vv<- by(d, d[,group],function(x){
#     xf <- x[1,group]
#     xr <- x[,resp,drop=FALSE]
#     v <- NULL
#     for (j in 1:length(FUN)){
#       FF <- FUN[[j]]
#       vf <- apply(xr,2,FF)  
#       v <- c(v,vf)
#     }
#     v <- c(xf,v)
#   }
#           )
 
#   val <- unlist(vv)
#   val <- as.data.frame(matrix(val, ncol=length(vv[[1]]),byrow=TRUE))
#   names(val) <- c(group,s)  

#   grpi <- match(group,names(d))
#   for (j in 1:length(group)){
#     if (is.factor(d[,group[j]])){
#         l <- levels(d[,grpi[j]])[ val[,group[j]] ]
#         val[,group[j]] <- as.factor(l)    
#     }
#   }
#   return(val)  
# }





# plotBy2   <-
#   function(x, y=NULL,  subject=NULL, group=NULL, data = parent.frame(),
#            gFun=NULL, Fun=NULL, title="PLOT", lines=FALSE, same.axis=TRUE,
#            group.col=NULL, group.pch=NULL, group.lty=NULL, silent=TRUE
#            , xlim=range(x), ylim=range(y), ...
#            ){


#     sh.eprint <- function(x){
#       if (ep==TRUE)
#         {str <- deparse(substitute(x)); cat(paste("E>>", str," =", x),fill=TRUE)}}
#     ep        <- FALSE;
    
#     plot.table    <- NULL
#     subject.count <- 1
#     m <- match.call(expand.dots = TRUE)
#     if (is.matrix(eval(m$data, parent.frame()))) 
#       m$data <- as.data.frame(data)

#     #print(m$x)
#     #print(m$y)
#     #print(attr(m$x,"class"))
#     #print(class(m$x))
    
#     if (class(m$x)=="call"){  ## This way of checking for a formula is "dirty"
#       m$y <- x[[2]]
#       m$x <- x[[3]]
#     }

#     #print(m$x)
#     #print(m$y)
#                                         #print("iiiiiiiiiiii")    
#     lwd <- eval(m$lwd)
#     #lty <- eval(m$lty, data)
    
#     #pch <- eval(m$pch)
#     xlim<- eval(m$xlim)
#     ylim<- eval(m$ylim)
    
#     xlab<- m$xlab
#     ylab<- m$ylab
    
#     group.sym <- group.pch
#     only.x         <- FALSE
#     several.groups <- ifelse(is.null(by), FALSE, TRUE)
    
#     x.name <- deparse(m$x)
#     y.name <- deparse(m$y)
#     x <- eval(m$x, data)
#     y <- eval(m$y, data)


#     subject <- eval(m$subject, data)
     
#     if (is.null(m$group)){
#       by.names <- ".default.by.name"
#       by.data <- rep(".by.",length(x))
#       group.id.data <- by.data
#       several.groups <- FALSE
#     }
#     else{
#       if (length(m$group) > 1)
#         sssby <- m$group[2:length(m$group)]
#       else
#         sssby <- m$group
#       by.names <- paste(sssby);
#       by.data <- data[, by.names,FALSE]
#       for(j in 1:ncol(by.data))
#         by.data[,j] <- as.factor(by.data[,j])
#       mmm <- by.data[,by.names[1]];
#       if (length(by.names)>1)
#         for (i in 2:length(by.names))
#           mmm <- mmm:by.data[,by.names[i]]
#       mmm   <- ordered(mmm)
#       group.id.data <- mmm
#     }
#     group.name  <- paste(by.names,collapse="")


#     ## Subjects
#     if (is.null(m$sub)){
#       sub.names <- ".default.sub.name"
#       sub.group <- rep(".sub.",length(x))
#     }
#     else{
#       if (length(m$sub)>1)
#         ssssub <- m$sub[2:length(m$sub)]
#       else
#         ssssub <- m$sub      
#       sub.names <- paste(ssssub);
#       #print(sub.names)
#       sub.data <- data[, sub.names, drop=FALSE]
#       dd <- apply(sub.data,1,paste, collapse="*")
#       #print(dd)
#       #print(sub.data)
#     #  mmm <- sub.data[,sub.names[1]];
#     #  if (length(sub.names)>1)
#     #    for (i in 2:length(sub.names)){
#     #      #print(sub.data[,sub.names[i]])
#     #      mmm <- mmm:sub.data[,sub.names[i]]
#     #    }
#     #  mmm   <- ordered(mmm)
#     #  sub.group <- mmm
#       sub.group <- dd      
#     }
#     sub.group.name  <- paste(sub.names,collapse="*")

#     if ( is.null(y)) {
#       only.x  <- TRUE;
#       y  <- x;
#       x.index <- unlist(by (x, list(sub.group, group.id.data), function(a){1:length(a)}))
#       x  <- x.index;
#     }

#     new.data        <- data.frame(x, y, sub.group, group.id.data); ##print(data)
#     names(new.data) <- c(x.name, y.name, sub.group.name, group.name)

#     col <- eval(m$col, data);
#     if (is.list(col)){
#       col.name <- deparse(m$col[[2]]);
#       colCode <- col[[2]]
#       col <- col[[1]]
#     } else {
#       col.name <- deparse(m$col); 
#       colCode <- NULL
#     }
#     colF <- ifelse (!is.na(match(col.name,names(data))),TRUE,FALSE)


#     if (!colF){
#       if (is.null(col)) col <- 1
#       sub.id  <- as.numeric(factor(sub.group))
#       tmp.col <- rep(col,length(sub.id))
#       col     <- tmp.col[sub.id]
#     }
#     if (is.null(colCode))
#       new.data$colF <- col
#     else
#       new.data$colF <- colCode[col]








#     pch <- eval(m$pch, data);
#     if (is.list(pch)){
#       pch.name <- deparse(m$pch[[2]]);
#       pchCode <- pch[[2]]
#       pch <- pch[[1]]
#     } else {
#       pch.name <- deparse(m$pch); 
#       pchCode <- NULL
#     }
#     pchF <- ifelse (!is.na(match(pch.name,names(data))),TRUE,FALSE)

#     if (!pchF){
#       if (is.null(pch)) pch <- 1
#       sub.id  <- as.numeric(factor(sub.group))
#       tmp.pch <- rep(pch,length(sub.id))
#       pch     <- tmp.pch[sub.id]
#     }
#     if (is.null(pchCode))
#       new.data$pchF <- pch
#     else
#       new.data$pchF <- pchCode[pch]




#     sym       <- pch


#     lty <- eval(m$lty, data);
#     if (is.list(lty)){
#       lty.name <- deparse(m$lty[[2]]);
#       ltyCode <- lty[[2]]
#       lty <- lty[[1]]
#     } else {
#       lty.name <- deparse(m$lty); 
#       ltyCode <- NULL
#     }

#     ltyF <- ifelse (!is.na(match(lty.name,names(data))),TRUE,FALSE)
#     if (!ltyF){
#       if (is.null(lty)) lty <- 1      
#       sub.id  <- as.numeric(factor(sub.group))
#       tmp.lty <- rep(lty,length(sub.id))
#       lty     <- tmp.lty[sub.id]
#     }
#     if (is.null(ltyCode))
#       new.data$ltyF <- lty
#     else
#       new.data$ltyF <- ltyCode[lty]



    


    
    
#     xlabel <- if (only.x) "index" else x.name ;
#     ylabel <- if (only.x) x.name else y.name
    
#     if (!is.null(group.id.data[1])){   
#       group.data <-  by( new.data, group.id.data, function(x){x})
#     }
#     else{
#       group.data <-  by( data, rep(1, dim(data)[1]), function(x){x})
#     }
    
#     ## Avoid overwriting globals...    
#     g.x.exists  <- ifelse (exists("group.x"),   {proc.plot.tmp...g.x <- group.x;   TRUE}, FALSE)
#     g.y.exists  <- ifelse (exists("group.y"),   {proc.plot.tmp...g.y <- group.y;   TRUE}, FALSE)
#     x.exists    <- ifelse (exists("subject.x"), {proc.plot.tmp...x   <- subject.x; TRUE}, FALSE)
#     y.exists    <- ifelse (exists("subject.y"), {proc.plot.tmp...y   <- subject.y; TRUE}, FALSE)
    
#     ## Work through the groups
#     ## ###########################################
#     for (grp.id in 1:length(group.data)){       
#       curr.group    <-  group.data[[grp.id]];      ##print(curr.group)
#       curr.group.id <-  paste(curr.group[,group.name][1])
#       ## Group symbols
#       cur.sym <- ifelse(group.sym!=FALSE, 
#                         ifelse(group.sym !=FALSE,   group.sym,  group.sym[grp.id]),
#                         ifelse(lines==TRUE,    " ",    "o"))
      
#       ## Group colors
#       cur.col <-  ifelse(!is.null(group.col),
#                          ifelse(length(group.col)>1, group.col[grp.id], group.col), 1)
#       ## Line type
#       cur.lty <-  ifelse(!is.null(group.lty),
#                          ifelse(length(group.lty)>1, group.lty[grp.id], group.lty), 1)

#       cur.pch <-  ifelse(!is.null(group.pch),
#                          ifelse(length(group.pch)>1, group.pch[grp.id], group.pch), 1)

#       sub.list <- if (!is.null(sub.group.name)) {
#         by(curr.group, factor(curr.group[,sub.group.name]), function(d){d})
#       }
#       else{
#         list(curr.group)
#       }

#       ## Make empty plot with desired dimensions
#       xlab <- ifelse(!is.null(xlab), xlab, xlabel)
#       ylab <- ifelse(!is.null(ylab), ylab, ylabel)
#       xaxt <- ifelse(is.factor(x), "n", "s")

#       if (same.axis==TRUE){
#         local.x <- x
#         if (is.factor(x))
#           local.x <- as.numeric(x)
#         if (class(x)=="Date")
#           local.x <- x
#         plot(local.x, y, pch=" ", xlab=xlab, ylab=ylab, xaxt=xaxt, xlim=xlim, ylim=ylim)
#       } else {
#         plot(as.numeric(curr.group[,1]), curr.group[,2], pch=" ", xlab=xlab,
#              ylab=ylab, xaxt=xaxt, xlim=xlim, ylim=ylim) 
#       }
#       if (is.factor(x)) axis(1,1:length(levels(x)), levels(x))
      
#       ## Handle Titles
#       title.str <- ifelse(several.groups != FALSE, paste(title, curr.group.id,sep=''), title)
#       title( title.str )
      
#       group.x <<- curr.group[,x.name]; group.y <<- curr.group[,y.name];
#       group.id<<- grp.id; cur.col <<- cur.col; cur.lty <<- cur.lty
#       sh.eprint(group.args)
      
#       if(!is.null(gFun)) do.call(gFun,NULL)
      
#       ## Work through the subjects within groups
#       ## ###########################################
#       for (sub.id in 1:length(sub.list)){
#         sub.data   <- sub.list[[sub.id]]; ##print(sub.data)
#         sub.id.str <- paste(sub.data[,sub.group.name][1]); 
#         if (only.x == TRUE)
#           sub.data[,x.name] <- 1:length(sub.data[,y.name]);
        
#         newcol  <- ifelse (!is.null(sub.data$colF[1]), sub.data$colF[1],col[sub.id])
#         cur.col <- ifelse(!is.null(group.col), cur.col,
#                           ifelse(!is.null(col),
#                                  ifelse(length(col)==1, col, newcol),  1))[1]
        
#         newlty  <- ifelse (!is.null(sub.data$ltyF[1]), sub.data$ltyF[1],lty[sub.id])
#         cur.lty <- ifelse(!is.null(group.lty), cur.lty,
#                           ifelse(!is.null(lty),
#                                  ifelse(length(lty)==1, lty, newlty),  1))[1]

#         newpch  <- ifelse (!is.null(sub.data$pchF[1]), sub.data$pchF[1],pch[sub.id])
#         cur.pch <- ifelse(!is.null(group.pch), cur.pch,
#                           ifelse(!is.null(pch),
#                                  ifelse(length(pch)==1, pch, newpch),  1))[1]
        
#         #cur.sym <- ifelse(!is.null(group.sym),
#         #                  ifelse(length(group.sym)==1, group.sym, group.sym[grp.id]),
#         #                  ifelse(!is.null(sym), ifelse(length(sym)==1, sym, sym[sub.id]),
#         #                         1))[1]
        
#         plot.table <- rbind(plot.table,
#                             c(cur.pch, cur.col, curr.group.id, sub.id.str, cur.lty,))
#         points(sub.data[,x.name], sub.data[,y.name], pch=cur.pch, col=cur.col,lwd=lwd)
        
#         if (lines == TRUE){
#           lines (sub.data[,x.name], sub.data[,y.name], col=cur.col,lwd=lwd,lty=cur.lty)  
#         }
#         subject.x <<- sub.data[,x.name]; subject.y <<- sub.data[,y.name];
#         subject.id<<- sub.id; cur.col <<- cur.col; cur.lty <<- cur.lty

#         #if(!is.null(Fun)) do.call(Fun,NULL)
        
#         if(!is.null(Fun)) Fun()

#         subject.count <- subject.count + 1
#       }   
#     }
#     remove(list=c("subject.x","subject.y","group.x","group.y"), pos=1);
#     if (g.x.exists) group.x   <<- proc.plot.tmp...g.x
#     if (g.y.exists) group.y   <<- proc.plot.tmp...g.y
#     if (  x.exists) subject.x <<- proc.plot.tmp...x
#     if (  y.exists) subject.y <<- proc.plot.tmp...y
#     value <- as.data.frame(plot.table)
#     names(value) <- c("symbol","colour","group","subject","line")
#     if (silent==FALSE)
#       print(value)
#     return(invisible(value))
#   }

