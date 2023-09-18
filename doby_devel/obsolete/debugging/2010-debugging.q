library(doBy)

flist <- list.files("doBy/R",pattern=glob2rx("*.R"),full.names=TRUE);
sapply(flist,source)

ss<-summaryBy(Ozone+Wind~Month, data=airquality,FUN=c(mean),na.rm=TRUE,
  keep.names=TRUE)


attach(warpbreaks)










x <- c(1,1,1,0,0,1,1,1,2,2,2,1,2,2,2,3)
(ans <- subSeq(x))
ans$value
subSeq(x,item=1)
subSeq(x,item="1")

sapply(flist,source)
x <- as.character(c(1,1,1,0,0,1,1,1,2,2,2,1,2,2,2,3))
(ans<-subSeq(x))
ans$value
subSeq(x,item="1")
subSeq(x,item=1)

x <- factor(c(1,1,1,0,0,1,1,1,2,2,2,1,2,2,2,3))
(ans<-subSeq(x))
ans$value
subSeq(x,item=1)
subSeq(x,item="1")


identical(c(1,1,1),c("1","1","1"))






 x <- c("dec","jan","feb","mar","apr","may")
 src1 <- list(c("dec","jan","feb"), c("mar","apr","may"))
 tgt1 <- list("winter","spring")
 recodeVar(x,src=src1,tgt=tgt1)


x<-c(rep(1:3,3),4,4)
x<-c(rep(1:3,3),4,4,NA)
src = list(c(1,2))
tgt = list('A')
default=NULL
keep.na=F

x <- factor(x)
  if (length(src)!=length(tgt)){
    stop("length of src not equal to length of tgt")
  }
  mtc <- lapply(src, function(zzz){which(x %in% zzz)})
  idx <- seq_along(x)
  unmatch <- setdiff(idx, unlist(mtc))

  val <- x#rep(NA,length(x))

  for (ii in 1:length(tgt))
    val[mtc[[ii]]] <- tgt[[ii]]

  if (!is.null(default)){
    if (keep.na){
      iii <- intersect(which(!is.na(x)), unmatch)
      val[iii] <- default
    } else {
      val[unmatch] <- default
    }
  }
  
  if (is.factor(x))
    val <- as.factor(val)
  val




recodeVar(x, src, tgt)



