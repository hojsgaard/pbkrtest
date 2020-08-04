library(doBy)

flist <- list.files("doBy/R",pattern=glob2rx("*.R"),full.names=TRUE);
sapply(flist,source)

airquality <- subset(airquality, Month %in% c(5,6))

subsetBy(~Month, subset=Wind > mean(Wind), data=airquality, join=F)
sapply(flist,source)
subsetBy2(~Month, x=airquality, subset=Wind > mean(Wind),join=F)

xlst <- splitBy(~Month, data=airquality)

lapply(xlst, function(xx){eval(cl2, envir=list('_x'=xx)) }

substitute( cl2, list('_x'=airquality))



yvar <- c(0,0,1,0,0,0,0,0,1,0,0,0,1,1,0,0,0)
(tvar <- seq_along(yvar) + c(0.1,0.2))

yvar <- c(0,0,0,1,0,0,1,0,0,0,0,0,1,1,0,1,0,0)
yvar <- c(0,1,0,1,0)
yvar <- c(1,1,1,1)
yvar <- c(1,0,0,0)

yvar <- c(0,0,1,0,0,0)

tvar <- seq_along(yvar)

sapply(flist,source)
tse <- timeSinceEvent(yvar,tvar)

tse2<- TSE(yvar,tvar)


tse$sign.tse


event.idx <- which(yvar==1)

## get time difference to each event 
rrr <-  do.call(rbind, lapply(event.idx, function(ii) tvar-tvar[ii]))
abs.tse <- apply(abs(rrr),2,min)

## get the event windows
ff<-tvar[event.idx[1:(length(event.idx)-1)]]+diff(tvar[event.idx])/2
eee<-rep.int(NA, length(yvar))
eee[tvar<=ff[1]] <- 1
for (ii in 2:(length(ff)-0)){
   eee[tvar>ff[ii-1] & tvar<=ff[ii] ] <- ii
}
eee[tvar>ff[length(ff)]] <- length(ff)+1

## get the signs
ggg <- list()
for (ii in 1:(length(event.idx))){
	ggg[[ii]] <- rrr[ii,eee==ii]
}
ggg<-unlist(ggg)
sign.tse <- sign(ggg)*abs.tse

  run <- cumsum(yvar)
#  run[run==0]<-NA
  un <- unique(run)
  tlist <- list()
  for (ii in 1:length(un)){
    vv <- un[ii]
    yy <- yvar[run==vv]
    tt <- tvar[run==vv]
    tt <- tt - tt[1]
    tlist[[ii]] <- tt
  }
  timeAfterEvent <- unlist(tlist)
  timeAfterEvent[run==0] <- NA


cbind(yvar, tvar, abs.tse, sign.tse,eee)





rbind(yvar,tvar,eee)

	
  abs.tse  <-apply(abs(rrr),2,min)
  print(abs.tse)
	
  
  



x <- c(1,1,2,2,2,1,1,3,3,3,3,1,1,1)
subSeq(x)
subSeq(x, item=1)
subSeq(letters[x])
subSeq(letters[x],item="a")


1,1,1,2,2,2,1,2,2,2,3)
(ans <- subSeq(x))
ans$value
# Notice: Same results below
subSeq(x,item=1)
subSeq(x,item="1")

x <- as.character(c(1,1,1,0,0,1,1,1,2,2,2,1,2,2,2,3))
(ans<-subSeq(x))
ans$value
# Notice: Same results below
subSeq(x,item="1")
subSeq(x,item=1)

x <- factor(c(1,1,1,0,0,1,1,1,2,2,2,1,2,2,2,3))
(ans<-subSeq(x))
ans$value
# Notice: Same results below
subSeq(x,item=1)
subSeq(x,item="1")
