library(doBy)

flist <- list.files("doBy/R",pattern=glob2rx("*.R"),full.names=TRUE);
sapply(flist,source)

yvar <- c(0,0,0,1,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0)
tvar <- seq_along(yvar) + 10

cs <- cumsum(yvar)
un <- unique(cs)
tlist <- list()
for (ii in 1:length(un)){
	vv <- un[ii]
	yy <- yvar[cs==vv]
	tt <- tvar[cs==vv]
	tt <- tt - tt[1]
	tlist[[ii]] <- tt
}
tsince <- unlist(tlist)
tsince[cs==0] <- NA


yvar2 <- rev(yvar)
tvar2 <- rev(tvar)

cs2 <- cumsum(yvar2)
un2 <- unique(cs2)
tlist2 <- list()
for (ii in 1:length(un2)){
	vv <- un2[ii]
	yy <- yvar2[cs2==vv]
	tt <- tvar2[cs2==vv]
	tt <- tt - tt[1]
	tlist2[[ii]] <- tt
}
tsince2 <- unlist(tlist2)
tsince2[cs2==0] <- NA

tsince2 <- rev(tsince2)
cs[cs==0]<-NA

cbind(yvar,tvar, cs, tsince, tsince2)

sapply(flist,source)
timeSinceEvent(yvar,tvar)

timeSinceEvent(c(0,0,0))
timeSinceEvent(c(0,0,0),c(1,2,NA))











sss <- subSeq()

cumsum(yvar)
unlist(lapply(sss$slength, function(ll) 1:ll))

