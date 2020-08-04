library(doBy)

flist <- list.files("doBy/R",pattern=glob2rx("*.R"),full.names=TRUE); 


sapply(flist,source) 

ddd <- data.frame(aaa=rep(letters[1:5],2),bbb=rep(c("A","B"),each=5),y=1:5,z=1:5)
ddd <- ddd[-3,]
ddd <- rbind(ddd,ddd)

sapply(flist,source) 
uu<-splitBy(~aaa+bbb, data=ddd)

.splitMatrix <- function(dataMatrix, grps){
  idx <- 1:nrow(dataMatrix)
  ggg <- split(idx, grps)
  lapply(ggg, function(iii) dataMatrix[iii,,drop=FALSE])
}

orderBy(~aaa, data=ddd)



library(lme4)

flist <- list.files("doBy/R",pattern=glob2rx("*.R"),full.names=TRUE); 

fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
obj <- fm1
esticon(fm1, c(1,1))

sapply(flist,source) 
esticon(fm1, c(1,1))










flist <- list.files("doBy/R",pattern=glob2rx("*.R"),full.names=TRUE); 


sapply(flist,source) 
esticon(fm1, c(1,1))

(m <- lmer(logyd~periode+(1|CKRDYRNR), data=OffFeedRed))
esticon(m,c(1,0,0,0))






xx = data.frame(x = c("b", "a"), y = c(0, 1, 0, 0))
xx
zz<-summaryBy(y ~ x, xx, FUN = c(min, max))



#xx = data.frame(x = c("a", "b"), y = c(0, 1, 0, 0))




var1 <- c(1,2,3,4,5,6)
var2 <- c(1,1,1,1,2,2)
var3 <- c(1,1,1,2,2,2)
x <- data.frame(cbind(var1,var2,var3))


sapply(flist,source) 

summaryBy(var1 ~ var2, data=x, FUN=mean)
summaryBy(var1 ~ var2, data=x, FUN=mean, id=~var3)



sapply(flist,source) 

sapply(flist,source) 
summaryBy(log(uptake) ~ 1, data=CO2,  FUN=myfun1)




summaryBy(var1 ~ var2, data=x, FUN=max, id=~var3)


summaryBy(var1 ~ var2, data=x, FUN=max)


yy <- c(1,10,-1,10)
zz <- c("b","a","b","a")

tapply(yy,zz,sum)



