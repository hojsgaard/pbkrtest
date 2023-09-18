
f <- list.files("C:/work/Stat/Rdevel/doByDEVEL/doBy/R",pattern="\\.[R]",full.names=TRUE); 
fl<-f[(1:length(f))[-grep("~",f)]]


d=data.frame(cbind(x=0, aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa = 0,
bbbbbbbbbbbbbbbbbbbbbbbbbbbbb = 0, c = 0))

> d
  x aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbbbbbb c
1 0                              0                             0 0
> 
formula(d)
x ~ aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa + bbbbbbbbbbbbbbbbbbbbbbbbbbbbb +
    c
> 
sapply(fl,source) 

summaryBy(formula(d), d)
datavar:[1] "x"
"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "bbbbbbbbbbbbbbbbbbbbbbbbbbbbb"  "c"

numvar :[1] "x"
"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "bbbbbbbbbbbbbbbbbbbbbbbbbbbbb"  "c"

facvar :character(0)
idvar  :NULL
lhsstr: x
status:
rhsvar     : aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
bbbbbbbbbbbbbbbbbbbbbbbbbbbbb     c
idvar      :
x ~ aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa + bbbbbbbbbbbbbbbbbbbbbbbbbbbbb +
    c
Error in `[.data.frame`(data, , rhsvar, drop = FALSE) :
  undefined columns selected


This is due to rhsvar containing the indentation spaces in front of the
variable c:

Browse[1]> str(rhsvar)
 chr [1:3] "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
"bbbbbbbbbbbbbbbbbbbbbbbbbbbbb" "    c"










ff<- Weight+Feed~Evit+Cu+Time
          .xxx. <- ff[[2]]
          

unlist(strsplit(paste(deparse(.xxx.), collapse=""),".\\+."))





          unlist(paste(strsplit(deparse(.xxx.), collapse=""),".\\+."))












library(doBy)

f <- list.files("C:/work/Stat/Rdevel/doByDEVEL/doBy/R",pattern="\\.[R]",full.names=TRUE); 
fl<-f[(1:length(f))[-grep("~",f)]]


data(dietox)
ss<-splitBy(formula = ~Evit+Cu, data = dietox)

sapply(fl,source) 
a5<-summaryBy(Weight+Feed~Evit+Cu+Time, data=subset(dietox,Time>1),
         FUN=c(a = mean, b = var, c = length))

a5<-summaryBy(Weight+Feed~Evit+Cu+Time, data=subset(dietox,Time>1),
         FUN=c(a = mean, b = var, length))

a6 <- summaryBy(Weight+Feed~Evit+Cu+Time, data=subset(dietox,Time>1),
         FUN=function(x) c(a = mean(x), b = var(x), c = length(x)))


data(dietox)
sapply(fl,source) 
dietox<-subset(dietox, Time>9 & Evit %in% c(1,2)& Cu %in% c(1,2))

sapply(fl,source) 
a1<-summaryBy(log(Weight)+Feed~Evit+Cu+Time,      data=dietox, FUN=c(length, sum, mean, median))  

sapply(fl,source) 
FUN=function(x) {c(N=length(x),sum=sum(x),mean=mean(x),median=median(x))}  
a2<-summaryBy(log(Weight)+Feed~Evit+Cu+Time,      data=dietox, FUN=FUN)  

sapply(fl,source) 
FUNC=function(x) {c(length(x),sum(x),mean(x),median(x))}  
a3<-summaryBy(log(Weight)+Feed~Evit+Cu+Time,      data=dietox, FUN=FUNC)  






stattableA<-summaryBy(patcnt+mort+age~old_drg+new_drg+hai_status,

     data=TRACHdata,FUN=c(length,sum,mean,median)  )

 

stattableB<-summaryBy(patcnt+mort+adm_pdeath+los+total_charge+male+age~old_drg+new_drg+hai_status,

     data=TRACHdata,FUN=function(x) {c(N=length(x),sum=sum(x),mean=mean(x),median=median(x))}  )

 

stattableC<-mySummaryBy(patcnt+mort+adm_pdeath+los+total_charge+male+age~old_drg+new_drg+hai_status,

     data=TRACHdata,FUN=c(length,sum,mean,median)  )

 

stattableD<-mySummaryBy(patcnt+mort+adm_pdeath+los+total_charge+male+age~old_drg+new_drg+hai_status,

     data=TRACHdata,FUN=function(x) {c(N=length(x),sum=sum(x),mean=mean(x),median=median(x))}  )

