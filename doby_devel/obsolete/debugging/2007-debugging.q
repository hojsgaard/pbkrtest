library(doBy)

f <- list.files("C:/work/Stat/Rdevel/doByDEVEL/doBy/R",pattern="\\.[R]",full.names=TRUE); 
fl<-f[(1:length(f))[-grep("~",f)]]

data(dietox)
sapply(fl,source) 

dietox<-subset(dietox, Time>9 & Evit %in% c(1,2)& Cu %in% c(1,2))
sapply(fl,source) 
a2<-summaryBy(log(Weight)+Feed~Evit+Cu+Time,      data=dietox, FUN=c(mean), keep=F)  

a2<-.summaryByOLD(log(Weight)+Feed~Evit+Cu+Time,      data=dietox, FUN=c(mean), keep=F)  

lhsvar <- c("log(uptake)","conc")
z<-do.call(cbind,lapply(paste(lhsvar), function(x)eval(parse(text=x), CO2)))
colnames(z)<-lhsvar






system.time(for (ii in 1:100){
a1<-summaryBy(Weight+Feed~Evit+Cu+Time,      data=dietox, FUN=c(mean,var), use='pair')  
})

system.time(for (ii in 1:100){
a2<-summaryBy2(Weight+Feed~Evit+Cu+Time,      data=dietox, FUN=c(mean,var), use='pair')  
})

sapply(fl,source) 
a2<-summaryBy2(Weight+Feed~Evit+Cu+Time,      data=dietox, FUN=c(mean,var), use='pair')  






sapply(fl,source) 
a3<-summaryDF(dietox, c("Evit","Cu","Time"), operate=c("Weight","Feed"), functions=c("mean"))
summaryDF(dietox, c("Evit","Cu","Time"), operate=c("Weight"), functions=c("var"))



summaryDF = function(data, splits, operate.on.columns, functions = "mean",...){ 
  ## function to create a summary data frame
  ##  data is the original data to use
  ##  splits = column names which describe the rows to combine
  ##  operate.on.columns = column names which give the columns to summarize
  ##  functions = the function to apply to the elements of one column
  ##     having the same splits.



rhsfact <- apply(rhs, 1, paste, collapse="|")
unrhs   <- unique(rhsfact)









sapply(fl,source) 
x <- c(rep(1,5),rep(2,3),rep(3,7),rep(1,4))


sapply(fl,source) 
x<-c(1,1,2,2,2,1,1,1)

firstobs(x)
lastobs(x)




sapply(fl,source) 
firstobs(~Pig, data=dietox)
lastobs(~Pig, data=dietox)














dat.fac <- data.frame(fac1 = as.factor(sample(1:5, 10, replace = TRUE)),
                      fac2 = as.factor(sample(1:10, 10, replace = TRUE)),
                      fac3 = as.factor(sample(c(1,3,6,8), 10, replace = TRUE)))

orderBy(~fac1+fac2, data=dat.fac)




summaryBy(act3~dag_fra_klv, data=wd)




library(doBy)
x <- as.data.frame(matrix(ncol=3,seq(1,12),dimnames=list(c(),c("hh","total","total.inf"))))
x
summaryBy(total+total.inf~hh,x,FUN=sum)

What surprises me are the zeros in the resulting total.sum column. 
The problem remains if total.inf is renamed to totalinf or total_inf but not if renamed to ttotal.inf .


   library(doBy);
   s <- c(1,1);
   n <- 10;
   b <- as.data.frame(rbind(rep(0,times=n),rep(1,times=n)));
   frm <- cbind(something=s,b)

sapply(fl,source) 
   summaryBy(something ~ ., data=frm, FUN=length)

   data(warpbreaks)
   summaryBy(breaks ~., data=warpbreaks, FUN=length) 





>     # Error in parse(file, n, text, prompt, srcfile, encoding) : 
>     #    syntax error, unexpected END_OF_INPUT in "something ~  "
> 



load("reproplus.Rdata")

use <- unique(reproplus$cowidp)## [1:10]
wdata <- subset(reproplus, cowidp %in% use)

# produces a list of data sets one for each cowidp
# attr(wdatal,"groupid") shows the list of cowidps

wdata1 <- splitBy(~dyrnr+i.parity, wdata)
sapply(fl,source) 
wdata1 <-splitByp(~dyrnr+i.parity, wdata)

wdata1 <-splitByp(~cowidp, wdata)


res <- as.list(rep(NA, wdata1$length))
for (iii in 1:wdata1$length){
  argh <- select(wdata1,iii)
  ##print(head(argh))
  res[[iii]] <-argh
}


rbindlist <- function(res){
  val <- NULL
  repeat{
  val <- rbind(val, res[[1]])
  res[[1]] <- NULL
  if (length(res)==0)
    break()
  }
  return(val)
}


z <- sapply(ls(), function(x)
            object.size(get(x)))


as.matrix(rev(sort(z))[1:10])













unid  <- unique(wdata$cowidp)
res   <- as.list(rep(NA, length(unid)))

for (i in 1:length(unid)){
  print(i)
  res[[i]] <- subset(wdata, cowidp==unid[i])
}










mah<-read.csv("mah.csv")

sumfun <- function(x, ...){ c(m=mean(x, ...), v=var(x, ...), l=length(x)) } 

sapply(fl,source) 
sumfun <- function(x, ...){ c(m=mean(x, ...), l=length(x)) } 
mah <- mah[mah$Ocount>6,]
mah$Ocount <- factor(mah$Ocount)
sapply(fl,source) 
summaryBy(pvalue~Ocount, data=mah, FUN = sumfun, na.rm=TRUE)
warnings()


sapply(fl,source) 
a<-splitBy(~Ocount, data=mah)


sapply(fl,source) 
lapply(a, .matrix2dataFrame2, at=at,restoreAll=FALSE)

sapply(fl,source) 
v<-.matrix2dataFrame2(a[[1]], at=at,restoreAll=FALSE)


m <- .asNumericMatrix2(mah)

sapply(fl,source) 

x  <- .asNumericMatrix2(CO2)
at <- .subsAttr2(CO2)
x2 <- .matrix2dataFrame2(x,at)

for (i in 1:ncol(x2)){
  print(identical(x2[,i],CO2[,i]))
}




y <- mah[,1]

data(CO2)

CO2[,2] <- as.character(CO2[,2])
CO2$day <- Sys.Date()

at<-lapply(CO2, function(y){
  print("--------------")
  sm  <- storage.mode(y)
  if (sm=='character')
    a   <- attributes(factor(y))
  else
    a   <- attributes(y)
  v <- list(sm=sm,atr=a)
  v
})
#attr(at,"dataclass") <- class(CO2)

x <- .asNumericMatrix2(CO2)

  d <- dimnames(x)
  k <- length(d[[2]])
  w <- vector("list", k)
  names(w) <- d[[2]]

di  <- CO2[,i]

for (i in 1:k) {
  a   <- at[[i]]
  xi  <- x[,i]
  if (a$sm=='character'){
    storage.mode(xi) <- 'integer'
    attributes(xi) <- a$atr
    xi  <- as.character(xi)
  } else {
    storage.mode(xi) <- a$sm
    attributes(xi)   <- a$atr
  }
  w[[i]] <- xi
}

x2<-structure(w, class = 'data.frame', row.names = d[[1]])







  




i <- 7
identical(x2[,i], CO2[,i])



data(dietox)

## Calculate weekwise feed efficiency = weight gain / feed intake
dietox <- orderBy(~Pig+Time, data=dietox)
v<-lapplyBy(~Pig, data=dietox, function(d) c(NA, diff(d$Weight)/diff(d$Feed)))
dietox$FE <- unlist(v)

## Technically this is the same as 
dietox <- orderBy(~Pig+Time, data=dietox)
wdata <- splitBy(~Pig, data=dietox)
v <- lapply(wdata, function(d) c(NA, diff(d$Weight)/diff(d$Feed)))
dietox$FE <- unlist(v)


v<-lapplyBy(~Pig, data=dietox, function(d) c(NA, diff(d$Weight)/diff(d$Feed)))































En kørsel giver 
 
> test1<-read.csv('test.csv',sep=";")
> test1
   herd model ver run      logL rank
13    2     4   8   1 -3880.395   15
1     2     4   7   1 -3880.130   15
39    2    11   9   1 -3839.710    8
40    2    11   9   2 -3839.694    8
17    2     8   8   1 -3824.993   14
34    2     8   9   2 -3824.952   14
> summaryBy(logL~herd+model,data=test1,id=~rank+ver+run,FUN=c(max),keep.names=T)
  herd model      logL rank ver run
1    2     4 -3880.130   15   8   1
2    2     8 -3824.952   14   8   1
3    2    11 -3839.694    8   9   1

Max rækken for herd 2 og model 4 er rækkenavn 1 med ver=7, men i summaryBy resultatet er ver=8 så den tager vel ikke den rigtige ver værdi med?

Med venlig hilsen / Regards
Lars Relund Nielsen 
