g <- function(x){c(mean=mean(x,na.rm=T),var=var(x,na.rm=T))}
summarize(Ozone, Month,  FUN=g)
  
g <- function(x)c(Mean=mean(x,na.rm=TRUE),Median=median(x,na.rm=TRUE))
summarize(temperature, month, g)

source("D:/Stat/Rdevel/doByDEVEL/doBy/R/splitBy.R")
source("D:/Stat/Rdevel/doByDEVEL/doBy/R/summaryBy.R")
source("D:/Stat/Rdevel/doByDEVEL/doBy/R/esticon.R")


summaryBy(Ozone+Wind~Month, data=airquality,FUN=c(mean,var),na.rm=TRUE)

summaryBy(Ozone+Wind~Month, data=airquality,FUN=c(mean,var),na.rm=TRUE,
  prefix=c("m","v"))
  
summaryBy(Ozone+Wind~Month, data=airquality,FUN=c(mean,var),na.rm=TRUE,
  keep.names=F)
  prefix=c("m"))

summaryBy(Ozone+Wind~Month, data=airquality,FUN=c(mean),na.rm=TRUE,
  keep.names=T)


summaryBy(Ozone+Wind~Month, data=airquality,FUN=c(mean,var),na.rm=TRUE)


airquality$Month <- factor(airquality$Month)
xyplot(Ozone~Wind | Month, data=airquality, type=c('l','smooth'))

xyplot(Ozone~Day, group=Month, data=airquality, type=c('l','smooth'))


pairs(airquality, panel = panel.smooth, main = "airquality data")

m<-lm(Ozone~Month+Wind+Month*Wind, data=airquality)

esticon(m, c(0,1,-1,0,0,0,10,-10,0,0))







