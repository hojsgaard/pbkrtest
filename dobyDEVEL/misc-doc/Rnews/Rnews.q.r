summaryBy(Ozone+Wind~Month, data=airquality,FUN=c(mean,var))

summaryBy(Ozone+log(Wind)~Month, data=airquality,FUN=c(mean,var))

source("D:/Stat/Rdevel/doByDEVEL/doBy/R/summaryBy.R")
summaryBy(Ozone~Month, data=airquality,FUN=c(mean,var),na.rm=T)


eval(I(Ozone+Wind), data=airquality)








splitBy(~Month, data=airquality)

sampleBy(~Month, frac=1, data=airquality,replace=T)

sampleBy(~1, frac=1, data=airquality)

subsetBy(~Month, data=airquality)

subsetBy(~Month, subset='Wind>mean(Wind)', data=airquality)

summaryBy(Wind~Month, data=airquality,FUN=c(mean))

  Month mean.Wind
1     5 11.622581
2     6 10.266667
3     7  8.941935
4     8  8.793548
5     9 10.180000



a<-by(airquality, airquality$Month, function(d){
  c(mean(d[,c("Ozone","Wind")],na.rm=T), diag(var(d[,c("Ozone","Wind")],na.rm=T)))})
do.call("rbind",a)
