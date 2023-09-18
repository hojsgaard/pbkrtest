### chunk number 1: 
oopt <- options()
options("digits"=4)
options("width"=40)


### chunk number 2: 
head(airquality)
#names(airquality) <- c("Oze","Sor","Wnd", "Tmp","Mth", "Day")


### chunk number 3: 
summaryBy(Ozone+Wind~Month, data=airquality,FUN=c(mean,var),
          prefix=c("m","v"),  na.rm=TRUE)


### chunk number 4: 
a<-by(airquality, airquality$Month, function(d){
  c(mean(d[,c("Ozone","Wind")],na.rm=T), diag(var(d[,c("Ozone","Wind")],na.rm=T)))})
do.call("rbind",a)


### chunk number 5: 
x<-orderBy(~Temp+Month, data=airquality,decreasing=T)


### chunk number 6: 
head(x)


### chunk number 7: 
x<-splitBy(~Month, data=airquality)


### chunk number 8: 
attr(x,"groupid")


### chunk number 9: 
sampleBy(~1, frac=0.5, data=airquality)


### chunk number 10: 
sampleBy(~Month, frac=0.2, data=airquality,systematic=T)


### chunk number 11: 
subsetBy(~Month, subset='Wind>mean(Wind)', data=airquality)


### chunk number 12: 
airquality <- transform(airquality, Month=factor(Month))
m<-lm(Ozone~Month*Wind, data=airquality)
coefficients(m)


### chunk number 13: 
Lambda <- rbind(
  c(0,-1,0,0,0,0,-10,0,0,0),
  c(0,1,-1,0,0,0,10,-10,0,0),
  c(0,0,1,-1,0,0,0,10,-10,0),
  c(0,0,0,1,-1,0,0,0,10,-10)
  )
esticon(m, Lambda
)


### chunk number 14: 
Lambda <- rbind(
  c(0,0,0,0,0,0,1,0,0,0),
  c(0,0,0,0,0,0,0,1,0,0),
  c(0,0,0,0,0,0,0,0,1,0),
  c(0,0,0,0,0,0,0,0,0,1)
  )
esticon(m, Lambda, joint.test=T)


### chunk number 15: 
options(oopt)


