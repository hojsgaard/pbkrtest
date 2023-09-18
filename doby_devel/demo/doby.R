

data(dietox)
dietox$Cu     <- as.factor(dietox$Cu)
dietox$Evit   <- as.factor(dietox$Evit)

##  Plot of weight against time for each subjects within groups

par(mfrow=c(3,3),omi=c(0,0,.5,0))

plot.by(Time,Weight,subject=Pig,group=c(Evit,Cu),title="Evit*Cu=", data=dietox,group.pch=1:10, col=1:100)
mtext("Groupwise plots of weight against Time",outer=TRUE)

cat("\nType  <Return>    to continue : "); readline()

plot.by(Time,Weight,subject=Pig,group=c(Evit,Cu),title="Evit*Cu=",lines=T,group.col=1:30,pch=1:30, 
    data=dietox, silent=F)
mtext("Groupwise plots of weight against Time, subjects connected by lines",outer=TRUE)

cat("\nType  <Return>    to continue : "); readline()

group.f        <- function(){
    s<-lm(group.y~group.x+I(group.x^2)+I(group.x^3)); 
    lines(group.x[order(group.x)], s$fitted[order(group.x)],lwd=3,col=1)
    cat("Coefficients :", s$coef, "Mean :", mean(group.y), fill=T)
}
subject.f   <- function(){
    r<-spline(subject.x,subject.y);
    lines(r$x,r$y,col=3,lty="dashed")
}
plot.by(Time,Weight,subject=Pig,group=c(Evit,Cu),title="Evit*Cu =", col=1:30,lines=F, lwd=2, 
    group.fun="group.f",fun="subject.f",data=dietox )
mtext("Weight against Time with mean curve",outer=TRUE)

cat("\nType  <Return>    to continue : "); readline()

par(mfrow=c(1,1))
plot.by(Weight,subject=Pig,title="Plot of all pigs",lines=T,col=1:300, data=dietox)


## Grouped QQ-plots

cat("\nType  <Return>    to continue : "); readline()

dietox12    <- subset(dietox,dietox$Time==12)
par(mfrow=c(3,3),omi=c(0,0,.5,0))

qqnorm.by(Weight, group=c(Evit,Cu), data=dietox12)
mtext("QQ-plots for Weight in week 12",outer=T)



##
## Grouped summary statistics
##

cat("\nType  <Return>    to continue : "); readline()

data(dietox)
dietox12    <- subset(dietox,dietox$Time==12)

summary.1 <- summary.by(cbind(Weight,Feed)~Evit+Cu,      data=dietox12, FUN=list(mean,var))  
cat("Groupwise mean and variance at week 12\n")
print(summary.1)

cat("\nType  <Return>    to continue : "); readline()

summary.2 <- summary.by(cbind(Weight,Feed)~Evit+Cu+Time, data=dietox,   FUN=list(mean,var))  
cat("Groupwise mean and variance for each week\n")
print(summary.2)

##
## Power plots
##

cat("\nType  <Return>    to continue : "); readline()

data(dietox)
dietox12    <- subset(dietox,dietox$Time==12)
par(mfrow=c(2,1),omi=c(0,0,.5,0))
power.by(cbind(Weight,Feed)~Evit+Cu, data=dietox12)  
mtext("Groupwise plot of log variance against log mean",outer=T)


##
## Import and export CSV (comma separated) files
##

cat("\nType  <Return>    to continue : "); readline()
data(dietox)
exportcsv(dietox, file="mydietox.csv")
mydietox <- importcsv(file="mydietox.csv")
