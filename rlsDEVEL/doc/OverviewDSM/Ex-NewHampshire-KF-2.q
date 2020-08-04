data(nhtemp)
temp <- as.numeric(nhtemp)
year <- 1912:1971
time <- 1:60
nht <- as.data.frame(cbind(temp,year,time))

par(mfrow=c(1,2))
plot(temp~year, data=nht)
abline(lm(temp~year, data=nht),col="red")
lines(smooth.spline(nht$temp~nht$year),col="blue")

yt     <- nht$temp
time   <- nht$time


## Simulate some data
timeBase <- 1:100
a <- 50; b <- .1
mu <- a + b*timeBase + cos((6.28/50)*(timeBase-1))
plot(mu,type='l')
yt <- mu + rnorm(length(mu), sd=0.1)
plot(yt,type='l')

ntime  <- length(yt)
s2W <- 0.00001
s2C <- 0.0001
s2V <- 1e0

m0  <- c(50,0)
C0  <- diag(c(100000*s2C,s2C))
Wt  <- diag(c(100000*s2W,s2W))
Gt  <- diag(1,2)

par(mfrow=c(1,2))
ft.vec <- et.vec <- ft5.vec <- lambdat.vec <- rep(NA, ntime)
mt.mat <- matrix(NA,nrow=2,ncol=ntime)
work <- ft.vec

time   <- timeBase
mt <- m0; Ct <- C0
for (i in 1:ntime){
  Ft <- c(1, time[i]); ## print(Ft)
  at <- Gt %*% mt
  Rt <- Gt %*% Ct %*% t(Gt) + Wt
  ft <- t(Ft) %*% at
  Qt <- t(Ft) %*% Rt %*% Ft + s2V
  mt <- at + Rt %*% Ft %*% solve(Qt) %*% (yt[i]-ft)
  Ct <- Rt - Rt %*% Ft %*% solve(Qt) %*% t(Ft) %*% t(Rt)
  print(cbind(at,Ct))
  mt.mat[,i] <- mt
  ft.vec[i] <- ft
  work[i] <- Ct[2,2]
  lambdat.vec[i] <- t(Ft) %*% mt
  et.vec[i] <- yt[i]-ft
}
# End of the simple Kalman filter

# Mean squared prediction errors
mean((yt-ft.vec)^2,na.rm=T)

# Plot the results
plot(time,yt,col='red',xlim=range(c(0,time)))
lines(time, lambdat.vec,col='blue',lty=2)
abline(lm(yt~time),col='black',lwd=2)

abline(h=m0[1], lwd=2)
rb <- rainbow(6)
abline(mt.mat[,10],col=rb[1])
abline(mt.mat[,20],col=rb[2])
abline(mt.mat[,30],col=rb[3])
abline(mt.mat[,40],col=rb[4])
abline(mt.mat[,50],col=rb[5])
abline(mt.mat[,60],col=rb[6])
abline(v=seq(10,60,10))

plot(mt.mat[2,], main='Slope',type='l')

mt.mat[,seq(10,60,10)]


tt <- time+10
summary(lm(yt~tt+I(tt^2)))



