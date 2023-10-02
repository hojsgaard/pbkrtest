#####################################################################
#
# A very simple implementation of a Kalman filter for a dynamic
# linear model.
#
# The response y.t is temperaure in New Hampshire, USA in year t
# which ranges from 1912 to 1971.
#
#####################################################################

#
# Organize data
#

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
ntime  <- length(yt)
time   <- nht$time

####################################################################
#
# A very simple Kalman filter is then implemented as follows where
#   mt.vec  : parameter estimates to each time point
#   ft.vec  : 1-step forward predictions
#   et.vec  : 1-step forward prediction error
#   ft5.vec : 5-step forward predictions
#
####################################################################

s2W <- 0.0001
s2C <- 0.0001
s2V <- 1e0

m0  <- c(50,0)
C0  <- diag(s2C,2)
Wt  <- diag(s2W,2)
Gt  <- diag(1,2)

ft.vec <- et.vec <- ft5.vec <- lambdat.vec <- rep(NA, ntime)
mt.mat <- matrix(NA,nrow=2,ncol=ntime+1)

mt <- m0; Ct <- C0
for (i in 1:ntime){
  Ft <- c(1, time[i])
  at <- Gt %*% mt
  Rt <- Gt %*% Ct %*% t(Gt) + Wt
  ft <- t(Ft) %*% at
  Qt <- t(Ft) %*% Rt %*% Ft + s2V
  mt <- at + Rt %*% Ft %*% solve(Qt) %*% (yt[i]-ft)
  Ct <-  Rt - Rt %*% Ft %*% solve(Qt) %*% t(Ft) %*% t(Rt)
  mt.mat[,i] <- mt
  ft.vec[i] <- ft
  lambdat.vec[i] <- t(Ft) %*% mt
  if (i <= ntime-5)
    ft5.vec[i+5] <- c(1, time[i]) %*% at
  et.vec[i] <- yt[i]-ft
}
# End of the simple Kalman filter

# Mean squared prediction errors
mean((yt-ft.vec)^2,na.rm=T)
mean((yt-ft5.vec)^2,na.rm=T)

# Plot the results
par(mfrow=c(1,1))
plot(time,yt,col='red')
lines(time, ft.vec, col='blue',lty=1)
lines(time, ft5.vec,col='blue',lty=2)
abline(lm(yt~time),col='black')

par(mfrow=c(1,1))
plot(time,yt,col='red',main='lambdat')
lines(time, lambdat.vec, col='blue',lty=1)

save(ft.vec,lambdat.vec,file='simple')


####################################################################
#
# Wrap the filter into a function, and calculate also the value
# of the log-likelihood function
#
####################################################################

plot(time, yt,main=paste("s2V:", round(s2V,3), "s2W:", round(s2W,3),
"s2C:", round(s2C,3)), col='blue')
lines(ft.vec, col='red')

ft.vec <- et.vec <- rep(NA, ntime)
mt.mat <- matrix(NA,nrow=2,ncol=ntime+1)
mt.mat[,1]<-c(50,0.00)

s2W <- 0.00001
s2C <- 0.01
s2V <- 1e3

kalman1 <- function(s2V,s2W){
  Wt <- diag(s2W,2)
  Ct <- C0 <- diag(s2C,2)
  logL <- 0
  for (i in 1:ntime){
    Ft <- c(1, time[i])
    at <- Gt %*% mt.mat[,i]
    Rt <- Gt %*% Ct %*% t(Gt) + Wt
    ft <- t(Ft) %*% at
    Qt <- t(Ft) %*% Rt %*% Ft + s2V
    mt <- at + Rt %*% Ft %*% solve(Qt) %*% (yt[i]-ft)
    Ct <- Rt - Rt %*% Ft %*% solve(Qt) %*% t(Ft) %*% t(Rt)
    mt.mat[,i+1] <- mt
    ft.vec[i] <- ft
    et.vec[i] <- yt[i]-ft
    logL <- logL - log(det(Qt)) - t(yt[i]-ft) %*% solve(Qt) %*% (yt[i]-ft)
  }
  str <- sprintf("s2V: %.1f s2W: %.5f \n s2C: %.5f logL: %.2f", s2V, s2W, s2C, logL)
  plot(yt,main=str)
  lines(ft.vec)
  return(logL)
}

# Compare plots for different variance parameters

par(mfrow=c(3,3))
s2C <- 1e-4;
kalman1(s2V=1e4,s2W=1e-4)
kalman1(s2V=1e4,s2W=1e-2)
kalman1(s2V=1e4,s2W=1e-0)

kalman1(s2V=1e2,s2W=1e-4)
kalman1(s2V=1e2,s2W=1e-2)
kalman1(s2V=1e2,s2W=1e-0)

kalman1(s2V=1e0,s2W=1e-4)
kalman1(s2V=1e0,s2W=1e-2)
kalman1(s2V=1e0,s2W=1e-0)
## shsavePlot("fig\\nht-07")

# Estimate parameters using maximum likelihood
frVW <- function(x,ssm) {
  return(-kalman1(s2V=exp(x[1]),s2W=exp(x[2])))
}
vwMLE <- optim(c(1,-1), frVW)
mle<-exp(vwMLE$par)
kalman1(s2V=mle[1],s2W=mle[2])

s2C <- 1e-4;
mle4 <- exp(optim(c(1,-1), frVW)$par)

s2C <- 1e-2;
mle2 <- exp(optim(c(1,-1), frVW)$par)

s2C <- 1e-0;
mle0 <- exp(optim(c(1,-1), frVW)$par)

par(mfrow=c(1,3))
s2C <- 1e-4;
kalman1(s2V=mle4[1],s2W=mle4[2])
s2C <- 1e-2;
kalman1(s2V=mle2[1],s2W=mle2[2])
s2C <- 1e-0;
kalman1(s2V=mle0[1],s2W=mle0[2])

## shsavePlot("fig\\nht-08")


#
#  Successive regressions (for comparison only)
#

mt.ls<- matrix(NA,nrow=2,ncol=ntime)
ft1.ls <- rep(NA, ntime)
ft5.ls <- rep(NA, ntime)

for (i in 3:(ntime-1)){
  m <- lm(yt~time, data=data.frame(yt=yt[1:i], time=time[1:i]))
  new1 <- data.frame(time=i+1)
  new5 <- data.frame(time=i+5)
  ft1.ls[i+1] <- predict(m, newdata=new1)
  if (i <= ntime-5)
  ft5.ls[i+5] <- predict(m, newdata=new5)
}


par(mfrow=c(1,1))
plot(time,yt,col='red')
lines(time, ft1.ls,col='blue',lty=1)
lines(time, ft5.ls,col='blue',lty=2)
abline(lm(yt~time),col='black')
shsavePlot("fig\\nht-05")

mean((yt-ft1.ls)^2,na.rm=T)
mean((yt-ft5.ls)^2,na.rm=T)


####################################################################
#
# Instead put the evolution into the system equation
# 
#
####################################################################

s2W <- 0.0001
s2C <- 0.0001
s2V <- 1e0

m0  <- c(50,0)
C0  <- diag(s2C,2)
Wt  <- diag(s2W,2)
Gt  <- matrix(c(1,0,1,1), ncol=2)
Ft  <- c(1,0)
ft.vec <- et.vec <- ft5.vec <- rep(NA, ntime)
mt.mat <- matrix(NA,nrow=2,ncol=ntime+1)

mt <- m0; Ct <- C0
for (i in 1:ntime){
  at <- Gt %*% mt
  Rt <- Gt %*% Ct %*% t(Gt) + Wt
  ft <- t(Ft) %*% at
  Qt <- t(Ft) %*% Rt %*% Ft + s2V
  mt <- at + Rt %*% Ft %*% solve(Qt) %*% (yt[i]-ft)
  Ct <- Rt - Rt %*% Ft %*% solve(Qt) %*% t(Ft) %*% t(Rt)
  mt.mat[,i] <- mt
  ft.vec[i] <- ft
  if (i <= ntime-5)
    ft5.vec[i+5] <- c(1, time[i]) %*% at
  et.vec[i] <- yt[i]-ft
}
# End of the simple Kalman filter

# Mean squared prediction errors
mean((yt-ft.vec)^2,na.rm=T)
mean((yt-ft5.vec)^2,na.rm=T)

# Plot the results
par(mfrow=c(1,1))
plot(time,yt,col='red')
lines(time, ft.vec, col='blue',lty=1)
lines(time, ft5.vec,col='blue',lty=2)
abline(lm(yt~time),col='black')
## shsavePlot("fig\\nht-09")

