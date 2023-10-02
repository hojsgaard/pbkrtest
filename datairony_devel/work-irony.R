load_all("dataIrony")

require(ggplot2)
data(cowbw2)

qplot(tfc, bw, data=cowbw2, colour=cowid) + geom_path()

cowlst <- cowbw2 %>% split(.$cowid)

i <- 2
dd <- cowlst[[i]]
tvar <- dd$tfc
yvar <- dd$bw

f1 <- ses(yvar, tvar, n.ahead=10)
f2 <- des(yvar, tvar, n.ahead=10)
f1
f2

plot(f1)
lines(f2)

at <- seq(10, 310, by=20)
forecast_lines(f1, at=at, h=0:20, col='red', lwd=3)
forecast_lines(f2, at=at, h=0:20, col='blue', lwd=3)












oN  <- 20
y1 <- rnorm(N, mean=5)
y2 <- y1 + 1:N

se1 <- ses(y1)


a1 <- es_fit(se1)

se2 <- ses(y2)
a2 <- es_fit(se2)

de1 <- des(y1)
b1 <- es_fit(de1)

de2 <- des(y2)
b2 <- es_fit(de2)

de1_ <- update(de1, alpha=a1)
de2_ <- update(de2, alpha=a2)
se1_ <- update(se1, alpha=b1)
se2_ <- update(se2, alpha=b2)

par(mfrow=c(2,2))
plot(se1)
plot(se2)
plot(de1)
plot(de2)


class(Nile)                # a time series object
nile <- as.numeric( Nile ) # a numeric vector
ses1 <- ses(nile)
des1 <- des(nile)

ses1
des1

print(ses1)
ses1

par(mfrow=c(1,2))
plot(ses1)
plot(des1)

ses2 <- update(ses1, alpha=fit_es(ses1, n.ahead=4))
des2 <- update(des1, alpha=fit_es(des1, n.ahead=4))

par(mfrow=c(1,2))
plot(ses2)
plot(des2)

p1 <- fit_es(ses1, n.ahead=1:10)
p2 <- fit_es(des1, n.ahead=1:10)



ses1


load_all("dataIrony")
alpha <- .getalpha(ses1$y.obs, ses1$tvar, FUN=.SES, n.ahead=5)
alpha

alpha <- .getalpha(ses1$y.obs, ses1$tvar, FUN=.SES, n.ahead=1)
alpha

load_all("dataIrony")
alpha <- .getalpha(ses1$y.obs, ses1$tvar, FUN=.DES, n.ahead=5)
alpha

alpha <- .getalpha(ses1$y.obs, ses1$tvar, FUN=.DES, n.ahead=1)
alpha

fit_alpha(ses1$y.obs, ses1$tvar, n.ahead=1:10, FUN=.DES)
fit_alpha(ses1$y.obs, ses1$tvar, n.ahead=1:10, FUN=.SES)


load_all("dataIrony")



load_all("dataIrony")
fit_es(es, 1)

load_all("dataIrony")
fit_es(es, 1:3)

load_all("dataIrony")
.fit_es(es, 1)

vv <- Vectorize(.fit_es, vectorize.args="n.ahead")
vv(es, 1:10)




str(ses1)

gg <- Vectorize(FUN=.getalpha, vectorize.args="n.ahead")

ses1$y.obs, ses1$tvar, FUN=.DES, n.ahead=1

alpha <- gg(ses1$y.obs, ses1$tvar, FUN=.DES, n.ahead=1:10)









N <- 20
y1 <- rnorm(N, sd=2)
y2 <- cumsum(y1)
y3 <- 1:N + y1

s1 <- SES(y1)
s1
plot(s1)
forecast_lines(s1)

s2 <- SES(y2)
s2
plot(s2)
forecast_lines(s2)

s3 <- SES(y3)
s3
plot(s3)
forecast_lines(s3)

d1 <- DES(y1)
d1
plot(d1)
forecast_lines(d1)

d2 <- DES(y2)
d2
plot(d2)
forecast_lines(d2)

d3 <- DES(y3)
d3
plot(d3)
forecast_lines(d3)


library(esmooth)
load_all("esmooth")
## qplot(tfc, bw, data=cowbw, colour=cowid) + geom_path()


qplot(tfc, bw, data=cowbw2, colour=cowid) + geom_path()
cowlst <- cowbw %>% split(.$cowid)
## loop through them all
i <- 2
dd <- cowlst[[i]]
tvar <- dd$tfc
yvar <- dd$bw

f1 <- SES(yvar, tvar)
f2 <- DES(yvar, tvar)

plot(f1)
at <- seq(10, 310, by=20)
forecast_lines(f1, at=at, ahead=0:20, col='red', lwd=3)
forecast_lines(f2, at=at, ahead=0:20, col='blue', lwd=3)





yvar <- aggregate(co2)
tvar <- seq_along(yvar)

f1 <- SES(yvar, tvar)
f2 <- DES(yvar, tvar)

plot(f1)
at <- 1 + seq(0, 35, by=5)
forecast_lines(f1, at=at, ahead=0:5, col='red', lwd=3)
forecast_lines(f2, at=at, ahead=0:5, col='blue', lwd=3)


yvar2 <- yvar + rnorm(length(yvar), sd=40)
f1 <- SES(yvar2, tvar)
f2 <- DES(yvar2, tvar)

plot(f1)
at <- 1 + seq(0, 35, by=5)
forecast_lines(f1, at=at, ahead=0:5, col='red', lwd=3)
forecast_lines(f2, at=at, ahead=0:5, col='blue', lwd=3)











