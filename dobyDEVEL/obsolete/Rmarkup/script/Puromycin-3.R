## == HERE ==
##The first lines of data are:
#<<>>=
head(Puromycin)
#@

#Transformation almost gives linearity
#<<fig=T>>=
par(mfrow=c(1,2))
plot(rate~conc,        data=Puromycin, col=as.numeric(state))
plot(1/rate~I(1/conc), data=Puromycin, col=as.numeric(state))
#@

#Fit a model to transformed data
#<<>>=
m1 <- lm(1/rate~state + I(1/conc) + state*I(1/conc), data=Puromycin)
summary(m1)
#@





