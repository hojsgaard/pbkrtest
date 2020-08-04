##    = HTMLreport Example 1 =
##    == The Puromycin data  ==
##    === Søren Højsgaard  ===
##    %%date

## === The &&Puromycin&& data ===
## The first lines of data are:

## <<>>=
head(Puromycin,3)
## @

## Transformation almost gives __linearity__
## <<fig=T,HTMLheight=300,HTMLwidth=600>>=
par(mfrow=c(1,2))
plot(rate~conc,        data=Puromycin, col=as.numeric(state))
plot(1/rate~I(1/conc), data=Puromycin, col=as.numeric(state))
## @

## Fit a model to **transformed** data
## <<>>=
m1 <- lm(1/rate~state + I(1/conc) + state*I(1/conc), data=Puromycin)
summary(m1)
## @

##<<aaa,fig=T>>=
plot(rnorm(1000))
##@



