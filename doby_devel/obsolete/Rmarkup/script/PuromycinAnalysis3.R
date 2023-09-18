##    = The puromycin study =
##    == By Winnie the Pooh  ==
##    %%date

## === Introduction ===

#The first lines of data are:

#@@
head(Puromycin)
#@
#**Transformation** //almost// __gives__ &&linearity&&
##@@fig
par(mfrow=c(1,2))
plot(rate~conc,        data=Puromycin, col=as.numeric(state))
plot(1/rate~I(1/conc), data=Puromycin, col=as.numeric(state))
#@

#Fit a model to transformed data
#@@
m1 <- lm(1/rate~state + I(1/conc) + state*I(1/conc), data=Puromycin)
summary(m1)
xx <- 1:10
#@

## = NOTICE: We may add pure HTML if we want to =

## <UL>
## <LI>Unordered information. 
## <LI>Ordered information. 
## <LI>Definitions. 
## </UL>

## <OL>
## <LI>Unordered information. 
## <LI>Ordered information. 
## <LI>Definitions. 
## </OL>

## = NOTICE: We may make inline calls to R =
## \Sexpr{sum(xx)}
