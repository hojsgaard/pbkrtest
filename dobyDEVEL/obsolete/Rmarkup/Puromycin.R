##    = The puromycin study =
##    == By Winnie the Pooh  ==
##    %%date

## === Introduction ===

## The idea behind the HTMLreport() function is to provide
## a simple way of translating an R-script (with a little
## bit of formatting) into a HTML document.  

## Below we illustrate the facilities of the HTMLreport() function
## (there are not very many facilities yet).

## Text appearing after one or two hashes goes into the report.
## Text appearing after three hashes does not go to the report.

## Text can be marked up in // italics // and in **bold face** and
## as  __underlined text__. These markups can not be combined


## The first lines of data are:

## <<>>=
head(Puromycin,3)
## @

## Transformation almost gives linearity
## <<fig=T,HTMLheight=300,HTMLwidth=600>>=
par(mfrow=c(1,2))
plot(rate~conc,        data=Puromycin, col=as.numeric(state))
plot(1/rate~I(1/conc), data=Puromycin, col=as.numeric(state))
## @

## Fit a model to transformed data
## <<>>=
m1 <- lm(1/rate~state + I(1/conc) + state*I(1/conc), data=Puromycin)
summary(m1)
## @





