##############################################
##    = Rscript2HTML Example 1 =
##    == The Puromycin data  ==
##    === Søren Højsgaard  ===
##    %%date
##############################################

## === The __Puromycin__ data ===
## The first lines of data are:

##@@
head(Puromycin,3)
nr = nrow(Puromycin)
## @
## There are \Sexpr{nr} rows in the dataframe.
## (Notice that we may refer to R expression in the text).

## Transformation almost gives __linearity__
##@@fig
par(mfrow=c(1,2))
plot(rate~conc,        data=Puromycin, col=as.numeric(state))
plot(1/rate~I(1/conc), data=Puromycin, col=as.numeric(state))
## @

## Fit a model to **transformed** data
## <<>>=
m1 <- lm(1/rate~state + I(1/conc) + state*I(1/conc), data=Puromycin)
summary(m1)
## @

## Model diagnostics
## <<fig=T,HTMLheight=300,HTMLwidth=600>>=
par(mfrow=c(2,2))
plot(m1)
## @

### TODO: Maybe more could be done...
## === Additional topics ===

## We may use HTML markups as well, for example

## <UL>
## <LI> Here comes
## <LI> the sun
## </UL>

## <OL>
## <LI> Here comes
## <LI> the sun
## </OL>

## <DL>
## <DT><STRONG>Here Comes</STRONG>
## <DD> here comes
## <DT><STRONG>The Sun</STRONG>
## <DD> the sun.
## </DL>




