## = Analysis of NIR data =
## %%date

## NIR measurements are made at 152 wavelengths on 17 milk
## samples. Milk runs trough a glass tube. **Near infra red** 
## light is sent through the
## tube and the the amount of light that goes through the milk at
## different wavelengths is recorded. The milk is analyzed for contents
## of __fat__, __lactose__, __protein__ and __drymatter__.
##
## The question is whether __fat__,__lactose__, __protein__ and
## __drymatter__ can be predicted from the NIR measurements.

##@@
data(NIRmilk, package = "doBy")
dim(NIRmilk)
head(round( NIRmilk[,c( 1:6, 152:158)], 3))
##@

## @@@
nir <- NIRmilk[, 2:153]
waveLength <- gsub('\\.','',(gsub('X','',names(nir))))
matplot(waveLength, t(nir), type='l', xlab='wavelength')
##@


## More variation in data is revealed if data is centered around the mean
## of each column

## @@@
nircent <- scale(nir, center = TRUE, scale = FALSE)
matplot(waveLength,t(nircent), type='l', xlab='wavelength')
##@

# We make a PCA on the centered data:
##@@
PCA <- prcomp(nircent)
summary(PCA)
##@

## Hence, 80 % of the total variation in a 150--dimensional data set is
## explained by the first two principal components and practically all
## variation is explained by the first three principal components. This
## is quite a substantial reduction in dimension.
##
## We can display the loadings as

##@@@ 
matplot(waveLength,PCA$rot[,1:3],type='l',col=1:3,lty=1,ylim=c(-.22,.22))
abline(h=0)
library(gplots)
smartlegend('center','bottom',legend=c('PC1','PC2','PC3'),
            col=c(1:3),lty=c(1,1,1))
##@

# So - the loadings for __PC1__ (black line) come mainly from the low
# wavelengths; loadings for __PC2__ (red line) come mainly from high
# wavelengths loadings for __PC3__ come from, more localized regions of
# wavelengths. Finally the intermediate wavelengths seem to contribute
# only slightly to all three components. 

## == PCR: Principal component regression ==

## Principal component regression is very straight forward:
## <OL>
## <LI> First derive principal components of the explanatory variables and 
## <LI> Then use these principal components as explanatory variables.
## </OL>

## Let us combine the first three principal components with the responses:

##@@@
nirnew<-cbind(PCA$x[,1:3],NIRmilk[,155:158])
head(nirnew)
pairs(nirnew)
##@

# Now, we can try to make a multiple regression explaining __fat__ not
# directly in terms of the wavelengths but in terms of the principal
# components: 

##@@
m1<-lm(fat ~ PC1 + PC2 + PC3, data = nirnew)
summary(m1)
##@



