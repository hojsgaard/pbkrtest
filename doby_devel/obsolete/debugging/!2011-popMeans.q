### ########################################
### LSMEANS
### ########################################

library(doBy)
library(lme4)
library(lattice)

flist <- list.files("doBy/R",pattern=glob2rx("*.R"),full.names=TRUE);
sapply(flist,source)

library(doBy)
dd <- expand.grid(a=factor(1:4),b=factor(1:3),c=factor(1:2))
dd$y <- rnorm(nrow(dd))
dd$x <- rnorm(nrow(dd))^2
dd$z <- rnorm(nrow(dd))
mm <- lm(y~a+b+c+log(x),data=dd)

mm <- lm(y~a+b+log(x)+z+a:b+x:z,data=dd)
mm <- lm(y~a+b+c+x,data=dd)

sapply(flist,source)
popMatrix(mm, "a")
popMatrix(mm, at=list(a=c('1','2','3','4')))
popMatrix(mm, "a", at=list(a=c('1','2','3','4')))

pma <- popMatrix(mm, "a", at=list(x=123))
summary(pma)

engine <- "esticon"
sapply(flist,source)
pme <- popMeans(mm, "a")

summary(pme)
summary(pme)

library(multcomp)
aa<-glht(mm, pma)

sapply(flist,source)
pme <- popMeans(mm, "a", engine=glht)
pme <- popMeans(mm, "a", engine=esticon)


sapply(flist,source)
popMatrix(mm, "a", at=list(a='1'))

sapply(flist,source)
popMatrix(mm, at=list(a=c('1','2')))

sapply(flist,source)
popMatrix(mm, c("a","c"), at=list(a=c('1','2')))

sapply(flist,source)
popMatrix(mm, "c", at=list(a=c('1','2'),x=123))

sapply(flist,source)
popMatrix(mm, "a", at=list(a=c('1','2'), x=123))

sapply(flist,source)
popMatrix(mm, c("a","c"), at=list(c='1'))

sapply(flist,source)
popMatrix(mm)

sapply(flist,source)
popMatrix(mm, at=list(x=123))





###################################

data (bacteria,package='LiSciData')
bacteria <- subset(bacteria, day %in% c("1","2") & sucrose %in% c("1","2"))
bacteria <- droplevels(bacteria)

bac.mod <- lm(logdens ~ day + leucine + sucrose,  data = bacteria)

sapply(flist,source)
popMatrix(bac.mod)
sapply(flist,source)
popMatrix(bac.mod, "leucine")

popMatrix(bac.mod, "leucine", at=list(leucine="2"))

sapply(flist,source)
popMatrix(bac.mod, "leucine", at=list(leucine=levels(bacteria$leucine)))

sapply(flist,source)
popMatrix(bac.mod, at=list(leucine=levels(bacteria$leucine)))

popMatrix(bac.mod, "leucine", at=list(sucrose="2"))

#####################################33
