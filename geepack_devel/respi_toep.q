
####
#  on  the respiratory data:
# exemplifying to use a user defined correlations structure
# and to fit a model to missing data ( exemplified for the 
#                                                 toeplitx structure
#   and the ar1 structure using the waves argument
##
#  fiting a toeplitz structure  sigma_ij=sigma_|i-j|
#    1.   on the full data set
#    2. on the missing  data set
#  3. fitting an ar1 structure on the missing data
#            just using the waves argument


library(lattice)
library(geepack)
library(dataRep)
library(doBy)
library(locfit)
library(xtable)


#reading the data:
data(respiratory)
v<-respiratory
v$person<-with(v,interaction(center,id))
v$center<-factor(v$center)

######
# analysis of the toeplitz structure
#####
#  construction the design matrix for the toplitz correlation
#      a) contruction of the design jmatrix for unstructured correlation
   zcor<- genZcor(clusz = c(table(v$person)), waves = v$visit, corstrv=4) 
#      b) adding columns to parameterize the toeplitz strucure
   zcor.toep<-matrix(NA,nrow(zcor),3)
   zcor.toep[,1]<-apply(zcor[,c(1,4,6)],1,sum)
   zcor.toep[,2]<-apply(zcor[,c(2,5)],1,sum)
   zcor.toep[,3]<-zcor[,3]

#  model fit
m.toep<-geese(outcome~baseline+center+sex+treat+age+I(age^2),
               data=v,id=person,
              family=binomial,corstr='userdefined',zcor=zcor.toep)


m2<-geeglm(size ~ as.factor(time)*treat, Gamma, data=sitkamiss, id=tree,
  corst='ar1', waves=ordering)

sapply(f,source)
m.toep<-geeglm(outcome~baseline+center+sex+treat+age+I(age^2),
               data=v,id=person,              family=binomial)


sapply(f,source)
m.toep<-geeglm(outcome~baseline+center+sex+treat+age+I(age^2),
               data=v,id=person,
              family=binomial,corstr='userdefined',zcor=zcor.toep)



#estimating the toeplitz structure with missing values

# generating data with deleting some observations
  v.miss<-v[runif(v$visit)>0.2,]
# generating the user defined toeplitz matrix
 zcor<- genZcor(clusz = c(xtabs(~id+center,data=v.miss)), waves = v.miss$visit, corstrv=4) 
 zcor.toep<-matrix(NA,nrow(zcor),3)
 zcor.toep[,1]<-apply(zcor[,c(1,4,6)],1,sum)
 zcor.toep[,2]<-apply(zcor[,c(2,5)],1,sum)
 zcor.toep[,3]<-zcor[,3]

#model fit
m.toep.miss<-geese(outcome~baseline+center+sex+treat+age+I(age^2),
               data=v.miss,id=person,
              family=binomial,corstr='userdefined',zcor=zcor.toep)


###########fitting an ar1 model to the missing data
##  using the waves argument
m.ar1.miss<-geese(outcome~baseline+center+sex+treat+age+I(age^2),
               data=v.miss,waves=visit,id=person,
              family=binomial,corstr='ar1')
##############################


print('parameters: toeplitz')
print(summary(m.toep)$mean)
print('toeplitz, missing data')
print(summary(m.toep.miss)$mean)


print('correwlation parameters: toeplitz')
print(summary(m.toep)$correlation)
print('toeplitz, missing data')
print(summary(m.toep.miss)$correlation)



