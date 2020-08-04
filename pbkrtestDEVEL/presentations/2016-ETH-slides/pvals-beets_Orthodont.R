library(lme4)
library(doBy)
library(snow)


source('../../kaff/R/KenRog-General.R')
source('../../kaff/R/KenRog-modcomp_init.R')
source('../../kaff/R/KenRog-utils.R')
source('../../kaff/R/KenRog-linearAlgebra.R')


source("../../SHD/R/PB_anova.R")
source("../../SHD/R/PB_ParmBoot.R")

..sweaveCounter <- ifelse(exists("..sweaveCounter"), ..sweaveCounter + 1, 1)
cat(sprintf("..sweaveCounter=%i\n", ..sweaveCounter))


cl=NULL
###################################################
### code chunk number 3: main-example-beets.Rnw:83-85
###################################################
data(beets)
beet0<-lmer(sugpct~block+sow+harvest+(1|block:harvest), data=beets, REML=FALSE)
beet_no.harv <- update(beet0, .~.-harvest)
beet_no.sow  <- update(beet0, .~.-sow)


ml.sow<-anova(beet0, beet_no.sow)[2,7]
ml.harv<-anova(beet0, beet_no.harv)[2,7]
KR.sow<-KRmodcomp(beet0,beet_no.sow)$stats['pval']
KR.harv<-KRmodcomp(beet0,beet_no.harv)$stats['pval']




Nsim<-1000

rr.harv <- PBrefdist(beet0, beet_no.harv, nsim=Nsim, details=1, cl=cl)
rr.sow <- PBrefdist(beet0, beet_no.sow, nsim=Nsim, details=1, cl=cl)


a<-PBmodcomp(beet0, beet_no.harv, t.ref=rr.harv)
b<-BCmodcomp(beet0, beet_no.harv, t.ref=rr.harv)
harv<-c(LRT=a$LRT['p'],KR=KR.harv,ParmBoot=a$PBtest['p'],
Bartlett=b$Bartlett['p'],Gamma=b$Gamma['p'])


a<-PBmodcomp(beet0, beet_no.sow, t.ref=rr.harv)
b<-BCmodcomp(beet0, beet_no.sow, t.ref=rr.harv)
sow<-c(LRT=a$LRT['p'],KR=KR.sow,
ParmBoot=a$PBtest['p'],
Bartlett=b$Bartlett['p'],Gamma=b$Gamma['p'])
bee<-as.data.frame(rbind(harvest=harv,sow=sow))
names(bee)<-c('LRT','KR','ParmBoot','Bartlett','Gamma')

bee<-round(bee*100,1)


save(bee,file='beets-pvals.Rdat')




############
data(Orthodont,package='nlme')
ort1ML<- lmer(distance ~ age + Sex + age:Sex + (1 + age | Subject),
                  REML = FALSE, data=Orthodont)
ort2ML<- lmer(distance ~ age + Sex + (1 + age | Subject), 
                 REML=FALSE, data=Orthodont)
ort1<- update(ort1ML, .~., REML = TRUE, data = Orthodont)
ort2<- update(ort2ML, .~., REML = TRUE, data = Orthodont)
KR<-KRmodcomp(ort1,ort2)$stats[c('pval')]

rr.orto <- PBrefdist(ort1ML, ort2ML, nsim=Nsim, details=1, cl=cl)
a<-PBmodcomp(ort1ML, ort2ML, t.ref=rr.orto)
b<-BCmodcomp(ort1ML, ort2ML, t.ref=rr.orto)
orto<-c(LRT=a$LRT['p'],KR=KR,ParmBoot=a$PBtest['p'],
Bartlett=b$Bartlett['p'],Gamma=b$Gamma['p'])

orto<-data.frame(rbind(orto))

names(orto)<-c('LRT','KR','ParmBoot','Bartlett','Gamma')
orto<-round(orto*100,1)
row.names(orto)='sex:age'

save(orto,file='orto-pvals.Rdat')


##making the4 tableas

load(file='beets-pvals.Rdat')

library(xtable)
tab<-xtable(bee,digits=1,caption=
'p-values ($\\times$ 100) for removing the harvest or sow effect (sugar beets).')
cat(print(tab,sanitize.text.function=function(x){x},
caption.placement='top'
),file='pvalbeets.txt')


load(file='orto-pvals.Rdat')
tab<-xtable(orto,digits=1,caption=
'p-values $\\times$ 100) testing the sex:age interaction (dental growth).')
cat(print(tab,sanitize.text.function=function(x){x},
caption.placement='top'
),file='pvalorto.txt')





load(file='beets-pvals.Rdat')

library(xtable)
boo<-sapply(bee,function(x) ifelse(x<0.01,'<0.001',x))
boo<-as.data.frame(boo)
rownames(boo)<-rownames(bee)
tab<-xtable(boo,digits=0,caption=
'p-values ($\\times$ 100) for removing the harvest or sow effect.')
cat(print(tab,sanitize.text.function=function(x){x},
caption.placement='top'
),file='pvalbeets.txt')


load(file='orto-pvals.Rdat')
tab<-xtable(orto,digits=1,caption=
'p-values ($\\times$ 100) testing the sex:age interaction.')
cat(print(tab,sanitize.text.function=function(x){x},
caption.placement='top'
),file='pvalorto.txt')






