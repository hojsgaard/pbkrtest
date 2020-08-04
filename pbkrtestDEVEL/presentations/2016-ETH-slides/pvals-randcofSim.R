#thr analysis for table in the talk
num<-1
v<-get(load(paste('../../data/dataSimAnalysUHH/randcofKR-R-',num,'.Rdat',sep='')))
wtime<-     read.csv('../../data/dataSimAnalysUHH/randcofKR-SAS-1.csv')
wintercept<-read.csv('../../data/dataSimAnalysUHH/randcofKR-SAS-intercept-1.csv')
h<-get(load(file=paste('../../data/dataSimAnalysUHH/randcofSHD-R-',num,'d.Rdat',sep='')))


wtimeChi<-read.csv('../../data/dataSimAnalysUHH/randcofKR-SAS-chisq-1.csv')
winterceptChi<-read.csv('../../data/dataSimAnalysUHH/randcofKR-SAS-intercept-chisq-1.csv')

pvalsas<-	function(prob){
prob<-as.character(prob)
prob<-cbind(as.numeric(ifelse(prob=='<.0001',runif(1,0,0.0001),prob)))
prob
}


pvals<-function(x) mean(x<0.05,na.rm=TRUE)

#analysis forf the  intercept effect
probH<-h$pintercept
res<-v$res2
probKR<-cbind(res$pval)
probSAS<-pvalsas(wintercept$ProbF)
probWald<-pvalsas(winterceptChi$ProbChi)


k<-list(probKR=probKR,probH=probH,probSAS=probSAS,probWald=probWald)
kintercept<-unlist(sapply(k,function(x) apply(x,2,pvals)))

#analysis forf the  time effect
probH<-h$ptime
res<-v$res1
probKR<-cbind(res$pval)
probSAS<-pvalsas(wtime$ProbF)
probWald<-pvalsas(wtimeChi$ProbChi)

k<-list(probKR=probKR,probH=probH,probSAS=probSAS,probWald=probWald)
ktime<-unlist(sapply(k,function(x) apply(x,2,pvals)))


k<-rbind(kintercept,ktime)
k<-round(k*100,1)
k<-as.data.frame(k)
library(xtable)
nams<-c('KR(R)','LR','ParmBoot','Bartlett','Gamma','KR(SAS)','Wald')
id<-c(2,7,3,4,5,1,6)
##
k<-k[,id]
names(k)<-nams[id]
rownames(k)<-c('$\\beta_0$','$\\beta_1$')

tab<-xtable(k[,1:2],digits=1,caption=
'Observed test sizes (in \\%) for $H_0: \\beta_k=0$ for random coefficient model.')
cat(print(tab,sanitize.text.function=function(x){x},
caption.placement='top'
),file='pvalrandcof1.txt')

tab<-xtable(k,digits=1,caption=
'Observed test sizes ($\\times 100$) for $H_0: \\beta_k=0$ for random coefficient model.')
cat(print(tab,sanitize.text.function=function(x){x},
caption.placement='top'
),file='pvalrandcof2.txt')



