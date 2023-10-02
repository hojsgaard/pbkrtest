eelworm<-read.table("eelworm.txt",header=TRUE)
eelworm<-transform(eelworm,
 block=factor(block),
 plot=factor(plot),
 fumigant=factor(fumigant),
 treat=factor(treat)
)
