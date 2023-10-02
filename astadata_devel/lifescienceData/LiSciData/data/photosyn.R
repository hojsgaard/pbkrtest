photosyn<-read.table("photosyn.txt",header=TRUE)
photosyn$water<-factor(photosyn$water,levels=c('low','medium','high'),
                      labels=c('low','medium','high'))


