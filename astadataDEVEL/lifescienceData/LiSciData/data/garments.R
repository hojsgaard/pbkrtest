garments<-read.table('garments.txt',header=TRUE)
garments$run<-as.factor(garments$run)
garments$pos<-as.factor(garments$pos)
garments$material<-as.factor(garments$material)

