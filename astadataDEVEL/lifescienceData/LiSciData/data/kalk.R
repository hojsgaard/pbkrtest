kalk<-read.table("kalk.txt",header=TRUE)
kalk$treatment<-factor(kalk$treatment)
kalk<-kalk[,c(1:3)]
