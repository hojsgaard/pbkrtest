bacteria<-read.table("bacteria.txt",header=TRUE)
bacteria$day<-factor(bacteria$day)
bacteria$sucrose<-factor(bacteria$sucrose)
bacteria$leucine<-factor(bacteria$leucine)

