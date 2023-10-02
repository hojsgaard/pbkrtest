bachelor_avl<-read.table("bachelor_avl.txt",header=TRUE)
bachelor_avl<-transform(bachelor_avl,
                          avler = as.factor(avler)
)
