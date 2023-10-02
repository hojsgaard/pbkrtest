PigCortisol <- read.table(file="PigCortisol.txt",header=TRUE)
PigCortisol$pig <- factor(PigCortisol$pig)
PigCortisol$litter <- factor(PigCortisol$litter)
PigCortisol$treat <- factor(PigCortisol$treat)

