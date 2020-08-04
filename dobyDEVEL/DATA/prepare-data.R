fa <- read.table("fatacid.txt", header=T)
library(ggplot2)
library(tidyverse)
library(broom)
fa <- fa %>% mutate(sample=factor(sample), litter=factor(litter),
                    pigid=factor(pigid),
                    sample=factor(sample,                             
                                  labels=c("s60", "s100")))
summary(fa)
sapply(fa, class)
fa %>% spread(sample, x14) -> fatacid
summary(fatacid)
fatacid %>% sapply(class)
fatacid
colnames(fatacid)[4:5] <- c("x14.1", "x14.2")
save(fatacid, file="fatacid.RData")









qplot(dose, x14.1, data=fatacid)

bartlett.test(x14.1 ~ dose, data=fatacid)
bartlett.test(x14.2 ~ dose, data=fatacid)


qplot(dose, x14.2, data=fatacid)

qplot(dose, log(x14.2), data=fatacid)

qplot(x14.1, x14.2, data=fatacid, color=factor(dose))




lm(x14.2 ~ x14.1, data=fatacid) %>% tidy

lm(x14.2 ~ dose, data=fatacid) %>% tidy

lm(x14.2 ~ dose + x14.1, data=fatacid) %>% tidy

gather(fatacid, sample, x14, x14.1, x14.2)

fa2 <- fatacid %>% group_by(dose) %>% summarise(m1=mean(x14.1, na.rm=T),
                                               v1=var(x14.1, na.rm=T),
                                               s1=sd(x14.1, na.rm=T),
                                               m2=mean(x14.2, na.rm=T),
                                               v2=var(x14.2, na.rm=T),
                                               s2=sd(x14.2, na.rm=T))
fa2


qplot(m2, v2, data=fa2)
qplot(log(m2), log(v2), data=fa2)
lm(log(v2) ~ log(m2), data=fa2) %>% tidy



qplot(dose, m, data=fa2, colour=sample)




pp <- qplot(dose, x14, colour=sample, data=fa)
pp

pp <- qplot(dose, log(x14), colour=sample, data=fa)
pp

pp <- qplot(dose, sqrt(x14), colour=sample, data=fa)
pp
