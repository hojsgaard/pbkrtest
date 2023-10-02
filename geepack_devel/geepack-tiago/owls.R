library(tidyverse)
library(geepack)

owls <- read.table("Owls.txt", header=T)
owls$LBroodSize=log(owls$BroodSize)

## Look at data
owls$NestNight=as.factor(paste0(owls$Nest,".",substr(owls$FoodTreatment,1,3)))


## You get what you ask for: Trouble!!!
## NestNight is not sorted...
owls[, c("Nest", "NestNight")]  %>% head(20)

## Now sort:
owls <- owls  %>% group_by(NestNight)  %>% arrange(NestNight) 
owls$NestNight

fmodelo <-
    formula(SiblingNegotiation~offset(LBroodSize)+FoodTreatment+ArrivalTime)

mod.by.nest <-
    geeglm(formula=fmodelo,data=owls,family = poisson,id = Nest, corstr = "ar1")
summary(mod.by.nest)

mod.by.nestnight <- geeglm(formula=fmodelo,data=owls,family = poisson,id=NestNight,corstr="ar1")
summary(mod.by.nestnight)




