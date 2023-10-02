library(lme4)
library(pbkrtest)
library(parallel)
library(lmerTest)        
#' ## Analyze data

rm(list=ls())
load("nested-sim-2018.RData")
ls()

grp <- design$grp
subj <- design$subj

i <- 1
lg <- lmer(Yn[,i] ~ grp + (1|subj), REML=FALSE)
sm <- update(lg, .~. - grp)


## FIXME: Report time; do not parallelize by default
load_all("pbkrtest")
seqPBmodcomp(lg, sm, h=20)
seqPBmodcomp2(lg, sm, h=200)


SAmodcomp
anova(lg, sm)
ll <- KRmodcomp(lg, sm)

L <- model2restrictionMatrix(lg, sm)    
out <- contest(lg, L)




STmodcomp <- function(lg, sm){
    L <- model2restrictionMatrix(lg, sm)    
    contest(lg, L)
}




