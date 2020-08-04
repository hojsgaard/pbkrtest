### R code from vignette source 'useR-slides.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: src-useR.Rnw:4-5
###################################################
library(pbkrtest)


###################################################
### code chunk number 2: src-useR.Rnw:86-89
###################################################
data(beets,package='pbkrtest')
library(doBy)
library(lme4)


###################################################
### code chunk number 3: src-useR.Rnw:92-94
###################################################
beets$bh <- with(beets, interaction(block, harvest))
summary(aov(sugpct~block+sow+harvest+Error(bh), beets))


###################################################
### code chunk number 4: src-useR.Rnw:109-111
###################################################
options("digits"=4)
options("show.signif.stars"=F)


###################################################
### code chunk number 5: src-useR.Rnw:115-121
###################################################
beetLarge<-lmer(sugpct~block+sow+harvest+(1|block:harvest),
            data=beets, REML=FALSE)
beet_no.harv <- update(beetLarge, .~.-harvest)
beet_no.sow  <- update(beetLarge, .~.-sow)
as.data.frame(anova(beetLarge, beet_no.sow))
as.data.frame(anova(beetLarge, beet_no.harv))


###################################################
### code chunk number 6: src-useR.Rnw:139-144
###################################################
library(lattice)
data(Orthodont,package='nlme')
pdf(file='fig/ortfig01.pdf',width=8,height=8/1.7)
xyplot(distance~age|Sex, group=Subject, data=Orthodont,type='l')
graphics.off()


###################################################
### code chunk number 7: src-useR.Rnw:167-171
###################################################
ort1ML<- lmer(distance ~ age + Sex + age:Sex + (1 + age | Subject),
                  REML = FALSE, data=Orthodont)
ort2ML<- update(ort1ML, .~.-age:Sex)
as.data.frame(anova(ort1ML, ort2ML))


###################################################
### code chunk number 8: src-useR.Rnw:325-327
###################################################
beetLarge <- update(beetLarge, REML=TRUE)
beet_no.harv <- update(beet_no.harv, REML=TRUE)


###################################################
### code chunk number 9: src-useR.Rnw:332-333
###################################################
KRmodcomp(beetLarge,beet_no.harv)


###################################################
### code chunk number 10: src-useR.Rnw:343-346
###################################################
ort1<- update(ort1ML, .~., REML = TRUE)
ort2<- update(ort2ML, .~., REML = TRUE)
KRmodcomp(ort1,ort2)


###################################################
### code chunk number 11: src-useR.Rnw:389-390
###################################################
PBmodcomp(beetLarge,beet_no.harv)


###################################################
### code chunk number 12: src-useR.Rnw:433-435
###################################################
#generates the follow table
source('pvals-randcofSim.R')


