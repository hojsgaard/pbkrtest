nsc.l.s <- read.csv("deciduous.csv")

nsc.l.s<-nsc.l.s[!is.na(nsc.l.s$sugars),]


library(doBy)
library(nlme)
glmm.nsc.l = lme(sugars~elevation*habit,random=list(~1|location,~1|treelocation),data=nsc.l.s)

anova(glmm.nsc.l)

e.low <- c(1,0,0,0,1,0,0,0) # evergreen @ low
d.low <- c(1,0,0,0,0,0,0,0) # deciduous @ low
e.med <- c(1,1,0,0,1,1,0,0) # evergreen @ medium
d.med <- c(1,1,0,0,0,0,0,0) # deciduous @ medium
e.tb  <- c(1,0,1,0,1,0,1,0) # evergreen @ timberline
d.tb  <- c(1,0,1,0,0,0,0,0) # deciduous @ timberline
e.tl  <- c(1,0,0,1,1,0,0,1) # evergreen @ treeline
d.tl  <- c(1,0,0,1,0,0,0,0) # deciduous @ treeline
d.e.low <- c(0,0,0,0,1,0,0,0) # deciduous vs evergreen @ low
d.e.med <- c(0,0,0,0,1,1,0,0) # deciduous vs evergreen @ medium
d.e.tb  <- c(0,0,0,0,1,0,1,0) # deciduous vs evergreen @ timberline
d.e.tl  <- c(0,0,0,0,1,0,0,1) # deciduous vs evergreen @ treeline

planned.comparisons = esticon(glmm.nsc.l,rbind(d.low,e.low,d.med,e.med,d.tb,e.tb,d.tl,e.tl),conf.int = T)

planned.differences = esticon(glmm.nsc.l,rbind(d.e.low,d.e.med,d.e.tb,d.e.tl),conf.int = T)

planned.comparisons

planned.differences

summaryBy(sugars~elevation+habit, data=nsc.l.s, FUN=length)



















## ##
## ## doBy Bug-reports - and fixes
## ##

----- Original message -----
From: rmailbox@justemail.net
To: sorenh@agrsci.dk
Date: Mon, 03 Nov 2008 12:16:29 -0800
Subject: Problem using transformBy with one grouping level

Hello Soren,

First, thanks for contributing the package, doBy. I find it very useful and that it helps to keep code clear.

I have trouble with transformBy when there is only one level in the variable that I want to group by.
I have reproduced exactly the code in the vignette for CO2, and that works fine. Then I included a subset on CO2 data set
to illustrate the problem. It would be nice not to have to worry about the special case that the grouping variable only has one level.

library(doBy)
data(CO2)
CO2 <- transform(CO2, Treat = Treatment, Treatment = NULL)
levels(CO2$Treat) <- c("nchil", "chil")
levels(CO2$Type) <- c("Que", "Mis")
CO2 <- subset(CO2, Plant %in% c("Qn1", "Qc1", "Mn1", "Mc1"))

transformBy ( . ~ Plant, data = CO2, conc.cent = conc - mean(conc) )

# Everything above works.

But this produces an error:
transformBy ( . ~ Plant, data = subset(CO2, Plant == "Qn1" ),  conc.cent = conc - mean(conc) )

# Specifically, it gives an error message:
# Error in `[.data.frame`(grps, , 1) : undefined columns selected

# I am using R 2.7.2 on Windows XP with doBy version 3.6

Many Thanks,
Eric

## Fixing

library(doBy)
data(CO2)
CO2 <- transform(CO2, Treat = Treatment, Treatment = NULL)
levels(CO2$Treat) <- c("nchil", "chil")
levels(CO2$Type) <- c("Que", "Mis")
CO2 <- subset(CO2, Plant %in% c("Qn1", "Qc1", "Mn1", "Mc1"))

transformBy ( . ~ Plant, data = CO2, conc.cent = conc - mean(conc) )

# Everything above works.

#But this produces an error:
transformBy ( . ~ Plant, data = subset(CO2, Plant == "Qn1" ),  conc.cent = conc - mean(conc) )

sapply(flist,source)
zzz <- subset(CO2, Plant == "Qn1" )
transformBy ( . ~ Plant, data = zzz,  conc.cent = conc - mean(conc) )

transformBy ( . ~ Plant, data = subset(CO2, Plant == "Qn1" ),  conc.cent = conc - mean(conc) )


sapply(flist,source)
CO2$klyt = 1
z<-splitBy(~Plant+klyt, data=CO2)

splitBy(~1, data=CO2)



sapply(flist,source)
zzz$klyt=1
zz<-splitBy(~Plant+klyt, data=zzz)
