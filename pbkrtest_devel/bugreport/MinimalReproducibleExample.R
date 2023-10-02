################################################################################################
########### Power analysis #####################################################################
################################################################################################
rm(list=ls())
# install.packages("pbkrtest")
library(pbkrtest)

# Repr.Example.txt is data set with the same number of replicates for each factor combination as the origainal
# The variance explained by the different factors is very similar to the original dataset
# It produces the same kind of convergence warnings and so on...
data <- read.delim("Repr.Example.txt")

data$X.lines <- as.factor(data$X.lines)
data$line <- as.factor(data$line)
data$cross.. <- as.factor(data$cross..)
data$sim <- as.numeric(data$sim)

model1 <- lmer(sim~ 1+ (1|X.lines/line) + (1|cross..) , data=data)
summary(model1)
model2 <- lmer(sim~ 1+ (1|X.lines)  + (1|cross..), data=data)
summary(model2)

anova(model1, model2)


model1 <- lmer(sim~ 1+ (1|X.lines/line) + (1|cross..) , data=data, REML=FALSE)
summary(model1)
model2 <- lmer(sim~ 1+ (1|X.lines)  + (1|cross..), data=data, REML=FALSE)
summary(model2)

anova(model1, model2)

getME(model1, "is_REML")
getME(model2, "is_REML")


# Test for significance of line random effect

load_all("pbkrtest")
summary(PBmodcomp(model1, model2,  nsim = 100, ref = NULL, seed=NULL, cl = NULL, details = 3))

summary(PBmodcomp(model1, model2,  nsim = 1000, ref = NULL, seed=NULL, cl = NULL, details = 0))


ref <- PBrefdist(model1, model2,  nsim = 500)

summary(PBmodcomp(model1, model2,  nsim = 100, ref = ref, seed=NULL, cl = NULL, details = 3))

ref2 <- ref
ref2[ref2<=0] <- 0.01
attr(ref2, "samples") <- c(nsim=500, npos=500)

summary(PBmodcomp(model1, model2,  nsim = 100, ref = ref2, seed=NULL, cl = NULL, details = 3))
