library(lmSupport)

dL <- dfReadDat("Lab16Long2way.dat",SubID=NULL)

read.table("Lab16Long2way.dat", header=T)
                                        # This dataset describes the startle response from 68 participants. Each of them experienced 
# both predictability conditions (order counterbalanced). Their horror moving 
# watching habits and baseline startle response were recorded. 
# They were assigned to either No-Alcohol or Alcohol condition (only one of the two).

# Within-Subject Variables  : Predictability
# Between-Subject Variables : Horror movie watching, Baseline, and Alcohol

library(lme4)
ignore <- lmerControl(check.nobs.vs.nRE = "ignore")
m1 <- lmer(Startle ~ 1 + PredictC + (1 + PredictC|SubID), data=dL, control=ignore)
summary(m1)
Anova(m1, type=3, test = "F")

# Anova output with pbkrtest version 0.4-4:
# Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df)

# Response: Startle
#                  F Df Df.res    Pr(>F)    
# (Intercept) 75.935  1     67 1.246e-12 ***
# PredictC     0.280  1     67    0.5985    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Anova output with pbkrtest version 0.4-6:

# Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df)

# Response: Startle
#                  F Df Df.res    Pr(>F)    
# (Intercept) 75.935  1 67.274 1.207e-12 ***
# PredictC     0.280  1 81.924    0.5982    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
