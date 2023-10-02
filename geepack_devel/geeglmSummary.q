# geeglm summary
# 
# Author: Tobias Verbeke
###############################################################################


### example data
tc <- textConnection("A 2004  2649  41  5450000 
        B 2004  1009  6 3099504
        C 2004  815 1 3103333
        D 2004  4916  112 36157100
        E 2004  355 3 1988500
        F 2004  3148  117 41641960
        G 2004  219 10  2709000
        H 2004  167 4 1119666
        I 2004  909 2 5065260
        A 2005  4735  66  5450000
        B 2005  913 13  3099504
        C 2005  817 1 3103333
        D 2005  5331  166 36157100
        E 2005  217 6 1988500
        F 2005  4117  145 41641960
        G 2005  309 41  2709000
        H 2005  130 8 1119666
        I 2005  1109  1 5065260
        A 2006  4359  88  5450000
        B 2006  854 3 3099504
        C 2006  749   0 3103333
        D 2006  2764  39  36157100
        E 2006  340 1 1988500
        F 2006  5008  100 41641960
        G 2006  1298  29  2709000
        H 2006  205   3 1119666
        I 2006  913 1 5065260")
exData <- read.table(tc)
close(tc)

names(exData) <- c("MS", "year", "N", "pos", "pop")
exData <- within(data = exData, {
      nonPos <- N - pos;
      wt <- pop/N
      total <- sum(pop)})

geeglmModel1 <- geeglm(cbind(pos, nonPos) ~ year, id = MS, data = exData, family = binomial(), scale.fix = TRUE,
    std.err="fij")
summary(exData$MS)
# A B C D E F G H I 
# 3 3 3 3 3 3 3 3 3 
summary(geeglmModel1)

geeglmModel1 <- geeglm(cbind(pos, nonPos) ~ year, id = MS, data = exData, family = binomial())
summary(geeglmModel1)

ooo <- orderBy(~MS, exData)
mmm <- geeglm(cbind(pos, nonPos) ~ year, id = MS, data = ooo, family = binomial())



# 
# Call:
# geeglm(formula = cbind(pos, nonPos) ~ year, family = binomial(), 
#     data = exData, id = MS, scale.fix = TRUE, std.err = "fij")
# 
#  Coefficients:
#             Estimate Std.err Wald Pr(>|W|)
# (Intercept)  252.899 228.796 1.22     0.27
# year          -0.128   0.114 1.26     0.26
# 
# Scale is fixed.
# 
# Correlation: Structure = independenceNumber of clusters:   27   Maximum cluster size: 1 #### <-- should be 3 ?



