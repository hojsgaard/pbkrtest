library(pbkrtest)
library(ggplot2)

FILENAME = "sim-data-pb-sat.RData"

#' ## Do graphics
## qplot(NT, mean, color=meth, data=mb12) + geom_line()
## qplot(log2(NT), log2(mean), color=meth, data=mb12) + geom_line()

qplot(log2(NT), log2(mean), color=meth, data=mb12) + geom_line() + 
    geom_errorbar(aes(ymin=log2(lq), ymax=log2(uq)))

qplot(log2(NS), log2(mean), color=meth, data=mb22) + geom_line() + 
    geom_errorbar(aes(ymin=log2(lq), ymax=log2(uq)))


#' ## Fit nice models
#' 
lm(log(mean) ~ log(NT), data=mb12, subset=meth=="KR")
lm(log(mean) ~ log(NT), data=mb12, subset=meth=="SAT")

lm(log(mean) ~ log(NS), data=mb22, subset=meth=="KR")
lm(log(mean) ~ log(NS), data=mb22, subset=meth=="SAT")






