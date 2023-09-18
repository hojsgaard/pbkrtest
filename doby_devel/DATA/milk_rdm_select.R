library(ggplot2)
library(magrittr)
library(doBy)
library(dplyr)
options("digits"=3)

data(milkman) # in doBy
head(milkman)
dim(milkman)
milkman$cowno %>% unique %>% length
milkman$cowlact %>% unique %>% length

milkman  %>%  split_by(~cowlact) %>% sapply(nrow) %>% as.data.frame

milkman %>% group_by(cowlact) %>% summarize(n())



#' Name of the game: Given data on milk yield up to day d, how well 
#' can we predict milk yield througout 300 day lactation period?

#' ## Data selection

#' Focus on morning milking
milkman <- subset(milkman, ampm==1)
dim(milkman)

#' Only want complete cases
milkman <- milkman[complete.cases(milkman),]
dim(milkman)

#' Only want the red cows
milkman <- subset(milkman, race=="RDM")
dim(milkman)

#' First recording should be on day 1 after calving
#' Last recording should be after day 300 after calving
d <- sapplyBy(~cowlact, data=milkman, FUN=function(x) range(x$dfc)) %>% 
  t %>% as.data.frame
head(d)
large <- rownames(d)[(d$V1 == 1) & (d$V2 >= 300)]
milkman <- milkman %>% subset(cowlact %in% large)
dim(milkman)

#' Only want first lactation cows
milkman <- milkman %>% subset(lactno==1)
dim(milkman)

milkman_rdm1 <- milkman
ggplot(subset(milkman, cowlact=="0916.1")) + geom_point(aes(x=dfc, y=my))


save(milkman_rdm1, file="milkman_rdm1.RData")
