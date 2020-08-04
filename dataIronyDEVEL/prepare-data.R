## Prepare data from Vivi Thorup for going into the package.
##
library(tidyverse)
load("esmooth.df.RData")
esm <- esmooth.df
cowbw <- esmooth.df %>% rename(cowid=CKRDYRNR, bw=BWclean) %>% select(cowid, tfc, bw)
cowbw <- cowbw %>%
    mutate(cowid = factor(substr(as.character(cowbw$cowid), start=8, stop=1000)))
head(cowbw)

## Seems ok
length(unique(as.character(esmooth.df$CKRDYRNR)))
length(unique(as.character(cowbw$cowid)))

## A smaller subset
cowbw2 <- cowbw  %>% filter(cowid %in% levels(cowid)[1:6])  %>%
    mutate(cowid=factor(cowid))

save(cowbw, file="esmooth/data/cowbw.RData")
save(cowbw2, file="esmooth/data/cowbw2.RData")

## DONE
