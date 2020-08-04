library(tidyverse)

ToothGrowth <- ToothGrowth %>% 
    mutate(dosef = factor(dose, 
                          levels = c(0.5, 1, 2), 
                          labels = c("LO", "ME", "HI")))



tooth2 <- lm(len ~ supp + dosef, data = ToothGrowth)
coef(tooth2)

new.data <- data.frame(supp = c("VC", "OJ"), dosef = "ME")
predict(tooth2, newdata = new.data, interval = "confidence")


load_all("doby")
at <- list(supp = c("VC" ,"OJ"), dosef = "ME")
at


K <- doBy::LE_matrix(tooth2, at = at)
K

load_all("doby")
at2 <- as.data.frame(at)
at2
K2 <- LE_matrix(tooth2, at = at2)
K2


at3 <- split(at2, 1:2)


do.call(rbind, lapply(at3, function(at) LE_matrix(tooth2, at = at)))












at <- list(supp = c("VC" ,"OJ"), dosef = "ME")
K <- linest_matrix(tooth2, at = at)

K
summary(K)






linest(tooth2, L = K)







lambda <- c(0, 0, -1, 1)
sum(coef(tooth2) * lambda)

library(doBy)
esticon(tooth2, cm = lambda)







































library(ggplot2)
library(dplyr)

bacteria <- read.table("./DATA/bacteria.txt", header=T)
bacteria <- bacteria  %>% mutate(day=factor(day), leucine=factor(leucine), 
                                 sucrose=factor(sucrose))
sapply(bacteria, class)

qplot(sucrose, logdens, group=day, colour=day, data=bacteria,
      geom=c("point", "line")) + facet_grid(~leucine) 

mod <- lm(logdens ~ day + leucine + sucrose, data = bacteria)


load_all("doBy")
l1 = c(1, 1/4, 1/4, 1/4, 0, 1, 0, 0, 1)
l2 = c(1, 1/4, 1/4, 1/4, 0, 1, 0, 0, 0)
K1 <- rbind(l1, l2)
ec1 <- esticon(mod, l1)
ec2 <- esticon(mod, K1)
class(ec1) 
attributes(ec1)
attributes(ec1)$L

load_all("doBy")
le1 <- linest(mod, K1)
le1
class(le1)
names(le1)
coef(le1)
confint(le1)
summary(le1)

load_all("doBy")
as(le1, "data.frame")

at1 <- list(day = c("1" ,"2"), leucine = "3", sucrose = "4")
at2 <- list(sucrose = "4")
at3 <- list(leucine = "3", sucrose = "4")

K1 <- linest_matrix(mod, at=at1)
K2 <- linest_matrix(mod, at=at2)
K3 <- linest_matrix(mod, at=at3)
K1; K2; K3

le1 <- get_linest_list(mod, effect="leucine", at=at1)
le1
class(le1)

L1 <- linest_matrix(mod, effect="leucine", at=at1)

linest(mod, L=L1) %>% summary

le2 <- get_linest_list(mod, effect="leucine", at=at2)
class(le2)
## burde være linest_list_class
## naturligt hvis effect også var attribute.


linest(mod, L=K1)

library(doBy)
popMeans(mod, at=at1) %>% summary 

library(doBy)
load_all("doBy")
lsm.tab <- popMeans(mod, effect="leucine", at=list(sucrose="4"))
lsm.tab

lsm.tab <- popMeans(mod, effect=c("leucine", "sucrose"))
lsm.tab
x <- lsm.tab
x$coef
x$K
x$grid

## linearEstimate -> linest _class
## linest_matrix -> linest_matrix_class



confint(le1)



confint(x)




confint(xx)
confint(xx, 1:4)







%%
%%<< >>= 
%%ggplot(lsm.tab, aes(x=interaction(leucine, sucrose), y=estimate, 
%%                    colour=sucrose)) + 
%%    geom_errorbar(aes(ymin=estimate-se, ymax=estimate+se), width=.1) +
%%    geom_line() +
%%    geom_point()
%%@
%%
