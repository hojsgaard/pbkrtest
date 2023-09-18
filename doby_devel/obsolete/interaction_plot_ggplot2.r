require(dplyr)
library(magrittr)
library(ggplot2)

interaction_plot <- function(.data, .formula, interval="conf.int") { 

    interval = match.arg(tolower(interval), c("conf.int", "boxplot", "none"))

    if (!inherits(.formula, "formula")) stop("'.formula' is not formula")
    if (length(.formula) != 3)          stop("'.formula' is not two sided")
    
    lhs <- all.vars(.formula[[2]])
    rhs <- all.vars(.formula[[3]])
    if (length(rhs) < 2) stop("rhs must have at least two elements")

    rr <- sym(lhs)
    s1 <- sym(rhs[1])
    s2 <- sym(rhs[2])
    
    ## If lhs is transformed
    resp <- eval(.formula[[2]], .data)
    .data$RESP <- resp  ## KLUDGY
    rr2 <- sym("RESP")  ## KLUDGY

    dd1 <- .data %>% group_by(!!sym(s1), !!sym(s2)) 
    tmp <- dd1 %>% summarise(val = mean({{ rr2 }}),
                             sd  = sd({{ rr2 }}),
                             lwr = val-2*sd, upr=val+2*sd,
                             .groups="keep")
    ##print(.data); print(dd1); print(tmp)

    switch(interval,
           "boxplot" = {
               ## BOXPLOT
               pp <- ggplot(.data, aes(x = factor(!!sym(s1)), y = !!sym(rr2),
                                       colour = !!sym(s2))) 
               
               pp + labs(y = deparse(.formula[[2]]), x=rhs[1]) +
                   geom_boxplot() + ## FIXME want conf. interval bars
                   geom_point(data = tmp, aes(y = val)) +
                   geom_line(data = tmp, aes(y = val, group = !!sym(s2))) + 
                   theme_bw()
           },
           "conf.int" = {
               ## mean +/- 2 sd
               pp <- ggplot(tmp, aes(x = factor(!!sym(s1)), y = val,
                                     colour = !!sym(s2)))
               
               pp + labs(y = deparse(.formula[[2]]), x=rhs[1]) +
                   geom_point(data = tmp, aes(y = val)) +
                   geom_line(data = tmp, aes(y = val, group = !!sym(s2))) + 
                   geom_errorbar(aes(ymin=lwr, ymax=upr), data=tmp,
                                 width=.4, position=position_dodge(0.1))
           },
           "none" = {
               pp <- ggplot(tmp, aes(x = factor(!!sym(s1)), y = val,
                                     colour = !!sym(s2)))               
               pp + labs(y = deparse(.formula[[2]]), x=rhs[1]) + 
                   geom_point(data = tmp, aes(y = val)) +
                   geom_line(data = tmp, aes(y = val, group = !!sym(s2))) 
           })
}


.data <- ToothGrowth
.formula  <- len ~ dose + supp

ToothGrowth %>% interaction_plot(len ~ dose + supp)
ToothGrowth %>% interaction_plot(len ~ dose + supp, interval="conf.int")
ToothGrowth %>% interaction_plot(len ~ dose + supp, interval="boxplot")
ToothGrowth %>% interaction_plot(len ~ dose + supp, interval="none")

warpbreaks %>% interaction_plot(log(breaks) ~ tension + wool)





dat <- ToothGrowth


## Works
by_ <- mtcars %>% group_by(cyl, gear)
by_ %>% summarise(
  disp = mean(disp),
  hp = mean(hp)
)


aa1 <- dat %>% group_by(dose, supp)
aa1
aa2 <- aa1 %>% summarise(val=mean(len))
aa2 ## Why


## Mimic this
tmp1 <- ddply(dat,.(dose, supp),summarise, val = mean(len), sd=sd(len))
ggplot(dat, aes(x = factor(dose), y = len, colour = supp)) + 
    geom_boxplot() + 
    geom_point(data = tmp1, aes(y = val)) +
    geom_line(data = tmp1, aes(y = val, group = supp)) + 
    theme_bw()

tmp1 <- ddply(dat,.(dose, supp),summarise, val = mean(len))

pp <- ggplot(dat, aes(x = factor(dose), y = len, colour = supp)) +
    geom_point(data = tmp1, aes(y = val)) +
    geom_line(data = tmp1, aes(y = val, group = supp)) +
    geom_crossbar(data=tmp1, aes(ymin=lwr, ymax=upr)) +
    theme_bw()
pp



tmp1 <- ddply(dat,.(dose, supp),summarise, val = mean(len), sd=sd(len), lwr=val-2*sd, upr=val+2*sd)




pp + geom_linerange(aes(ymin = lwr, ymax = upr), data=tmp1)




pp
pp + geom_boxplot()







fac1 <- "dose"
fac2 <- "supp"
resp <- "len"

s1 <- sym(fac1)
s2 <- sym(fac2)
rr <- sym(resp)

dat <- ToothGrowth
dd1 <- dat %>% group_by(!!sym(s1), !!sym(s2)) ## Works
tmp <- dd1 %>% summarise(val = mean({{ rr }})) %>% as.data.frame
#dd1 %>% summarise(value = mean( .data[[rr]] ))
#dd1 %>% summarise(value = mean( .data[[resp]] ))

ggplot(dat, aes(x = factor(!!sym(fac1)), y = !!sym(rr), colour = !!sym(fac2))) + 
    geom_boxplot() + 
    geom_point(data = tmp, aes(y = val)) +
    geom_line(data = tmp, aes(y = val, group = !!sym(fac2))) + 
    theme_bw()


library(lobstr)
library(rlang)

ff <- len ~ dose + supp

lhs <- all.vars(ff[[2]])
rhs <- all.vars(ff[[3]])


