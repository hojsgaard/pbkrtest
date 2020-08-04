library(doBy)
load_all("doBy")

myfun1 <- function(x){c(m=mean(x), s=sd(x))}

summaryBy(list(c("conc", "uptake"), "Plant"), 
          data=CO2, FUN=myfun1)


list(, "Plant")

ll <- list("conc", "uptake", lu="log(uptake)")



orderBy(c("Temp", "Month"), data = airquality) 
    




data(sens_biscuit, package="SensMap")

load_all("doBy")
prod_bisc=summaryBy(. ~ produit, data=sens_biscuit, FUN=c(mean),keep.names = TRUE,na.rm=TRUE)


prod_bisc=summaryBy(Vcouleur + Vepaisseur+Oeuf ~ produit, data=sens_biscuit, FUN=c(mean),keep.names = TRUE,na.rm=TRUE)


head(prod_bisc)

orderBy(c("Temp", "Month"), data = airquality)





l1<-splitBy(~vs, data=mtcars)

l2<-splitByOLD(~vs, data=mtcars)

lapply(l1, summary)
lapply(l2, summary)



sumfun <- function(x, ...){
   c(m=mean(x, na.rm=TRUE, ...), v=var(x, na.rm=TRUE, ...), l=length(x))
 }
 summaryBy(Ozone + Solar.R ~ Month, data=airquality, FUN=sumfun )
 

data(dietox)
summaryBy(cbind(lw=log(Weight), Feed, I(Feed^2)) ~ Evit + Cu, data=dietox, FUN=mean)

s<-"cbind(log(Weight), Feed)"
eval(parse(text=s), dietox)

CO2 %>% summary_by(cbind(conc, uptake, lu=log(uptake)) ~ Plant, 
                   FUN=myfun1)

ss1 <- "cbind(conc, uptake, lu=log(uptake))"
ss2 <- "conc"
gsub("^cbind\\((.*)\\)$", "\\1", ss1)
gsub("^cbind\\((.*)\\)$", "\\1", ss2)



ff <- "cbind(conc, uptake, lu=log(uptake))"

## strip cbind( ... ) if it is there; then put cbind around
ff2 <- gsub("^cbind\\((.*)\\)$", "\\1", ff)
paste0("cbind( ", ff2, " )")

gsub("^cbind\\((.*)\\)$", "\\1", ss2)






str <- "here åandå there åandå here åagainå and..."
gsub("å(.*?)å", "\verb\\+\\1\\+", str)



myfun1 <- function(x){c(m=mean(x), s=sd(x))}

load_all("doBy")
#debug("summaryBy")
summaryBy(cbind(conc, uptake, lu=log(uptake)) ~ Plant, 
          data=CO2, FUN=myfun1)
#undebug("summaryBy")

summaryBy(cbind(lc=log(conc)) ~ Plant,           data=CO2, FUN=myfun1)












load("datfmr.RData")

fmr
orderBy(fmr, dat)


library(doBy)
load_all("doBy")
load("datfmr.RData")

fmr
d1 <- orderByOLD(fmr, dat)

ff <- c("chr12_30.87", "chr1_71.35",  "chr1_86.65", "Cyp2b10")


d2 <- dplyr::arrange_(dat, .dots=ff)


fmr
orderByOLD(fmr, dat)


dat <- as.matrix(iris[,1:4])

load_all("doBy")
orderBy(~Sepal.Length + Sepal.Width, dat)  %>% class
orderByOLD(~Sepal.Length + Sepal.Width, dat)  %>% class

orderBy(~Sepal.Length + Sepal.Width + foo, dat)  %>% class
orderByOLD(~Sepal.Length + Sepal.Width + foo, dat)  %>% class


dplyr::arrange(as.data.frame(dat), Sepal.Length, Sepal.Width, foo)

d2 <- dplyr::arrange_(as.data.frame(dat), .dots=c("Sepal.Width", "jjj"))






















library(doBy)
library(tidyverse)
load_all("doBy")

ToothGrowth <- ToothGrowth %>%
    mutate(dosef = factor(dose,
                          levels = c(0.5, 1, 2),
                          labels = c("LO", "ME", "HI")))
ToothGrowth$x1 <- rnorm(nrow(ToothGrowth))
ToothGrowth$x2 <- rnorm(nrow(ToothGrowth))

tooth2 <- lm(len ~ supp + dosef + x1, data = ToothGrowth)
coef(tooth2)


coef(tooth2)
new.data <- data.frame(supp = c("VC", "OJ"), dosef = "ME")
new.data
predict(tooth2, newdata = new.data)


at <- list(supp = c("VC", "OJ"), dosef = "ME")
##K  <- LSmatrix(tooth2, at = at)
##K  <- LSmatrix(tooth2, at = at)
K  <- linest_matrix(tooth2, at=at)
K

linest(tooth2, K = K)

load_all("doBy")
K2 <- linest_matrix(tooth2, at=new.data)
K2




linest(tooth2, K = K2)



##l1 <- get_linest_list(tooth2, at=at)
at <- list(dosef = "ME")




l12 <- gll(tooth2,  effect="supp", at=NULL)
#l1
l12


aggregate_linest_list(l1)




l2 <- get_linest_list(tooth2, at=new.data)
aggregate_linest_list(l2)


## Global average
tooth2 <- lm(len ~ x1 + x2, data = ToothGrowth)
l12 <- gll(tooth2,  effect=NULL, at=NULL)

## Global average
load_all("doBy")
tooth2 <- lm(len ~ supp + x1 + x2, data = ToothGrowth)
l12 <- gll(tooth2,  effect=NULL, at=NULL)










#' ## Updating doBy
#'
#' Updating doBy along th lines of dplyr and friends.
#' 

library(doBy)
library(dplyr)
library(tidyr)
library(plyr)
load_all("doBy")

data(dietox, package="doBy")
data(milkman)


load_all("doBy")

x <- subsetBy(~Month, subset=Wind > mean(Wind), data=airquality)
x

subset_by <- function(data, formula, subset, select, drop=FALSE, join=TRUE,...){
    print(subset)
    subsetBy(formula=formula, subset=subset, data=data, select=select, drop=drop,
             join=join, ...)
}

subset_by(airquality, ~Month, subset=Wind > mean(Wind))


d <- splitBy(~Month, airquality)
lapply(d, function(x)
    subset(x, subset, select, drop)



subset_by <- function(data, formula, subset, select, drop=FALSE, join=TRUE,...){
    d <- splitBy(formula, data)
    lapply(d, function(x)
        subset(x, subset=subset, select=select, drop=drop, ...)
           ) 
}
subset_by(airquality, ~Month, subset=Wind > mean(Wind))




subset_by(airquality, ~Month, subset=Wind > mean(Wind))
















subsetBy2 <- function(formula, subset, data=parent.frame(), select, drop=FALSE, join=TRUE, ...){
    dat <- splitBy(formula, data)
    cl <- match.call()
    cl
    }
x <- subsetBy2(~Month, subset=Wind > mean(Wind), data=airquality)


frm <- unlist(rhsf2list(~Month))
airquality  %>% group_by_(frm)  %>%
    subset(Wind>mean(Wind))

x <- subsetBy(~Month, subset=Wind > mean(Wind), data=airquality)





    
















#' ### splitBy
#'

dat <- dietox
frm <- ~Evit+Cu
frm2 <- c("Evit","Cu")
splitBy(formula = frm, data = dietox)
splitBy(formula = frm2, data = dietox)

frm <- ~Month
frm2 <- "Month"
splitBy(frm, data=airquality)
splitBy(frm2, data=airquality)

group_by(airquality, "Month") %>%
    do(vals=data.frame(.))  %>%
    select(vals) %>%
    lapply(function(x) {(x)})

dat <- airquality
frm <- ~Month
frm2 <- "Month"

d1 <- splitBy(frm, dat)
d2 <- plyr::dlply(dat, frm, identity)
d3 <- plyr::dlply(dat, frm2, identity)

airquality$Month <- factor(airquality$Month)
airquality <- subset(airquality, Month %in% 5:7)

d1 <- splitBy(frm, dat)
d2 <- plyr::dlply(dat, frm, identity)
d3 <- plyr::dlply(dat, frm2, identity)


dat <- milkman
frm <- "cowlact"
frm <- c("cowno", "lactno")
d1 <- splitBy(frm, dat)
d2 <- splitBy2(frm, dat)


library(microbenchmark)
microbenchmark(
    splitBy2(frm, data=dat),
    splitBy(frm, data=dat),
    times=10
)

#' ### orderBy

dat <- airquality
frm <- ~ -Day + Month
d1 <- orderBy(frm, dat)
d2 <- orderBy2(frm, dat)
head(d1); head(d2)




dietox <- subset(dietox, Time==12)

sampleBy(formula = ~ Evit + Cu, frac=.1, data = dietox)


#' ### scaleBy


load_all("doBy")



dat <- milkman
frm <- list(".", c("cowno", "lactno"))
frm <- list(".", "cowlact")
load_all("doBy")
#dat <- iris
#frm <- . ~ Species
d1 <- scaleBy(frm, data=dat)
head(d1)
d2 <- scaleByOLD(frm, data=dat)
head(d2)

microbenchmark::microbenchmark(
                    scaleBy(frm, data=dat),
                    scale_by(dat, frm),
                    scaleByOLD(frm, data=dat),
                    times=10
)



mf <- scaleBy(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species,     
             data=iris)
mf

mf <- scaleBy2(. ~ Species,  
             data=iris)

scale_by(iris, . ~ Species)


mf <- scaleBy2(cbind(mpg, qsec, mpg/qsec) ~ cyl + am, data=mtcars)

mf[-1]
mf[1]

names(mf[1])


cn <- colnames(mf[[1]])
dn <- paste("V",seq_along(cn), sep="")
dn[nchar(cn)>0] <- cn
dn

dat <- mf[[1]]
colnames(dat) <- dn

z <- split(as.data.frame(dat), mf[-1])
out <- lapply(z, scale)
out



names(mf[1])
names(mf[-1])

scale(mf[1L])





    
##     return(mf)    
##     if (is.matrix(mf[[1L]])) {
##         lhs <- as.data.frame(mf[[1L]])
##         aggregate.data.frame(lhs, mf[-1L], FUN = FUN, ...)
##     }
##     else aggregate.data.frame(mf[1L], mf[-1L], FUN = FUN, ...)
## 


















#' ### To vignette
#'

library(doBy)

myfun1 <- function(x){c(m=mean(x), v=var(x), n=length(x))}

library(dplyr)
CO2 %>% group_by(Plant)
CO2 %>% group_by(Plant)  %>% summarize(mean(conc), mean(uptake), var(conc), var(uptake))

dat <- CO2
frm <- conc + uptake ~ Plant
load_all("doBy")
zzz <- doBy:::.get_variables(formula=frm, data=dat, id=NULL, debug.info=F) ## ; str(zzz)

frm <- cbind(conc, uptake) ~ Plant
load_all("doBy")
zzz <- doBy:::.get_variables(formula=frm, data=dat, id=NULL, debug.info=F) ## ; str(zzz)
zzz


sb <- function (formula, data, FUN, ..., subset, na.action = na.omit) 
{
    if (missing(formula) || !inherits(formula, "formula")) 
        stop("'formula' missing or incorrect")
    if (length(formula) != 3L) 
        stop("'formula' must have both left and right hand sides")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    m$... <- m$FUN <- NULL
    m[[1L]] <- quote(stats::model.frame)
    if (formula[[2L]] == ".") {
        cat("We are here \n")
        rhs <- as.list(attr(terms(formula[-2L]), "variables")[-1])
        lhs <- as.call(c(quote(cbind), setdiff(lapply(names(data), 
            as.name), rhs)))
        formula[[2L]] <- lhs
        m[[2L]] <- formula
    }
    mf <- eval(m, parent.frame())
    str(mf)
    mf <<- mf
    if (is.matrix(mf[[1L]])) {
        lhs <- as.data.frame(mf[[1L]])
        aggregate.data.frame(lhs, mf[-1L], FUN = FUN, ...)
    }
    else aggregate.data.frame(mf[1L], mf[-1L], FUN = FUN, ...)
}
CO2$y <- rnorm(nrow(CO2))
CO2$Plant <- NULL
dat <- CO2
frm <- cbind(uptake, conc) ~ Type + .
a<-sb(frm, data=dat, FUN=myfun1)







summaryBy(uptake + conc ~ Plant + . , data=dat, FUN=myfun1)













aggregate(cbind(conc, uptake) ~ Plant, data=CO2, FUN=myfun1)

summaryBy(conc + uptake ~ Plant, data=CO2, FUN=myfun1)

summaryBy(conc + uptake ~ Plant + . , data=CO2, FUN=myfun1)











CO2 %>% group_by() %>% summarize(mean(conc), mean(uptake), var(conc), var(uptake))

CO2 %>% group_by() %>% summarize_each(funs(mean, sd), conc, uptake)

CO2 %>% group_by() %>% summarize_each(funs(myfun1), conc, uptake)




#' Virker ikke.....
CO2 %>% group_by(Plant)  %>% do( myfun1(conc) )
CO2 %>% group_by(Plant)  %>% summarize( myfun1(conc) )



fmr
orderByOLD(fmr, dat)






















library(doBy)
library(tidyverse)
load_all("doBy")

ToothGrowth <- ToothGrowth %>%
    mutate(dosef = factor(dose,
                          levels = c(0.5, 1, 2),
                          labels = c("LO", "ME", "HI")))
ToothGrowth$x1 <- rnorm(nrow(ToothGrowth))
ToothGrowth$x2 <- rnorm(nrow(ToothGrowth))

tooth2 <- lm(len ~ supp + dosef + x1, data = ToothGrowth)
coef(tooth2)


coef(tooth2)
new.data <- data.frame(supp = c("VC", "OJ"), dosef = "ME")
new.data
predict(tooth2, newdata = new.data)


at <- list(supp = c("VC", "OJ"), dosef = "ME")
##K  <- LSmatrix(tooth2, at = at)
##K  <- LSmatrix(tooth2, at = at)
K  <- linest_matrix(tooth2, at=at)
K

linest(tooth2, K = K)

load_all("doBy")
K2 <- linest_matrix(tooth2, at=new.data)
K2




linest(tooth2, K = K2)



##l1 <- get_linest_list(tooth2, at=at)
at <- list(dosef = "ME")




l12 <- gll(tooth2,  effect="supp", at=NULL)
#l1
l12


aggregate_linest_list(l1)




l2 <- get_linest_list(tooth2, at=new.data)
aggregate_linest_list(l2)


## Global average
tooth2 <- lm(len ~ x1 + x2, data = ToothGrowth)
l12 <- gll(tooth2,  effect=NULL, at=NULL)

## Global average
load_all("doBy")
tooth2 <- lm(len ~ supp + x1 + x2, data = ToothGrowth)
l12 <- gll(tooth2,  effect=NULL, at=NULL)










#' ## Updating doBy
#'
#' Updating doBy along th lines of dplyr and friends.
#' 

library(doBy)
library(dplyr)
library(tidyr)
library(plyr)
load_all("doBy")

data(dietox, package="doBy")
data(milkman)


load_all("doBy")

x <- subsetBy(~Month, subset=Wind > mean(Wind), data=airquality)
x

subset_by <- function(data, formula, subset, select, drop=FALSE, join=TRUE,...){
    print(subset)
    subsetBy(formula=formula, subset=subset, data=data, select=select, drop=drop,
             join=join, ...)
}

subset_by(airquality, ~Month, subset=Wind > mean(Wind))


d <- splitBy(~Month, airquality)
lapply(d, function(x)
    subset(x, subset, select, drop)



subset_by <- function(data, formula, subset, select, drop=FALSE, join=TRUE,...){
    d <- splitBy(formula, data)
    lapply(d, function(x)
        subset(x, subset=subset, select=select, drop=drop, ...)
           ) 
}
subset_by(airquality, ~Month, subset=Wind > mean(Wind))




subset_by(airquality, ~Month, subset=Wind > mean(Wind))
















subsetBy2 <- function(formula, subset, data=parent.frame(), select, drop=FALSE, join=TRUE, ...){
    dat <- splitBy(formula, data)
    cl <- match.call()
    cl
    }
x <- subsetBy2(~Month, subset=Wind > mean(Wind), data=airquality)


frm <- unlist(rhsf2list(~Month))
airquality  %>% group_by_(frm)  %>%
    subset(Wind>mean(Wind))

x <- subsetBy(~Month, subset=Wind > mean(Wind), data=airquality)





    
















#' ### splitBy
#'

dat <- dietox
frm <- ~Evit+Cu
frm2 <- c("Evit","Cu")
splitBy(formula = frm, data = dietox)
splitBy(formula = frm2, data = dietox)

frm <- ~Month
frm2 <- "Month"
splitBy(frm, data=airquality)
splitBy(frm2, data=airquality)

group_by(airquality, "Month") %>%
    do(vals=data.frame(.))  %>%
    select(vals) %>%
    lapply(function(x) {(x)})

dat <- airquality
frm <- ~Month
frm2 <- "Month"

d1 <- splitBy(frm, dat)
d2 <- plyr::dlply(dat, frm, identity)
d3 <- plyr::dlply(dat, frm2, identity)

airquality$Month <- factor(airquality$Month)
airquality <- subset(airquality, Month %in% 5:7)

d1 <- splitBy(frm, dat)
d2 <- plyr::dlply(dat, frm, identity)
d3 <- plyr::dlply(dat, frm2, identity)


dat <- milkman
frm <- "cowlact"
frm <- c("cowno", "lactno")
d1 <- splitBy(frm, dat)
d2 <- splitBy2(frm, dat)


library(microbenchmark)
microbenchmark(
    splitBy2(frm, data=dat),
    splitBy(frm, data=dat),
    times=10
)

#' ### orderBy

dat <- airquality
frm <- ~ -Day + Month
d1 <- orderBy(frm, dat)
d2 <- orderBy2(frm, dat)
head(d1); head(d2)




dietox <- subset(dietox, Time==12)

sampleBy(formula = ~ Evit + Cu, frac=.1, data = dietox)


#' ### scaleBy


load_all("doBy")



dat <- milkman
frm <- list(".", c("cowno", "lactno"))
frm <- list(".", "cowlact")
load_all("doBy")
#dat <- iris
#frm <- . ~ Species
d1 <- scaleBy(frm, data=dat)
head(d1)
d2 <- scaleByOLD(frm, data=dat)
head(d2)

microbenchmark::microbenchmark(
                    scaleBy(frm, data=dat),
                    scale_by(dat, frm),
                    scaleByOLD(frm, data=dat),
                    times=10
)



mf <- scaleBy(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species,     
             data=iris)
mf

mf <- scaleBy2(. ~ Species,  
             data=iris)

scale_by(iris, . ~ Species)


mf <- scaleBy2(cbind(mpg, qsec, mpg/qsec) ~ cyl + am, data=mtcars)

mf[-1]
mf[1]

names(mf[1])


cn <- colnames(mf[[1]])
dn <- paste("V",seq_along(cn), sep="")
dn[nchar(cn)>0] <- cn
dn

dat <- mf[[1]]
colnames(dat) <- dn

z <- split(as.data.frame(dat), mf[-1])
out <- lapply(z, scale)
out



names(mf[1])
names(mf[-1])

scale(mf[1L])





    
##     return(mf)    
##     if (is.matrix(mf[[1L]])) {
##         lhs <- as.data.frame(mf[[1L]])
##         aggregate.data.frame(lhs, mf[-1L], FUN = FUN, ...)
##     }
##     else aggregate.data.frame(mf[1L], mf[-1L], FUN = FUN, ...)
## 


















#' ### To vignette
#'

library(doBy)

myfun1 <- function(x){c(m=mean(x), v=var(x), n=length(x))}

library(dplyr)
CO2 %>% group_by(Plant)
CO2 %>% group_by(Plant)  %>% summarize(mean(conc), mean(uptake), var(conc), var(uptake))

dat <- CO2
frm <- conc + uptake ~ Plant
load_all("doBy")
zzz <- doBy:::.get_variables(formula=frm, data=dat, id=NULL, debug.info=F) ## ; str(zzz)

frm <- cbind(conc, uptake) ~ Plant
load_all("doBy")
zzz <- doBy:::.get_variables(formula=frm, data=dat, id=NULL, debug.info=F) ## ; str(zzz)
zzz


sb <- function (formula, data, FUN, ..., subset, na.action = na.omit) 
{
    if (missing(formula) || !inherits(formula, "formula")) 
        stop("'formula' missing or incorrect")
    if (length(formula) != 3L) 
        stop("'formula' must have both left and right hand sides")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    m$... <- m$FUN <- NULL
    m[[1L]] <- quote(stats::model.frame)
    if (formula[[2L]] == ".") {
        cat("We are here \n")
        rhs <- as.list(attr(terms(formula[-2L]), "variables")[-1])
        lhs <- as.call(c(quote(cbind), setdiff(lapply(names(data), 
            as.name), rhs)))
        formula[[2L]] <- lhs
        m[[2L]] <- formula
    }
    mf <- eval(m, parent.frame())
    str(mf)
    mf <<- mf
    if (is.matrix(mf[[1L]])) {
        lhs <- as.data.frame(mf[[1L]])
        aggregate.data.frame(lhs, mf[-1L], FUN = FUN, ...)
    }
    else aggregate.data.frame(mf[1L], mf[-1L], FUN = FUN, ...)
}
CO2$y <- rnorm(nrow(CO2))
CO2$Plant <- NULL
dat <- CO2
frm <- cbind(uptake, conc) ~ Type + .
a<-sb(frm, data=dat, FUN=myfun1)







summaryBy(uptake + conc ~ Plant + . , data=dat, FUN=myfun1)













aggregate(cbind(conc, uptake) ~ Plant, data=CO2, FUN=myfun1)

summaryBy(conc + uptake ~ Plant, data=CO2, FUN=myfun1)

summaryBy(conc + uptake ~ Plant + . , data=CO2, FUN=myfun1)











CO2 %>% group_by() %>% summarize(mean(conc), mean(uptake), var(conc), var(uptake))

CO2 %>% group_by() %>% summarize_each(funs(mean, sd), conc, uptake)

CO2 %>% group_by() %>% summarize_each(funs(myfun1), conc, uptake)




#' Virker ikke.....
CO2 %>% group_by(Plant)  %>% do( myfun1(conc) )
CO2 %>% group_by(Plant)  %>% summarize( myfun1(conc) )



