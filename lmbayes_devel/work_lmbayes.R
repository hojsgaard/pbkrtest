library(ggplot2)
## library(devtools)
library(dplyr)
library(lmbayes)
load_all("_lmbayes")

## Just checking
form <- dist ~ speed + I(speed^2)
lm1 <- lm(form, data=cars)
coef(summary(lm1))
sigma(lm1)


## Bayesian update
prior <- list(m=c(1,1,1), C=diag(50, 3), sd=100)
blm2 <- bayes_lm(form, data=cars, prior=prior)
blm2

blm2 |> tidy()
coef(blm2)
sigma(blm2)

pp <- cars |> ggplot(aes(speed, dist))
pp + geom_line(aes(x=speed, y=predict(blm2)), col="red") +
    geom_line(aes(x=speed, y=predict(lm1)), col="blue")
    
idx <- sample(1:nrow(cars), size=nrow(cars)/2)

cars1 <- cars[idx,]
cars2 <- cars[-idx,]

bb1 <- bayes_lm(form, data=cars1, prior=prior)
bb2 <- bayes_lm(form, data=cars2, prior=prior)



##' NILE data

y <- as.numeric(Nile)
asw <- factor(c(rep(0, 30), rep(1, length(y)-30)))
tvar <- (1871:1970)-1871
dat <- data.frame(y, asw, tvar) |> as_tibble()
dat |> head()
dat |> tail()
dat |> ggplot(aes(tvar, y)) + geom_point()


## Want to fit models to increasingly larger sets of data:

tt1 <- 35
tt2 <- 50
dat2 <- dat |> filter(tvar < tt1)
dat3 <- dat |> filter(tvar >= tt1 & tvar < tt2)
dat4 <- dat |> filter(tvar < tt2)

form <- y ~ asw
dat2

lm1 <- lm(y~asw, data=dat)
lm1 |> sigma()
lm1 |> vcov()

prior <- list(m=c(1000, 0), C=diag(500, 2), sd=100)
fit1 <- bayes_lm(form, data=dat2, prior=prior)
fit1

dat_ <- cbind(dat, predint(fit1, newdata=dat))
pl <- dat_ |> ggplot(aes(tvar, y)) +
    geom_point() +
    geom_line(aes(y=pred), col="red") +
    geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2,
                  alpha=.3,
                  position=position_dodge(0.05)) +
    geom_vline(xintercept = tt1)
pl







## MILKMAN

library(ggplot2)
data(milkman, package="doBy")
cl <- milkman$cowlact |> unique()
many <- subset(milkman, subset=cowlact %in% cl[1:10])
many <- many[!is.na(many$my), ]

dd <-

one  <- subset(milkman, subset=cowlact=="0435.2" & ampm==1)
two  <- subset(milkman, subset=cowlact=="8110.1" & ampm==1)

pl1 <- one |> ggplot(aes(x=dfc, y=my)) + geom_point()
pl1

pl2 <- two |> ggplot(aes(x=dfc, y=my)) + geom_point()
pl2


rr <- many |>
    nest_by(cowlact)  |>
    reframe(fit = list(lm(log(my) ~ log(dfc) + dfc, data=data)))

rr <- many |>
    nest_by(cowlact)  |>
    summarise(fit = list(lm(log(my) ~ log(dfc) + dfc, data=data)))

iris |> doBy::splitBy(~Species)


x <- iris |>
    nest_by(Species)  |>
    reframe(fit = list(lm(Sepal.Width~., data=data)))

cc <- x |> group_by(Species)  |> summarise(b=coef(fit))
cc


library(doBy)
dd <- iris |> split_by(~Species)


dd <- lapply(dd, function(d){d$Species <- NULL; d})
dd[[1]]

dd |> lapply(function(.d) {lm(Sepal.Width~., data=.d)})


dd |> lapply(function(.d) {lm(Sepal.Width~. - Species, data=.d)})
             








mm <- round(colMeans(rr[,-1]), 6)
VV <- round(cov(rr[,-1]), 6)

one <- subset(milkman, subset=cowlact=="0435.2" & ampm==1)
one <- one[!is.na(one$my), ]

head(one)
dim(one)

data <- one[1:200,]
formula <- log(my) ~ log(dfc) + dfc

b0 <- c(2.5, .15, -0.003)

V0 <- .01 # diag(1, 3)
W  <- .1  # diag(.1, NROW(data))

aa <- bayes_lm(formula, data, b0, V0, W)
aa

pp <- qplot(dfc, log(my), data=one, ylim=c(1.5, 3.5))
pp


pint <- predint(aa, newdata=one)


pp + geom_line(aes(x=dfc, y=pint$pred), data=one) +
    geom_line(aes(x=dfc, y=pint$lwr), data=one, color="red") + 
    geom_line(aes(x=dfc, y=pint$upr), data=one, color="red")


object=aa
ss <- simulate(object, nsim=10)

level=0.95
pp <- c((1-level) / 2, 1-(1-level)/2)


lapply(split(ss, row(ss)), function(r) quantile(r, pp))



nsim <- 10
bb <- MASS::mvrnorm(n=nsim, mu=coef(aa), Sigma=vcov(aa))

bi <- bb[1,]

model.matrix(aa) %*% bi


y <- as.numeric(Nile)
a <- factor(c(rep(0, 30), rep(1, length(y)-30)))
tvar <- (1871:1970)-1871

dat <- data.frame(y, a, tvar)
dat

form <- y ~ a
lm(form, data=dat)

bb <- bayes_lm(form, data=dat[1:40,], m=c(1, 1), C=diag(10,2), phi=2)
bb

predint(bb, newdata=dat)

dat$pred <- predict(bb, newdata=dat)


dat |> ggplot(aes(tvar, y)) + 
  geom_point() +
  geom_line(aes(y=pred), col="red") 




  
  






















