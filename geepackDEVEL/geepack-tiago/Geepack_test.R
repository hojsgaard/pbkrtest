
# Dependencies
library(geepack)
library(readr)

# Working directory
#setwd("...")

data_ <- read_csv("data.csv")

hip_1 <- geeglm(data = na.omit(data_),
                formula = vi ~ vd + cov,
                family = poisson,
                id = factor(id),
                waves = wave,
                corstr="independence")
summary(hip_1)








data$id %>% unique  %>% length
data %>% nrow
data$id %>% table


dat <- na.omit(data)
dat2 <- doBy::orderBy(~id+wave, data=dat)
ones <- which(table(dat2$id) == 1)

dat3 <- subset(dat2, !(id %in% names(ones)))


dat4 <- dat3[1:100,]

factor(dat4$id)

# Uploading dataset




# Fit model for hypothesis 1

hip_1 <- geeglm(data = na.omit(data),     # I have also tried without "na.omit()"
                formula = vi ~ vd + cov,
                family = poisson,
                id = factor(id),
                waves = wave,
                corstr="unstructured")

# Get results for model
summary(hip_1)
