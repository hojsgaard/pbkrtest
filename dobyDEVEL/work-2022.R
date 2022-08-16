crime_rate <- doBy::crimeRate
rownames(crime_rate) <- crime_rate$State
crime_rate$State <- NULL
save(crime_rate, file="crime_rate.RData")


nir_milk <- doBy::NIRmilk
nir_milk$sample <- NULL

y <- nir_milk[,c("fat", "protein", "lactose", "dm")]
x <- nir_milk[,-match(c("fat", "protein", "lactose", "dm"), names(nir_milk))]

nir_milk <- list(x=x, y=y)
save(nir_milk, file="nir_milk.RData")














library(tibble)
library(doBy)
load_all()

x <- iris
y <- 10+(1:3)
names(y) <- letters[1:3]

x %>% sqb(1)
x %>% sqb(1:2)

x[[1]]

g"["(x, 2)



"["(x, "b")



gt(iris, "Sepal.Length")
gt(iris, 2)

gt(x, "a")

gt(x, 2)
