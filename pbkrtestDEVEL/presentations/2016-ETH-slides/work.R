library(gRim)
data(lizard)

lizard[1,1,1]<-lizard[1,1,2] <- 400

lizard
m <- dmod( ~ diam:height + diam:species + height:species, data=lizard)
m

glm(

