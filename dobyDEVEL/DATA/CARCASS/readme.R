## April 2022

## This carcassall data set is the version that has been in doBy since
## forever and until now. The new version is with appropriate levels
## etc.


load("carcassall_orig.RData")

names(carcassall)

xtabs(~slhouse+ sex, data=carcassall)
levels(carcassall$sex) <- c("castrate", "female")

ab   <- carcassall$slhouse
size <- carcassall$slhouse

levels(ab)   <- c("slh1", "slh2", "slh2")
levels(size) <- c("normal", "normal", "large")

carcassall$slhouse <- ab
carcassall$size <- size

head(carcassall)

save(carcassall, file="carcassall.RData")

xtabs(~slhouse + sex + size, data=carcassall)
