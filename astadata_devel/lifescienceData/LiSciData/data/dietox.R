dietox <- read.table("dietox.txt",header=TRUE)
dietox$Evit <- factor(dietox$Evit);
dietox$Cu <- factor(dietox$Cu);
dietox$Litter <- factor(dietox$Litter)
dietox$Pig <- factor(dietox$Pig)
