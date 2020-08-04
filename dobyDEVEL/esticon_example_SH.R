install_github("hojsgaard/doBy", force=T)
library(nlme)
library(doBy)
library(readxl)
library(dplyr)
toenail <- read_xlsx("Toenail.xlsx")
toenail$month <- case_when(
  toenail$time == 0 ~ 1,
  toenail$time == 1 ~ 2,
  toenail$time == 2 ~ 3,
  toenail$time == 3 ~ 4,
  toenail$time == 6 ~ 5,
  toenail$time == 9 ~ 6,
  toenail$time == 12 ~ 7
)

model <- gls(response ~ factor(treat)*factor(time), corr = corCompSymm( , form= ~ month | id), data=toenail)

summary(model)

comb <- c(0, 0, 0, 0, 0, 0, 0, 0, 1/6, 1/6, 1/6, 1/6, 1/6, 1/6)

lincom <- esticon(model, comb, conf.int=TRUE, level=0.95, joint.test=FALSE)


summary(lincom)
