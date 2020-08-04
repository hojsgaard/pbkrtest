#' ## Compare Kenward-Roger and Satterthwaite
#' ### Søren Højsgaard
#' 
#' Want to show/illstrate: KR fails does not scale to long individual
#' time series, but SAT does not.

library(pbkrtest)
library(microbenchmark)
library(lme4)
library(ggplot2)

#' ## Global variables
TIMES = 10
FILENAME = "sim-data-pb-sat.RData"

#' ## Simulate random regression design.
#' Two groups; NS subjects per group; NT time pointes per subject.
rr_design <- function(NS=10, NT=200, b0=1, b1=1){
    ##NS <-  10 ## Number of subjects
    ##NT <- 200 ## Number of time points

    d0 <- expand.grid(time=(1:NT), subject=factor(1:NS), trt=factor(c("a", "b")))
    d0 <- transform(d0, st=paste0(subject, "x", trt))
    
    d1 <- expand.grid(subject=factor(1:NS), trt=factor(c("a", "b")))
    d1 <- transform(d1, st=paste0(subject, "x", trt))
    
    d1$intercept <- rnorm(nrow(d1), mean=1)
    d1$slope     <- rnorm(nrow(d1), mean=1)
    
    d0 <- merge(d1, d0)
    d0 <- transform(d0, y = b0 * (trt=="a") + b1 * (trt=="a") * time +
                            intercept + slope * time + rnorm(nrow(d0)))
    d0
}

#' ## Want to show/illstrate:
#' KR fails does not scale to long individual time series, but SAT does.

d_list1 <- list(
    rr_design(NS=10, NT=8),
    rr_design(NS=10, NT=16),
    rr_design(NS=10, NT=32),
    rr_design(NS=10, NT=64),
    rr_design(NS=10, NT=128),
    rr_design(NS=10, NT=256))

d_list2 <- list( 
    rr_design(NS=8, NT=8),
    rr_design(NS=16, NT=8),
    rr_design(NS=32, NT=8),
    rr_design(NS=64, NT=8),
    rr_design(NS=128, NT=8),
    rr_design(NS=256, NT=8))

form <- y ~ trt + time + (time|st)
L    <- matrix(c(0, 0, 1), nrow=1)

#' ## The time consuming parts:
mb1 <- microbenchmark(
    KRmodcomp(lmer(form, data=d_list1[[1]]), L),
    KRmodcomp(lmer(form, data=d_list1[[2]]), L),
    KRmodcomp(lmer(form, data=d_list1[[3]]), L),
    KRmodcomp(lmer(form, data=d_list1[[4]]), L),
    KRmodcomp(lmer(form, data=d_list1[[5]]), L),
    KRmodcomp(lmer(form, data=d_list1[[6]]), L),
    SATmodcomp(lmer(form, data=d_list1[[1]]), L),
    SATmodcomp(lmer(form, data=d_list1[[2]]), L),
    SATmodcomp(lmer(form, data=d_list1[[3]]), L),
    SATmodcomp(lmer(form, data=d_list1[[4]]), L),
    SATmodcomp(lmer(form, data=d_list1[[5]]), L),
    SATmodcomp(lmer(form, data=d_list1[[6]]), L),
    times=TIMES
)

mb12 <- as.data.frame(summary(mb1))

mb2 <- microbenchmark(
    KRmodcomp(lmer(form, data=d_list2[[1]]), L),
    KRmodcomp(lmer(form, data=d_list2[[2]]), L),
    KRmodcomp(lmer(form, data=d_list2[[3]]), L),
    KRmodcomp(lmer(form, data=d_list2[[4]]), L),
    KRmodcomp(lmer(form, data=d_list2[[5]]), L),
    KRmodcomp(lmer(form, data=d_list2[[6]]), L),
    SATmodcomp(lmer(form, data=d_list2[[1]]), L),
    SATmodcomp(lmer(form, data=d_list2[[2]]), L),
    SATmodcomp(lmer(form, data=d_list2[[3]]), L),
    SATmodcomp(lmer(form, data=d_list2[[4]]), L),
    SATmodcomp(lmer(form, data=d_list2[[5]]), L),
    SATmodcomp(lmer(form, data=d_list2[[6]]), L),    
    times=TIMES
)

mb22 <- as.data.frame(summary(mb2))



#' ## Make nice data frames
#' 
NT1 <- 2^(2 + (1:(nrow(mb12)/2)))
ss1 <- formatC(NT1, width=3, format="d", flag="0")

mb12$NT   <- rep(NT1, 2)
mb12$meth <- c(rep("KR", length(NT1)), rep("SAT", length(NT1)))
mb12$expr <- NULL

NS2 <- 2^(2 + (1:(nrow(mb22)/2)))
ss2 <- formatC(NS2, width=3, format="d", flag="0")

mb22$NS   <- rep(NS2, 2)
mb22$meth <- c(rep("KR", length(NS2)), rep("SAT", length(NS2)))
mb22$expr <- NULL

#' save the lot
save(mb12, mb22, file=FILENAME)





