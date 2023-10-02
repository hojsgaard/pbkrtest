#' ## Beets split plot example
#' ### Søren Højsgaard
#' 
#' Also presented in  Halekoh, U., and Højsgaard, S. (2014) A Kenward-Roger Approximation and Parametric Bootstrap Methods for Tests in Linear Mixed Models - the R Package pbkrtest. J. Stat. Soft. Vol. 59, Issue 9
#'

library(pbkrtest)
data(beets, package="pbkrtest")

#' Because the experiment is balanced, the correct tests can be made using
#' aov():

beets$bh <- with(beets, interaction(block, harvest))
summary(aov(sugpct ~ block + sow + harvest + 
                Error(bh), data=beets))

#' Alternative; use lmer
beetLarge <- lmer(sugpct ~ block + sow + harvest + 
                      (1 | block:harvest), data=beets, REML=FALSE)
beet_no.harv <- update(beetLarge, .~. - harvest)
beet_no.sow  <- update(beetLarge, .~. - sow)

#' The effect of harvest is "too significant"; no problem with the effect of
#' sowing time compared to the aov() results above.
anova(beetLarge, beet_no.harv)
anova(beetLarge, beet_no.sow)

#' KRmodcomp remedies this
KRmodcomp(beetLarge, beet_no.harv)
KRmodcomp(beetLarge, beet_no.sow)

#' PBmodcomp remedies this
PBmodcomp(beetLarge, beet_no.harv)
PBmodcomp(beetLarge, beet_no.sow)
