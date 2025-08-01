% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PB_modcomp.R
\name{pb__modcomp}
\alias{pb__modcomp}
\alias{PBmodcomp}
\alias{PBmodcomp.lm}
\alias{PBmodcomp.merMod}
\alias{plot.XXmodcomp}
\alias{PBmodcomp.mer}
\alias{getLRT.mer}
\alias{PBmodcomp.gls}
\alias{seqPBmodcomp}
\title{Model comparison using parametric bootstrap methods.}
\usage{
PBmodcomp(
  largeModel,
  smallModel,
  nsim = 1000,
  ref = NULL,
  seed = NULL,
  cl = NULL,
  details = 0
)

\method{PBmodcomp}{merMod}(
  largeModel,
  smallModel,
  nsim = 1000,
  ref = NULL,
  seed = NULL,
  cl = NULL,
  details = 0
)

\method{PBmodcomp}{lm}(
  largeModel,
  smallModel,
  nsim = 1000,
  ref = NULL,
  seed = NULL,
  cl = NULL,
  details = 0
)

\method{PBmodcomp}{gls}(
  largeModel,
  smallModel,
  nsim = 1000,
  ref = NULL,
  seed = NULL,
  cl = NULL,
  details = 0
)

seqPBmodcomp(largeModel, smallModel, h = 20, nsim = 1000, cl = 1)
}
\arguments{
\item{largeModel, smallModel}{Two models}

\item{nsim}{The number of simulations to form the reference
distribution.}

\item{ref}{Vector containing samples from the reference
distribution. If NULL, this vector will be generated using
\code{PBrefdist()}.}

\item{seed}{A seed that will be passed to the simulation of new
datasets.}

\item{cl}{A vector identifying a cluster; used for calculating the
reference distribution using several cores. See examples below.}

\item{details}{The amount of output produced. Mainly relevant for
debugging purposes.}

\item{h}{For sequential computing for bootstrap p-values: The
number of extreme cases needed to generate before the sampling
process stops.}
}
\description{
Model comparison of nested models using parametric bootstrap
methods.  Implemented for some commonly applied model types.
}
\details{
The model \code{object} must be fitted with maximum likelihood
(i.e. with \code{REML=FALSE}). If the object is fitted with
restricted maximum likelihood (i.e. with \code{REML=TRUE}) then
the model is refitted with \code{REML=FALSE} before the
p-values are calculated. Put differently, the user needs not
worry about this issue.

Under the fitted hypothesis (i.e. under the fitted small model) \code{nsim}
samples of the likelihood ratio test statistic (LRT) are generated.

Then p-values are calculated as follows:

LRT: Assuming that LRT has a chi-square distribution.

PBtest: The fraction of simulated LRT-values that are larger or equal to the
observed LRT value.

Bartlett: A Bartlett correction is of LRT is calculated from the mean of the
simulated LRT-values

Gamma: The reference distribution of LRT is assumed to be a gamma
distribution with mean and variance determined as the sample mean and sample
variance of the simulated LRT-values.

F: The LRT divided by the number of degrees of freedom is assumed to be
F-distributed, where the denominator degrees of freedom are determined by
matching the first moment of the reference distribution.
}
\note{
It can happen that some values of the LRT statistic in the
reference distribution are negative. When this happens one will
see that the number of used samples (those where the LRT is
positive) are reported (this number is smaller than the
requested number of samples).

In theory one can not have a negative value of the LRT statistic but in
practice on can: We speculate that the reason is as follows: We simulate data
under the small model and fit both the small and the large model to the
simulated data. Therefore the large model represents - by definition - an
over fit; the model has superfluous parameters in it. Therefore the fit of the
two models will for some simulated datasets be very similar resulting in
similar values of the log-likelihood. There is no guarantee that the the
log-likelihood for the large model in practice always will be larger than for
the small (convergence problems and other numerical issues can play a role
here).

To look further into the problem, one can use the \code{PBrefdist()} function
for simulating the reference distribution (this reference distribution can be
provided as input to \code{PBmodcomp()}). Inspection sometimes reveals that
while many values are negative, they are numerically very small. In this case
one may try to replace the negative values by a small positive value and then
invoke \code{PBmodcomp()} to get some idea about how strong influence there
is on the resulting p-values. (The p-values get smaller this way compared to
the case when only the originally positive values are used).
}
\examples{

\dontrun{ 
(fm0 <- lmer(Reaction ~ (Days|Subject), sleepstudy))
(fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
(fm2 <- lmer(Reaction ~ Days + I(Days^2) + (Days|Subject), sleepstudy))

NSIM <- 50 ## Simulations in parametric bootstrap; default is 1000.

## Test for no effect of Days in fm1, i.e. test fm0 under fm1
PBmodcomp(fm1, "Days", cl=1, nsim=NSIM)
PBmodcomp(fm1, ~.-Days, cl=1, nsim=NSIM)
L1 <- cbind(0, 1)
PBmodcomp(fm1, L1, cl=1, nsim=NSIM) 
PBmodcomp(fm1, fm0, cl=1, nsim=NSIM)
anova(fm1, fm0)

## Test for no effect of Days and Days-squared in fm2, i.e. test fm0 under fm2
PBmodcomp(fm2, "(Days+I(Days^2))", cl=1, nsim=NSIM)
PBmodcomp(fm2, ~. - Days - I(Days^2), cl=1, nsim=NSIM)
L2 <- rbind(c(0, 1, 0), c(0, 0, 1))
PBmodcomp(fm2, L2, cl=1, nsim=NSIM) ## FIXME

PBmodcomp(fm2, fm0, cl=1, nsim=NSIM)
anova(fm2, fm0)

## Test for no effect of Days-squared in fm2, i.e. test fm1 under fm2
PBmodcomp(fm2, "I(Days^2)", cl=1, nsim=NSIM)
PBmodcomp(fm2, ~. - I(Days^2), cl=1, nsim=NSIM)
L3 <- rbind(c(0, 0, 1))
PBmodcomp(fm2, L3, cl=1, nsim=NSIM) 
PBmodcomp(fm2, fm1, cl=1, nsim=NSIM)
anova(fm2, fm1)

## Linear normal model:
sug <- lm(sugpct ~ block + sow + harvest, data=beets)
sug.h <- update(sug, .~. -harvest)
sug.s <- update(sug, .~. -sow)

PBmodcomp(sug, "harvest", nsim=NSIM, cl=1)
PBmodcomp(sug, ~. - harvest, nsim=NSIM, cl=1)
PBmodcomp(sug, sug.h, nsim=NSIM, cl=1)
anova(sug, sug.h)

## Generalized linear model
mm <- glm(ndead/ntotal ~ sex + log(dose), family=binomial, weight=ntotal, data=budworm)
mm0 <- update(mm, .~. -sex)

### Test for no effect of sex
PBmodcomp(mm, "sex", cl=1, nsim=NSIM)
PBmodcomp(mm, ~.-sex, cl=1, nsim=NSIM)
## PBmodcomp(mm, cbind(0, 1, 0), nsim=NSIM): FIXME
PBmodcomp(mm, mm0, cl=1, nsim=NSIM)
anova(mm, mm0, test="Chisq")
}

## Generalized linear mixed model (it takes a while to fit these)

\dontrun{
(gm1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
              data = cbpp, family = binomial))
(gm2 <- update(gm1, .~.-period))

PBmodcomp(gm1, "period", nsim=NSIM)
PBmodcomp(gm1, ~. -period, nsim=NSIM)
PBmodcomp(gm1, gm2, nsim=NSIM)
anova(gm1, gm2)
}

\dontrun{
## Linear mixed effects model:
sug   <- lmer(sugpct ~ block + sow + harvest + (1|block:harvest),
              data=beets, REML=FALSE)
sug.h <- update(sug, .~. -harvest)
sug.s <- update(sug, .~. -sow)

anova(sug, sug.h)
PBmodcomp(sug, sug.h, nsim=NSIM, cl=1)
PBmodcomp(sug, "harvest", nsim=NSIM, cl=1)

anova(sug, sug.s)
PBmodcomp(sug, sug.s, nsim=NSIM, cl=1)
PBmodcomp(sug, "sow", nsim=NSIM, cl=1)

## Simulate reference distribution separately:
refdist <- PBrefdist(sug, sug.h, nsim=1000, cl=1)
refdist <- PBrefdist(sug, "harvest", nsim=1000, cl=1)
refdist <- PBrefdist(sug, ~.-harvest, nsim=1000, cl=1)

## Do computations with multiple processors:
## Number of cores:

(nc <- detectCores())
## Create clusters
cl <- makeCluster(rep("localhost", nc))

## Then do:
refdist <- PBrefdist(sug, sug.h, nsim=1000, cl=cl)

## It is recommended to stop the clusters before quitting R:
stopCluster(cl)
}

lm1 <- lm(dist~speed+I(speed^2), data=cars)
PBmodcomp(lm1, .~.-speed, cl=2)
PBmodcomp(lm1, .~.-I(speed^2), cl=2)


}
\references{
Ulrich Halekoh, Søren Højsgaard (2014)., A Kenward-Roger
Approximation and Parametric Bootstrap Methods for Tests in Linear Mixed
Models - The R Package pbkrtest., Journal of Statistical Software,
58(10), 1-30., \url{https://www.jstatsoft.org/v59/i09/}
}
\seealso{
\code{\link{KRmodcomp}}, \code{\link{PBrefdist}}
}
\author{
Søren Højsgaard \email{sorenh@math.aau.dk}
}
\concept{model_comparison}
\keyword{inference}
\keyword{models}
