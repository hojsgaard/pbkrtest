\name{LSmeans}
\alias{LSmeans}
\alias{LSmeans.glm}
\alias{LSmeans.lm}
\alias{LSmeans.geeglm}
\alias{LSmeans.lmerMod}
\alias{LSmeans.merMod}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute linear estimates
}
\description{
Compute linear estimates for a range of models. One example of linear
estimates is population means (also known as LSMEANS).
}
\usage{
LSmeans(object, effect = NULL, at = NULL, K = NULL, level=0.95, 
      %only.at = TRUE, grid = TRUE,
      %df.adjust = FALSE,
      ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    Model object
  }
  \item{effect}{
    A vector of variables. For each configuration of these the estimate
    will be calculated.
  }
  \item{at}{
    A list of values of covariates (including levels of some factors)
    to be used in the calculations
  }
  \item{K}{
    A contrast matrix. If given then the specifications in \code{effect}
    and \code{at} are ignored. Otherwise, the matrix is generated
    according to the specifications in \code{effect}
    and \code{at}.

  }

  %% \item{only.at}{
  %%   If FALSE and \code{at} is not NULL then the contrast matrix is
  %%   formed as if \code{at} was not specified. Then the values of
  %%   \code{at} are set afterwards such that the contrast matrix will
  %%   contain several identical rows.
  %% }
  %% \item{grid}{
  %%   If \code{TRUE} and \code{engine='esticon'} then the values of the
  %%   covariates at which the population means are calculated are appended
  %%   to the result.
  %% }

  %% \item{df.adjust}{
  %%   If \code{object} is a \code{lmerMod} object, the degrees of freedom
  %%   can be adjusted according by a Kenward-Roger method. For other model
  %%   objects, this argument has no effect.
  %% }

  \item{level}{
    The level of the (asymptotic) confidence interval.
  }

  \item{...}{
    Additional arguments; currently not used.
  }
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
  A dataframe with results from computing the contrasts.
  }
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
\note{
  The \code{LSmeans} method is a recent addition to the package, and it
  will eventually replace the \code{popMeans} method.

  Please report unexpected behaviour.

  Some of the code has been adapted from the \bold{lsmeans} package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{linestMatrix}}
}
\examples{

## Make balanced dataset
dat.bal <- expand.grid(list(AA=factor(1:2), BB=factor(1:3), CC=factor(1:3)))
dat.bal$y <- rnorm(nrow(dat.bal))

## Make unbalanced dataset
#   'BB' is nested within 'CC' so BB=1 is only found when CC=1
#   and BB=2,3 are found in each CC=2,3,4
dat.nst <- dat.bal
dat.nst$CC <-factor(c(1,1,2,2,2,2,1,1,3,3,3,3,1,1,4,4,4,4))

mod.bal  <- lm(y ~ AA + BB*CC,    data=dat.bal)
mod.nst  <- lm(y ~ AA + BB : CC, data=dat.nst)

LSmeans(mod.bal, effect=c("BB", "CC"))
LSmeans(mod.nst, effect=c("BB", "CC"))

LSmeans(mod.nst, at=list(BB=1, CC=1))

LSmeans(mod.nst, at=list(BB=1, CC=2))
## Above: NA's are correct; not an estimable function


if( require( lme4 )){
warp.mm <- lmer(breaks ~ -1 + tension + (1|wool), data=warpbreaks)
class(warp.mm)
fixef(warp.mm)
coef(summary(warp.mm))
vcov(warp.mm)
if (require(pbkrtest ))
vcovAdj(warp.mm)
}



%% LSmeans(warp.mm, effect="tension")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

