\name{popMeans}
\alias{popMeans}
\alias{popMeans.default}
\alias{popMeans.lme}

\alias{LSMEANS}
\alias{LSMEANS.default}
\alias{LSMEANS.lme}

\alias{summary.conMeans}

%\alias{print.lsmeansTable}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculate population means (LSMEANS in SAS jargon)
}
\description{
  Calculate population means (LSMEANS in SAS jargon). The functions
  \code{popMeans} and \code{LSMEANS} are synonymous.
}
\usage{
popMeans(object, effect, at = NULL, only.at=TRUE, engine=esticon,grid=TRUE,...)
\method{popMeans}{default}(object, effect, at = NULL, only.at=TRUE, engine=esticon,grid=TRUE,...)
\method{popMeans}{lme}(object, effect, at = NULL, only.at=TRUE, engine=esticon,grid=TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    A model object.
  }
  \item{effect}{
    A vector of variables for which the population means should be calculated.
  }
  \item{at}{
    A list of covariate values
  }
  \item{only.at}{
    If FALSE and \code{at} is not NULL then the contrast matrix is
    formed as if \code{at} was not specified. Then the values of
    \code{at} are set afterwards such that the contrast matrix will
    contain several identical rows. 
  }

  \item{engine}{
    The function for actually calculating the linear combination of the
    parameters. 
    See 'details'.
  }
  \item{grid}{
    If \code{TRUE} and \code{engine='esticon'} then the values of the
    covariates at which the population means are calculated are appended
    to the result. 
  }
  
  \item{...}{Additional arguments; currently not used.}
}
\details{
  The function is \code{lsMeans()} is just an alias for
    \code{popMeans()}. 
  
  The \code{popMeans()} function generates the relevant contrast matrix
  and then calls the engine on that matrix. Another possible engine is
  the \code{glht} function  from the multcomp package
}
\value{
  A dataframe with a few additional
  attributes. 
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
%\note{
%%  ~~further notes~~
%}

\section{Warning}{
  This function is a recent addition to the 'doBy' package and it is not
  thoroughly tested. Use with caution!
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{esticon}}
  \code{\link[multcomp]{glht}}
}
\examples{
dat <- expand.grid(list(AA=factor(1:2), BB=factor(1:3), CC=factor(1:3)))
dat$y <- rnorm(nrow(dat))
dat$x <- rnorm(nrow(dat))
dat$x2 <- dat$x^2

## Examples

## 1) LSMEANS with factors only.
mod1 <- lm(y ~ AA + BB*CC + x, data=dat)
## Average over AA for each combination of (BB,CC); evaluate at x=mean(x)
popMeans(mod1, c("BB","CC"))
## Average over (AA,BB) for each value of CC; evaluate at x=mean(x)
popMeans(mod1, c("CC"))

## 2) The call to popMeans() below is equivalent to the following SAS code
## proc glm data=dat;
##	class AA BB CC;
##	model y = AA BB|CC x x*x;
##  lsmeans CC BB*CC / stderr;
## run;
##
mod2 <- lm(y ~ AA + BB*CC + x + x2, data=dat)
popMeans(mod2, "CC")

## Notice the difference to:
mod3 <- lm(y ~ AA + BB*CC + x + I(x^2), data=dat)
popMeans(mod3, "CC")

## The difference arises because in the former case, x2 is evaluated at mean(x2) whereas
## in the latter case x is evaluated at mean(x)^2

## 3) Plug in particular values of covariates
## The call to popMeans() below is equivalent to the following SAS code
## proc glm data=dat;
##	class AA BB CC;
##	model y = AA BB|CC x x2;
##  lsmeans CC BB*CC / at x=2 stderr;
## run;
popMeans(mod2, c("CC"), at=list(x=2))
## Above, x=2 is used while x2 is set to mean(x2)

## The call to popMeans() below is equivalent to the following SAS code
## proc glm data=dat;
##	class AA BB CC;
##	model y = AA BB|CC x x*x;
##    lsmeans CC BB*CC / at x=2 stderr;
## run;
##
popMeans(mod3, c("CC"), at=list(x=2))
## Above, x=2 is used while I(x^2) is set to mean(x^2)=4. Notice that setting
## popMeans(mod3, c("CC"), at=list(x=2,"I(x^2)"=123))
## has no effect: I(x^2) is still 4 because x=2. Hence the following two results
## are identical

popMeans(mod2, c("CC"), at=list(x=2, x2=4))
popMeans(mod3, c("CC"), at=list(x=2))

## END 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{utilites}% __ONLY ONE__ keyword per line
