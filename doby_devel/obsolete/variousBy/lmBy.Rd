\name{lmBy}
\alias{lmBy}
\alias{glmBy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fit 'lm's and 'glm's to groups of data }
\description{
  Data is split into groups and the model is fitted seperately to each
  group of data.
}
\usage{
lmBy(formula, data, subset, weights, na.action, method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, contrasts = NULL, offset, group, ...)

glmBy(formula, family = gaussian, data, weights, subset, na.action, start = NULL, etastart, mustart, offset, control = glm.control(...), model = TRUE, method = "glm.fit", x = FALSE, y = TRUE, contrasts = NULL, group, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{ ~~Describe \code{formula} here~~ }
  \item{data}{ ~~Describe \code{data} here~~ }
  \item{subset}{ ~~Describe \code{subset} here~~ }
  \item{weights}{ ~~Describe \code{weights} here~~ }
  \item{na.action}{ ~~Describe \code{na.action} here~~ }
  \item{method}{ ~~Describe \code{method} here~~ }
  \item{model}{ ~~Describe \code{model} here~~ }
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{qr}{ ~~Describe \code{qr} here~~ }
  \item{singular.ok}{ ~~Describe \code{singular.ok} here~~ }
  \item{contrasts}{ ~~Describe \code{contrasts} here~~ }
  \item{offset}{ ~~Describe \code{offset} here~~ }
  \item{group}{ ~~Describe \code{group} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }

%  \item{formula}{ ~~Describe \code{formula} here~~ }
  \item{family}{ ~~Describe \code{family} here~~ }
%  \item{data}{ ~~Describe \code{data} here~~ }
%  \item{weights}{ ~~Describe \code{weights} here~~ }
%  \item{subset}{ ~~Describe \code{subset} here~~ }
%  \item{na.action}{ ~~Describe \code{na.action} here~~ }
  \item{start}{ ~~Describe \code{start} here~~ }
  \item{etastart}{ ~~Describe \code{etastart} here~~ }
  \item{mustart}{ ~~Describe \code{mustart} here~~ }
%  \item{offset}{ ~~Describe \code{offset} here~~ }
  \item{control}{ ~~Describe \code{control} here~~ }
%  \item{model}{ ~~Describe \code{model} here~~ }
%  \item{method}{ ~~Describe \code{method} here~~ }
%  \item{x}{ ~~Describe \code{x} here~~ }
%  \item{y}{ ~~Describe \code{y} here~~ }
%  \item{contrasts}{ ~~Describe \code{contrasts} here~~ }
%  \item{group}{ ~~Describe \code{group} here~~ }
%  \item{\dots}{ ~~Describe \code{\dots} here~~ }
}
\details{
  The syntax is as for lm's and glm's with the additional keyword
  'group'. See Examples below
}
\value{
  A list of model objects
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ %
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{See also \code{\link{residuals}}, \code{\link{fitted}}}
\examples{

data(dietox)

\dontrun{
## requires the lattice package
xyplot(Weight~Time|Evit, groups=Pig, data=dietox, type='l')
}

v1<-lmBy(Weight~Time, group=~Pig,data=dietox)
v2<-glmBy(Weight~Time, group=~Pig,data=dietox,family=poisson('identity'))

f1<-fitted(v1)
r1<-residuals(v1)

f2<-fitted(v2)
r2<-residuals(v2,type='pearson')

\dontrun{
## requires the lattice package
xyplot(f1~Time|Evit, groups=Pig, data=dietox, type='l')
xyplot(r2~Time|Evit, groups=Pig, data=dietox, type='l')

xyplot(f2~Time|Evit, groups=Pig, data=dietox, type='l')
xyplot(r2~Time|Evit, groups=Pig, data=dietox, type='l')
}

}
\keyword{models}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
