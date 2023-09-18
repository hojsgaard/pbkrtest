\name{lsmean.geeglm}
\alias{lsmean.geeglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate lsmeans for geeglm objects}
\description{
Calculate lsmeans (also sometimes called population means) for geeglm
objects. Note, that to use this function you must install the 'pda'
package of Brian Yandell. See 'note' below.

}
\usage{
lsmean.geeglm(object, data = eval(object$call$data), factors, ..., rdf = 1000, coef = object$coefficients, cov = summary(object)$cov.scaled)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{geeglm object}
  \item{data}{data}
  \item{factors}{}
  \item{\dots}{}
  \item{rdf}{Arbitrarily set to 1000}
  \item{coef}{}
  \item{cov}{}
}
\details{
  For a description of the arguments see the documentation to the lsmean
  function in the 'pda' package.
}
\value{
A data frame
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{The pda package is not (at the time of
  writing, October 2004) on CRAN, but can be downloaded from
  \url{http://www.stat.wisc.edu/~yandell/pda/}
}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{geeglm}}, \code{\link{esticon}}}
\examples{

}
\keyword{utilities}% at least one, from doc/KEYWORDS

