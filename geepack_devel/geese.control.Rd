\name{geese.control}
\alias{geese.control}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Auxiliary for Controlling GEE Fitting}
\description{
  Auxiliary function as user interface for `gee' fitting. Only used when
  calling `geese' or `geese.fit'.
}
\usage{
geese.control(epsilon = 1e-04, maxit = 25, trace = FALSE,
              scale.fix = FALSE, jack = FALSE, j1s = FALSE, fij = FALSE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{epsilon}{positive convergence tolerance epsilon; the iterations
    converge when the absolute value of the difference in parameter
    estimate is below \code{epsilon}.}
  \item{maxit}{integer giving the maximal number of Fisher Scoring iteration.}
  \item{trace}{logical indicating if output should be produced for each iteration.}
  \item{scale.fix}{logical indicating if the scale should be fixed.}
  \item{jack}{logical indicating if approximate jackknife variance
    estimate should be computed.}
  \item{j1s}{logical indicating if 1-step jackknife variance estimate
    should be computed.}
  \item{fij}{logical indicating if fully iterated jackknife variance
    estimate should be computed.}
}
\details{
  When `trace' is true, output for each iteration is printed to the
  screen by the c++ code. Hence, `options(digits = *)' does not control
  the precision.  
}
\value{
  A list with the arguments as components.
}
\author{Jun Yan \email{jyan.stat@gmail.com} }
\seealso{`geese.fit', the fitting procedure used by `geese'.}
% \examples{
% }
\keyword{optimize}
\keyword{models}
