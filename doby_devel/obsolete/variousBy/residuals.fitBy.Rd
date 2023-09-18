\name{residuals.fitBy}
\alias{residuals.fitBy}
\alias{fitted.fitBy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Residuals and fitted values for groupwise fitted models}
\description{
  Residuals and fitted values for groupwise fitted models
}
\usage{
residuals.fitBy(object, ...)
fitted.fitBy(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A fitBy object (as returned by lmBy and glmBy)}
  \item{\dots}{Additional arguments}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A vector
}
%\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{See Also as \code{\link{lmBy}}, \code{\link{glmBy}} }
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
