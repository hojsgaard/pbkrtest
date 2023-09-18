\name{powerBy}
\alias{powerBy}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Make plot of log variance against log mean for grouped data}
\description{
  Needs to be documented
}
\usage{
powerBy(formula = ~., data = parent.frame())
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{formula}{ ~~Describe \code{formula} here~~ }
  \item{data}{ ~~Describe \code{data} here~~ }
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
A data frame
}
\author{ Søren Højsgaard, sorenh@agrsci.dk}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{ SEE ALSO
  \code{\link{histBy}},
  \code{\link{qqnormBy}},
  \code{\link{summaryBy}}
  % \code{\link{plotBy}}
}


\examples{
data(dietox)
#dietox12    <- subset(dietox,dietox$Time==12)
#par(mfrow=c(2,1),omi=c(0,0,.5,0))
#powerBy(cbind(Weight,Feed)~Evit+Cu, data=dietox12)  
#mtext("Groupwise plot of log variance against log mean",outer=TRUE)
}
\keyword{univar}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
