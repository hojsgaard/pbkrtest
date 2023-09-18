\name{qqnormBy}
\alias{qqnormBy}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Function to make qqplots for grouped data}
\description{
  Function to make qqplots for grouped data
  }
\usage{
qqnormBy(x, group = NULL, lines = TRUE, title = "", data = parent.frame(),...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{ name of variable  }
  \item{group}{ vector of names defining the groups }
  \item{lines}{ if TRUE a line is added to each plot  }
  \item{title}{ Title of each qqplot, see examples below  }
  \item{data}{ data frame }
  \item{...}{ }
}
%\details{
%  ~~ If necessary, more details than the description  above ~~
%}
\value{
  Nothing
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~

%\seealso{ ~~objects to SEE ALSO as \code{\link{~~fun~~}}, ~~~ }
\seealso{
 % \code{\link{plotBy}},
  \code{\link{histBy}},
  \code{\link{summaryBy}}
}

\examples{

data(dietox)
dietox$Cu     <- as.factor(dietox$Cu)
dietox$Evit   <- as.factor(dietox$Evit)
dietox12      <- subset(dietox,dietox$Time==12)

dietox12    <- subset(dietox,dietox$Time==12)

par(mfrow=c(3,3),omi=c(0,0,.5,0))

qqnormBy(Weight, group=c(Evit,Cu), data=dietox12)
mtext("QQ-plots for Weight in week 12",outer=TRUE)
}
\keyword{ hplot}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
