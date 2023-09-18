\name{histBy}
\alias{histBy}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Making groupwise histograms }
\description{
  Making groupwise histograms
}
\usage{
histBy(x, group = NULL, lines = TRUE, title = "", group.col = FALSE, data = parent.frame(), smooth = FALSE,...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{ name of variable  }
  \item{group}{ vector of names defining the groups }
  \item{lines}{ if TRUE a smoothed curve is added to each histogram  }
  \item{title}{ Title of each histogram, see examples below }
  \item{group.col}{ possibly a vector of colours}
  \item{data}{ data frame}
  \item{smooth}{ degree of smoothing, a positive integer }
  \item{...}{}
}
%\details{
%  ~~ If necessary, more details than the description  above ~~
%}
\value{
  TRUE
}
\author{Søren Højsgaard, sorenh@agrsci.dk}

\seealso{
  %\code{\link{plotBy}},
  \code{\link{qqnormBy}},
  \code{\link{summaryBy}},
  %\code{\link{histBy}},
  \code{\link{doby.xtabs}},
  }
}

\examples{

data(dietox)
dietox$Cu     <- as.factor(dietox$Cu)
dietox$Evit   <- as.factor(dietox$Evit)
dietox12      <- subset(dietox,dietox$Time==12)

par(mfrow=c(3,3),omi=c(0,0,.5,0))

histBy(Weight, group=c(Evit,Cu),group.col=rep(2:4,3), data=dietox12)
mtext("Histograms for Weight in week 12",outer=TRUE)

## Works only with the ash package installed:
##histBy(Weight, group=c(Evit,Cu),group.col=rep(2:4,3), data=dietox12, smooth=15)
##mtext("Smoothed histograms for Weight in week 12",outer=TRUE)

}
\keyword{hplot}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
