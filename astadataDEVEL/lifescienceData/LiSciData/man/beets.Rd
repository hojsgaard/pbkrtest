\name{beets}
\alias{beets}
\docType{data}
\title{Sugar beets data}
\description{
Yield of sugar beets as function of sowing and harvesting time in a
split--plot experiment.
}
\usage{data(beets)}
\format{
  A data frame with 30 observations on the following 5 variables.
  \describe{
    \item{harvest}{Harvest time. A factor with levels \code{harv1} \code{harv2}. The
      corresponding dates are 2/10 and 21/10.}
    \item{block}{The blocks, see details below.}
    \item{sow}{Sowing time. A factor with levels \code{sow1} \code{sow2} \code{sow3}
      \code{sow4} \code{sow5}. The corresponding dates are 4/4, 12/4,
      21/4, 29/4 and 18/5 }
    \item{yield}{Yield in Kg per 25 m2}
    \item{sugpct}{Sugar percentage}
  }
}
\details{
 The experiment was laid out as a as split plot experiment in
 blocks. There are three blocks each containing two whole--plot units
 corresponding to the harvest time. Each whole--plot unit is
 sub--divided into 5 split--plot units corresponding to the sowing dates.
}
\source{
  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
  ~~ possibly secondary sources and usages ~~
}
\examples{
data(beets)
## maybe str(beets) ; plot(beets) ...
}
\keyword{datasets}
