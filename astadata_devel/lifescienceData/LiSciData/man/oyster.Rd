\name{oyster}
\alias{oyster}
\docType{data}
\title{Oyster data}
\description{
  Oysters at three different locations (in the cooling water)
  near a power plant in Denmark. Start weight and end weight is recorded.
}
\usage{data(oyster)}
\format{
  A data frame with 20 observations on the following 3 variables.
  \describe{
    \item{treat}{a factor with levels \code{bay} \code{drain} \code{entrance}}
    \item{startwgt}{a numeric vector}
    \item{endwgt}{a numeric vector}
  }
}
%\details{
%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(oyster)
plot(endwgt~startwgt, data=oyster, col=as.numeric(treat))
## maybe str(oyster) ; plot(oyster) ...
}
\keyword{datasets}
