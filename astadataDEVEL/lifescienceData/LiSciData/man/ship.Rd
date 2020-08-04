\name{ship}
\alias{ship}
\docType{data}
\title{Ship damage data}
\description{
  Taken from McCullog/Nelder. Can be used for Poisson model.
}
\usage{data(ship)}
\format{
  A data frame with 34 observations on the following 5 variables.
  \describe{
    \item{type}{a factor with levels \code{A} \code{B} \code{C} \code{D} \code{E}}
    \item{year}{a numeric vector}
    \item{period}{a numeric vector}
    \item{months}{a numeric vector}
    \item{y}{a numeric vector}
  }
}
% \details{
%   ~~ If necessary, more details than the __description__ above ~~
% }
% \source{
%   ~~ reference to a publication or URL from which the data were obtained ~~
% }
% \references{
%   ~~ possibly secondary sources and usages ~~
% }
\examples{
data(ship)
## maybe str(ship) ; plot(ship) ...
}
\keyword{datasets}
