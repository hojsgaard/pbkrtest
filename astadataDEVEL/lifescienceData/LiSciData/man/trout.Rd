\name{trout}
\alias{trout}
\alias{troutsub}
\docType{data}
\title{Weight and length of rainbow trout}
\description{
A data set with the length and weight of rainbow trouts of two sexes
measured repeatedly over a 6 month period
}
\usage{data(fishw)}
\format{
  A data frame with 2999 observations on the following 8 variables.
  \describe{
    \item{family.id}{a numeric vector}
    \item{animal.id}{a numeric vector}
    \item{vessel}{a numeric vector}
    \item{date}{a numeric vector}
    \item{length}{a numeric vector}
    \item{weight}{a numeric vector}
    \item{sex}{a factor with levels \code{f} \code{m}}
    \item{month}{a numeric vector}
  }
}
\details{
  'fishwsub' is a subset of 'fishw'
}
\source{
  Data are collected by Majbrit Holst Nielsen, Danish Institute of
  Agricultural Sciences in the Autumn 2003
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(fishw)
## maybe str(fishw) ; plot(fishw) ...
}
\keyword{datasets}
