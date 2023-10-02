\name{mathmark}
\alias{mathmark}
\non_function{}
\title{ ~~ 1-line description of the data frame ~~ }
\usage{data(mathmark)}
\description{
The \code{mathmark} data frame has 88 rows and 5 columns.
~~ Give a concise description here ~~
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{mechanics}{a numeric vector}
    \item{vectors}{a numeric vector}
    \item{algebra}{a numeric vector}
    \item{analysis}{a numeric vector}
    \item{statistics}{a numeric vector}
  }
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Second Edition, Springer Verlag, 2000}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}
\examples{
data(mathmark)
str(mathmark) ;
plot(mathmark) 
}
\keyword{datasets}
