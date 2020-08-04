\name{dietsup}
\docType{data}
\alias{dietsup}

\non_function{}
\title{Groweth of rats}
\usage{data(dietsup)}
\description{
  The growth rates of rats fed with different amounts of a diet supplement.
   \cr
   Sinmple linear rergression with quadratic repsonse curve
 }
\format{
   Data frame with two variables
  \describe{
    \item{x:}{amount of supplement [g]}
    \item{y:}{growth rate [coded units]}
}}
\details{
Data are from the Table 14.10 page 480.
}
\source{
  Box, G. E. P., Hunter, W. G., Hunter, J. S. (1978) Statistics for
  Experiments
          }
\references{
}
\examples{
data(dietsup)
attach(dietsup)
plot(y~x)
fit<-predict(lm(y~x+I(x^2)))
lines(fit~x)
detach(dietsup)
}





\keyword{datasets}
