\name{body}
\alias{body}
\docType{data}
\non_function{}

\title{body}
\usage{data(body)}
\description{
Body measurements were collected from 22 male subjects. 
On wants to construct a model which would predict 
the mass of a person based on other characteristics.
  }
\format{
  This data frame contains:
  \describe{
       \item{mass:}{body mass[kg]}
       \item{fore:}{forearm cirumference}
       \item{bicep:}{bicep circumference}
       \item{chest:}{chest circumference}
       \item{neck:}{neck circumference}
       \item{shoulder:}{width of shoulder}
       \item{waist:}{waist circumference}
       \item{height:}{height}
       \item{calf:}{calf circumference [dansk: laeg]}
       \item{thigh:}{thigh circumference [dansk: laar]}
       \item{head:}{head: head circumference}
   }}
\details{linear regression, several regressors, model selection}
\source{
\url{http://support.sas.com/learn/statlibrary/statlib_eg4.1/linreg_3.htm#KB4}
}

\references{
}

\examples{
data(body)
pairs(body)
}
\keyword{datasets}
