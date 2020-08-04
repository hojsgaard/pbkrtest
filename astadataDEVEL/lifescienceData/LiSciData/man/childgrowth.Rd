\name{childgrowth}
\alias{childgrowth}
\docType{data}
\non_function{}

\title{childgrowth}
\usage{data(childgrowth)}
\description{
  The ratio of weight to height was measured for 72 boys and 72 girls.
  }
\format{
  This data frame contains:
  \describe{
       \item{ratio:}{weight/height}
       \item{age:}{age of children in months}
       \item{sex:}{boys and girls (boy, girl)}
   }}
\details{linear regression, quadratic regression, several regression
  lines;
The data for the boys coincide with the data given in  in
Sall.J. Creighton (2006)
who give no sex classification. The present girl data were generated.}
\source{
Sall, J., Creighton, L., and Lehman, A. (2006). JMP Start Statistics, Third Edition. Cary, NC: SAS Institute Inc.  
see also \url{http://support.sas.com/learn/statlibrary/statlib_eg4.1/linreg_3.htm#KB4}
}

\references{
}

\examples{
data(childgrowth)
plot(ratio~age,data=childgrowth,pch=c(4,16)[sex],col=c(1,2)[sex])
}
\keyword{datasets}
