\name{jeans}
\alias{jeans}
\docType{data}
\non_function{}

\title{jeans}
\usage{data(jeans)}
\description{
  Denim manufacturers are concerned with maximizing the comfort of the
  jeans they make.
  In the manufacturing process, starch is often built up in the fabric,
  creating stiff jeans that must be \emph{broken in} by the wearer before
  they are comfortable.
  To minimize the break-in time for each pair of pants,
  the manufacturers often wash the jeans to remove as much starch as
  possible. 
  In a particular study to evaluate starch removal,
  data was recorded including the washing method (3 types),
  size of the load (in pounds),
  whether or not the fabric was sand blasted prior to washing,
  and the starch content of the fabric after washing.
  }
\format{
  This data frame contains:
  \describe{
       \item{method:}{washing method}
       \item{load:}{size in pounds}
       \item{blast:}{fabric sandblasted: Yes or No}
       \item{starch:}{starch content of fabric after washing}
   }}
\details{linear regression, two analysis of variance, interaction }
\source{
Sall, J., Creighton, L., & Lehman, A. (2006). JMP Start Statistics,
Third Edition. Cary, NC: SAS Institute Inc. See also
\url{http://support.sas.com/learn/statlibrary/statlib_eg4.1/linreg_3.htm#KB4}
}

\references{
}

\examples{
data(jeans)
pairs(jeans)
}
\keyword{datasets}
