\name{dentalgrowth}
\alias{dentalgrowth}
\docType{data}
\title{Dentalgrowth in children}
\description{
In a longitudinal study the dental growth in children was analysed.
The distance from the center of the pituitary gland to the maxillary fissure for children at
ages 
 8, 10, 12 and 14 was measured.
}
\usage{data(dentalgrowth)}
\format{
  A data frame with 4 variables.
  \describe{
    \item{age}{ age of child when the distance was measured}
    \item{dist}{distance}
    \item{id}{identification number of child whithin sex }
    \item{sex}{sex of child}
  }
}
\details{
}
\source{
Myers, H.R. , Montgomery D.C., Vining, G.G. (2002) Generalized Linear Models, Exercise 6.6
}
\references{
  ~~ possibly secondary sources and usages ~~
}
\examples{
data(dentalgrowth)
library(lattice)
trellis.device('windows',theme='col.whitebg')
xyplot(dist~age|sex,group=id,data=dentalgrowth,type=c('p','l'))
}
\keyword{datasets}
