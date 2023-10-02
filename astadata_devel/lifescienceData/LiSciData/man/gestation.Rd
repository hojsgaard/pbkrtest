\name{gestation}
\docType{data}
\alias{gestation}

\non_function{}
\title{Weight at gestational age of boys and girls}
\usage{data(gestation)}
\description{
  Weight of boys and girls at different gestational
  (=under pregnancy, prenatal) ages.
  \cr
 Analysis of covariance
  }
\format{
   Data frame with four variables
  \describe{
    \item{boys.age:}{age of boys [weeks]}
    \item{boys.weight:}{weight of boys [g]}
    \item{girls.weight:}{age of girls [weeks]}
    \item{girls.weight:}{weight of girls [g]}
}}
\details{
Data are from the Table 2.2, page 22
}
\source{
Dobson, A. (2002) \emph{ An introduction to Generalized Linear Models},
London: Chapman \& Hall
        }
\references{
}
\examples{
data(gestation)
gestation<- reshape(gestation,direction='long',
          varying=list(age=c('boys.age','girls.age'),weight=c('boys.weight',
                                                              'girls.weight')),
          v.names=c('age','weight'),
          timevar='sex',times=c('boy','girl'))
summary(gestation)
}





\keyword{datasets}
