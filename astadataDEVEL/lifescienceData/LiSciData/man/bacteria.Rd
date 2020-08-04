\name{bacteria}
\alias{bacteria}


\non_function{}
\title{Bacterial growth under leucine and sucrose}
\usage{data(bacteria)}
\description{
  A factorial experiment was conducted to analyse the influence of 
  leucine and sucrose on the growth of bacteria.
  For all combinations of 4 concentrations of leucine and 3
  concentrations of sucrose
  the number of bacteria after some hours in a growth medium was
  observed.
  The whole experiment was replicated on 4 days.
  }
\format{
   Data frame containing
\describe{
    \item{day:}{Day of replication}
    \item{sucrose:}{indicator for the sucrose concentration: 1,2 or 3.}
    \item{leucine:}{indicator of the leucine concentration: 1,2,3, or
   4.}
 \item{density:}{num ber of bacteria in 1 mml}
 \item{logdens:}{natural logaritm of the number of bacteria in 1 mml}
  }
}
\source{
  Grafen, Alan and Hails, Rosie, 2006 Modern Statistics for the Life
  Sciences, Oxford: University Press, Chap. 9, page(s): 169
   }
\examples{
data(bacteria)
attach(bacteria)
}





\keyword{datasets}
