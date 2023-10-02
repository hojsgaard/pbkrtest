\name{furnace}
\alias{furnace}
\title{
  Recovery of heat}
\description{
  The heat revovery of heat lost to the environment in the form of
  exhaust gases by two types of furnaces is analysed.
  For different settings of the speed  of the exhaust gas passing
  the heat converter the rcovery ratio is measured.
  \cr
  Analysis of Covariance
}
\usage{data(furnace)}
\format{
A dataframe with 3 variables. 
\describe{
\item{recovery:}{ratio of heat recovered to heat lost}
\item{flow:}{speed of exhaust gas [meter/second]}
\item{furnace:}{type of furnace: A, B}
}
}

\source{
Milton, J. S. and Arnold, Jesse, C. (1995)
Introduction to Probability and Statistics, 
McGrwa Hill, New York, p. 526, chapter 12, exercise 53
}



\examples{
data(furnace)
}
\keyword{datasets}




