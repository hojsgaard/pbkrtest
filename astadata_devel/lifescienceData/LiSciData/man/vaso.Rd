\name{vaso}
\alias{vaso}
\docType{data}
\title{
  Vaso}
\description{
  The data an experiment to study the influence of
the volume and the rate of air inspired by a human on
the occurrence of transient vaso-constriction in the skin of the
fingers.
Four specific temperatures and two specific
humidities were analysed.
Vaso-constriction is the process by which blood vessels in the
extremities (arms and legs) are constricted, preventing blood flow to
certain tissues. Hence, on cold days fingers seem thinner.
  \cr
 To way ANOVA
\cr
}
\usage{data(vaso)}
\format{
A dataframe with 3 variables. 
\describe{
\item{volume:}{k}
\item{rate:}{4 levels}
\item{constr:}{binary: 1: constrriction observed, 0: no constriction}
}
}

\source{
 Finney, D.J (1947) The estimation from original records of the
 relationship
between dose and quantal response, Biometrika, 34, 320-334
}

\references{
Krzanowski, Wojtek J. (1998)
An Introduction to Statistical Modelling
Arnold, London
}


\examples{
data(vaso)
}

\keyword{datasets}




