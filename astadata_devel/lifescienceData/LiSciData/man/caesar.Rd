\name{caesar}
\alias{caesar}
\title{infection from birth by Caesarian section}
\description{
data collected from Weissenbacher, R.E, Klinikum Grosshadern, Munich, on
the occurrence of infections of birth by Caesarian section.
\cr
 logistic regression
\cr
sparse cells with non-existence of ML-estimator
}
\usage{data(caesar)}
\format{
A dataframe with 5 variables. 
\describe{
\item{infect:}{yes: infection after birth; no: no infection}
\item{risk:}{1: risk factors as diabetics or excessive weight are present; 0: not present}
\item{antib:}{1: antibiotics given as prophylaxe; 0: no antibiotics}
\item{caesp:}{1: Caesarian section planned; 0: not planned}
\item{r:}{number of births with infection}
}
}

\source{
Fahrmeyer, Tutz (2001)
Multivariate statistical modellng based on Generalized Linear Models, 
Springer, 2nd edition, pages 3, 30 and  51;
}



\examples{

data(caesar)
attach(caesar)

#printing the data as a flat contingency table:
  #  data.frame to contigency table
    f<-xtabs(r~infect+risk+antib+caesp)
  # flat contigency table
    f<-ftable(caesp+infect~antib+risk,data=f)
}
\keyword{datasets}




