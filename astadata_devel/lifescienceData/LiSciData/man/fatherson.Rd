\name{fatherson}
\alias{fatherson}
\docType{data}
\title{Pearson's data on height of father and sons}
\description{

14 samples of a producte were randomly divided into 5 groups 
and each group was stored under different conditions.
After storage the water content was measured.
}
\usage{data(fatherson)}
\format{
A dataframe with 2 variables. 
\describe{
\item{father}{height of father [inch]}
\item{son}{height of son [inch]}
}
}

\source{
}

\references{
}



\examples{
data(fatherson)
plot(father~son, data=fatherson)
}
\keyword{datasets}




