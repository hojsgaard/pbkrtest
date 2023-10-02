\name{esr}
\alias{esr}
\title{Determination of erythrocyte sedimenation rate(ESR)}
\description{
The ESR describes the rate at which the red blood cells (erythrocytes)
settle out of a suspension of blood plasma. In an experiment at the
Institute of Medical research, Kuala Lumpur, is was analyzed how the
ESR is related to the plasma proteins fribinogen and
gamma-globulin. An ESR<20 indicates a healthy individual. 
}

\usage{data(esr)}
\format{
A dataframe 
\describe{
\item{fib:}{fibrinogen [gm/l]}
\item{glob:}{\eqn{\gamma}-globulin}
\item{esr:}{0: esr<20; 1: 20<= esr}
}
}

\source{
Collet and Jemain(1985) Residuals, outliers and influential observations
in regression analysis. Sains Malaysiana, 14, 493-511
}

\references{
Collet, D. (2003) Modelling Binary Data, 2nd edition, Chapman & Hall/CRC, London, Example 1.7
}


\examples{
data(esr)
attach(esr)

}

\keyword{datasets}




