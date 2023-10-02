\name{enzyme}
\alias{enzyme}
\title{velocity of encymatic reaction}
\description{
The velocity of an enzymatic reaction (measured as \eqn{counts/min^2}
of a radioactive reaction product) where obtained with and without the addition
of puromycin.
}
\usage{data(enzyme)}
\format{
A dataframe with 3 variables. 
\describe{
\item{treat:}{'+': puromycin added, '-': not added}
\item{conc:}{concentration of substrate [ppm]}
\item{v:}{velocity of reation \eqn{counts/min^2}}
}
}


\source{
Bates, D.M. and Watts, D.G.(1988) Nonlinear Regression Analysis, Wiley: New York
}
\references{
Blæsild, P. (1996) Generaliserde Lineære Modeller og SAS, Matematisk Inbstitut, Aarhus Universitet
}


\examples{
data(enzyme)
attach(enzyme)
plot(v~conc,subset=c(treat=='+'),pch='+')
points(v~conc,subset=c(treat=='-'))
legend(0.6,100,legend=c('with puromycin','without puromycin'),pch=c('+','-'))
title('velocity of encymatic reaction\n with and without puromycin')
}

\keyword{datasets}




