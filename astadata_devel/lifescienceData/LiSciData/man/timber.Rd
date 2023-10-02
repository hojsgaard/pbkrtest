\name{timber}
\alias{timber}
\title{hardness and density of timber wood}
\description{
the janka hardness index and the density recorded for
36 Australian hard wood samples

The hardness is a difficult to measure quantity, and one wants 
to use density to
predict the hardness.
}
\usage{data(timber)}
\format{
A dataframe with 2 variables. 
\describe{
\item{hardness:}{janka hardness index}
\item{density:}{density of the wood}
}
}


\source{
Williams, E.J (1959) \emph{Regression Analysis}, Wiley: New York
}
\references{
Venables, W.N. (2000) \emph{Exegeses on Linear Models} paper Presented on the S-Plus User-s conference, Washington, DC, 8-9th October 1998
\url{http://www.stats.ox.ac.uk/pub/MASS3/Exegeses.pdf}
}


\examples{
data(timber)
attach(timber)
par(mfrow=c(2,1))
plot(density,hardness)
fit<-lm(hardness~density+I(density^2))
plot(predict(fit),residuals(fit))
abline(h=0)
title('residuals of hardness~ density + density^2 vs. fitted values')
}

\keyword{datasets}




