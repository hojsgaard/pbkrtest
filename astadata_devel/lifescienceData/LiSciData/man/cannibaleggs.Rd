\name{cannibaleggs}
\alias{cannibaleggs}
\docType{data}
\title{Tribolium: cannibalistic egg eating.}
\description{
Study to analyse cannibalistic behavior of three species of the beetle of genus Tribolium.
150 eggs (50 of each species) were exposed to adult beetles of one species and
it was recorded how many eggs of which species they ate.
}
\usage{data(cannibaleggs)}
\format{
A dataframe with  variables.
\describe{
\item{adult:}{adult Species: A=castaneum, b= confusum, C=madens}
\item{egg:}{species of the egg}
\item{day:}{indicator of the day the experiment was performed}
\item{y:}{number of eggs out of 50 that were NOT eaten}
}
}

\source{
}

\references{
McCullagh, P. and Nelder, J.A.(1989), Generalized Linear Models, Chapman and Hall, Exercise 4.25, p. 143
}




\examples{
data(cannibaleggs)  
}
\keyword{datasets}	




