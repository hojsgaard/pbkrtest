\name{steers}
\alias{steers}
\docType{data}
\title{Weight gain of steers}
\description{
For three  feed diets A, B and C the weight gain (g/day) of steers are examined in 
an RCBD.
For 4 differnt intial weight classes (from 1=light to 4=very heavy) 6 steers were chosen for each class.

Within each weight class each tretments was randomly assigned to 2 steers.
\cr
 RCBD
\cr
}
\usage{data(ovolution)}
\format{
A dataframe with 4 variables. 
\describe{
\item{weight:}{inital weight class 1,...,4}
\item{treat:}{3  diets A, B and C}
\item{steerid:}{steers id}
\item{gain:}{daily weight gain in g}
}
}

\source{
Miroslav Kaps and  William Lamberson (2009) biostatistics for animal science
CABI, p. 319.
}



\examples{
data(steers)
}

\keyword{datasets}




