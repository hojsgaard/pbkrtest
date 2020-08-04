\name{flourBeetle}
\alias{flourBeetle}
\title{Toxicity of insecticides to flour beetles}
\description{
Flour beetles (tribolium castaneum) were sprayed with one of three
different insecticides in solution in Shell oil P31.

The type of sprays were: DDT, gamma-BHC and a mixture of both.

The table contains the proportions of insects killed  after a period of 6 days.
}

\usage{data(flourBeetle)}
\format{
A dataframe 
\describe{
\item{killed:}{beetles killed}
\item{total:}{total number of beetles exposed}
\item{dose:}{dosis of spray used [mg/10cm$^2$]}
\item{spray:}{type of spray: ddt, bhc, mix}
}
}

\source{
Collet, D. (1991) Modelling Binary Data, Chapman & Hall, London,
 Example 4.3, 4.4, 5.9, p. 103
}

\references{}


\examples{
data(flourBeetle)
attach(flourBeetle)

}

\keyword{datasets}




