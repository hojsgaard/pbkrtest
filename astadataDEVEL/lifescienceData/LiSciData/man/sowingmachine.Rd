\name{sowingmachine}
\alias{sowingmachine}
\docType{data}
\non_function{}

\title{Sowingmachine}
\usage{data(sowingmachine)}
\description{
10 different fields were partitions into two subfields each.
On one subfield a traditional sawing machine was used on the other
a more modern machine.
The harvest yields of the two machines were compared.
}
\format{
  This data frame contains:
  \describe{
       \item{fieldid:}{id number of field}
       \item{new:}{yield using modern machine}
       \item{old:}{yield using old machine}
}}
\details{
Anova, paired
}
\source{
Wishart, J.(1934) \emph{Statistics in Agricultural Research}
Suppl. Journal Roy. Stat. Society
}

\references{
Jens Ledet Jensen (2009) Et Nanokurs i Statistik, chapter 7, p. 77.
}

\examples{	
data(sowingmachine)
}
\keyword{datasets}
