\name{lizard}
\alias{lizard}
\docType{data}
\non_function{}

\title{Lizard}
\usage{data(lizard)}
\description{
The data contain information about the habitat of  two lizard species  \emph{A. grahamii} and
\emph{A. opalinus} of the genus \emph{Anolis}.
The questions is whether the lizards habitat is dependent on ecological factors describing the place (perch) where they
were observed.
}
\format{
  This data frame contains:
  \describe{
       \item{n:}{number of lizards}
       \item{sun:}{Sun (=sunshine), Shade ( under shade)}
       \item{height:}{heightnif perch (High: >= 4.75 feet; Low: < 4.75 feet) }
       \item{perch:}{diameter of perch (Broad: > 4.0 inch; Narrow < 4.0 inch) }
       \item{time:}{time of observation (Morning, Mid.day, Afternoon) }
       \item{species:}{opalinus, grahamii }
  }}
\details{log-linear model, two-way or higher way tables}
\source{
Schoener, T.W. (1968) The \emph{anolis} lizards of Bimini: resource partitioning in a complex
fauna, Ecology, 49,  pp. 704--726
}

\references{
Crawley, Michael (2007).  The R book, Wiley,  p. 560 
}

\examples{
data(lizard)
}
\keyword{datasets}
