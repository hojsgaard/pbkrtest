\name{loyn}
\alias{loyn}
\docType{data}
\non_function{}

\title{Abundance of birds}
\usage{data(loyn)}
\description{
Data contain  for 56 forest patches in southeastern Victora, Australia, 
the abundance of forest birds in each patch, the distance of a patch to the neigbour and the nearest larger patch,
grazing intensity by livestock, altitude of the patch  and years since the isolation of the patch.
}
\format{
  This data frame contains:
  \describe{
       \item{abund:}{abundance of forest birds in patch}
       \item{area:}{patch area [ha]}
       \item{yr.isol:}{number of years since patch was isolated}
       \item{dist:}{distance to nearest patch [km]}
       \item{ldist}{distance to nearest larger patch [km]}
       \item{graze:}{grazing intensity in patch (1 to 5 indicating light to heavy)}
       \item{alt:}{altitude ([m] above sealevel) of patch}
  }}
\details{ GAM
}
\source{
Loyn, R.H (1987) Effetcs of patch area and habitat on bird abundance, species numbers and tree health
in fragmented Victorian forests, In: Nature Conservation: The role of remnants
of native vegetation (Saunders, D.A.- Arnold, G.W., Burbidge, A.A, Hopkins, A.J.M. eds.)
pp 65-77, Surrey Beatty and Sons, Chipping Norton, NSW}

\references{
Quinn, Gerry. P. and Keough, Michael J. (2002) \emph{Experimental Design and Data Analysis for Biologists}, University Press, Cambridge
Box 13-3, Example for GAMs
}

\examples{
data(loyn)
}
\keyword{datasets}
