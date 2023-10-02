\name{roberts}
\alias{roberts}
\docType{data}
\non_function{}

\title{Dead coolbah trees}
\usage{data(roberts)}
\description{
The presence of dead coolibah trees (Eucalyptus coolibah)
was observed in  sampled quadrats along a transect on floodplain.
Of interest was the dependence of the presence of these trees on the position on the transect, either
close to the dunes, to the lakeshore or intermediate.
}
\format{
  This data frame contains:
  \describe{
       \item{coolbah:}{presence of dead coolbah trees in sampling quadrat (with, without)}
       \item{position:}{position of the quadrat on the transect line (top=dunes, bottom=lokeshore, middle= intermediate)}
       \item{count:}{number of dead coolbah trees in the sampling quadrat}
  }}
\details{logistic regression,  Log-linear model
}
\source{
Roberts, J. (1993) Regeneration and growth of coolibah 
Eucalyptus Coolabah subsp. arida, a riparian [dansk: bred] tree
in the Cooper Crek region of South Australia,
\emph{Australian Journal of Ecology}, 18, pp. 345--350
}

\references{
Quinn, Gerry. P. and Keough, Michael J. (2002) \emph{Experimental Design and Data Analysis for Biologists}, University Press, Cambridge
Table 14-1 and Box 14-3
}

\examples{
data(roberts)
print(xtabs(count~position+coolbah,data=roberts))
}
\keyword{datasets}
