\name{french}
\alias{french}
\docType{data}
\non_function{}

\title{Regeneration and dispersal mechanism of plants}
\usage{data(french)}
\description{
In a study the number of occurrences  between the regeneration mechanism of species 
(either by seed or only vegetatively)
and their dispersal mechanism (by ants or vertebrate) was recorded.
}
\format{
  This data frame contains:
  \describe{
       \item{regen:}{regeneration mechanism, seed og veg(etatively)}
       \item{dispers:}{disperal mechanism, ant or vert(ebrate)}
       \item{count:}{number occurences in each class}
  }}
\details{ Log-linear model
}
\source{
French, R.H and Westoby, M. (1996) 
vertebrate-dispersed species in fore-prone environment.
\emph{Australian Journal of Ecology}, 21, pp. 379--385
}	

\references{
Quinn, Gerry. P. and Keough, Michael J. (2002) \emph{Experimental Design and Data Analysis for Biologists}, University Press, Cambridge
table 14.1, Box 14.2
}

\examples{
data(french)
print(xtabs(count~regen+dispers,data=french))
}
\keyword{datasets}
