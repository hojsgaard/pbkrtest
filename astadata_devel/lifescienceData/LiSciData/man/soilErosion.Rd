\name{soilErosion}
\alias{soilErosion}
\docType{data}
\non_function{}

\title{SoilErosion}
\usage{data(soilErosion)}
\description{
Experiment on the difference in erosion for two grass varieties.
}
\format{
  This data frame contains:
  \describe{
       \item{grass:}{grass variety: wheatgrass or cheatgrass}
       \item{open:}{\eqn{\log}(length[inch]) of largest opening 
                   of bar soil}
       \item{cover:}{logit of the percentage of soil coverage.}
       \item{erosion:}{soil lost by erosion[pounds/acre]}
}}
\details{
A 200-acre field  was divided into 6\eqn{\times}{X}6 
foot large subplots, their slope varying between 33 to 66 percent. The whole area was exposed to 1.8 inches of
artificial rain.
 13 of the finally used test-plots were covered with wheatgrass the other 16 plots with cheatgrass.
The plots differed in the degree of soil coverage, which was characterized by
the largest opening of bar soil [given in \eqn{\log-inches}] and the percentage 
of ground cover [expressed  as \eqn{logit(p)=\log(\frac{p}{1-p})} of the 
percentage \eqn{p}].
The erosion was measured as the soil lost in pounds per acre.
}
\source{
}

\references{
Bliss, C. I. (1970) \emph{Statistics in Biology},
Volume II, Mc Graw Hill, Düsseldorf,
p. 323--326, Table 18.13
}

\examples{	
data(soilErosion)
}
\keyword{datasets}
