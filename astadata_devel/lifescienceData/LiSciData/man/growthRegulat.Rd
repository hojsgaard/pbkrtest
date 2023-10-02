\name{growthRegulat}
\alias{growthRegulat}


\non_function{}
\title{Two root beet growth regulators}
\usage{data(growthRegulat)}
\description{
Two root beet growth regulators were analysed against no regulation
 in a 2 \eqn{\times} 2 factorial design.
}
\format{
  This data frame contains 
  \describe{
    \item{opdens:}{Optical density of a paper chromatography of amino acid at spot 4}
    \item{block:}{1,\eqn{\ldots},8 blocks.}
    \item{regM:}{Application of the regulator maleic hydrazide; 
    yes: applied, no: not applied}
    \item{regD:}{Application of the regulator dichlorphenoxyacetic: 
        yes: applied, no: not applied}
  }}
\details{
The effect of two synthetic  growth regulators 
upon the amino acid contents of sugar beets was studied in a 2 \eqn{\times} 2
factorial design.
One, maleic hydrazide (regM) has been reported in the literature 
to retard  growth while
the other 2,4-dichlorophenoxyacetic (regD),  is supposed to be a growth 
promoter at low concentrations.

Each regulator was tested against its absence in a field test of 8 
randomized blocks when applied 60 days before harvest.
From each plot the juice of ground beet roots were analyzed for their  
amino-acid content. This was measured by chromatography and the 
optical density at spot 4 was used as the response measurement.
}
\source{
        }
\references{
Bliss, C. I. (1970) \emph{Statistics in Biology},
Volume II, Mc Graw Hill, Düsseldorf,
p. 369-370, Table 19.1
}
\examples{
data(growthRegulat)

}
\keyword{datasets}
