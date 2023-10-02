\name{grainBeetle}
\alias{grainBeetle}
\title{Graint Beetles affected by fumnigant}
\description{
Grain Beetles were exposed to different doses of
the fumigant ethylene oxide to test its effectiveness
against insect pests.

The determination of whether the fumigant was   affected
could only be observed after several days after exposure and the 
beetles had to be transferred to glasstubes containing
suitable food and having a temperature at about 25 degrees.
}

\usage{data(grainBeetle)}
\format{
A dataframe 
\describe{
\item{conc:}{concentration of ethylene oxide [mg/l]}
\item{affected:}{number of beetles affected}
\item{exposed:}{total number of beetles exposed}
}
}

\source{
Busvine, J.R.(1938) The toxicity of ethylene oxide to 
Calandra oryzae, C. granaria,
Tribolium castaneum and Cimex lectularius, Annals of Applied Biology, 25,
605-632
}

\references{
Collet, D. (2003) Modelling Binary Data, 2nd edition, Chapman & Hall/CRC, London, Example 6.5; table 6.4
}


\examples{
data(grainBeetle)
attach(grainBeetle)
}

\keyword{datasets}




