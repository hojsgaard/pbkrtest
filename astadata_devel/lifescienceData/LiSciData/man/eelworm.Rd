\name{eelworm}
\alias{eelworm}
\docType{data}
\non_function{}

\title{EElworm eelworms}
\usage{data(eelworm)}
\description{
Experiment carried out the Rothamsted Experimental Station
to to measure the effetiveness of 4 soil fimugants in keeping
down the numner of eelworms in the soil.
}
\format{
  This data frame contains:
  \describe{
       \item{block:}{Number of block, 1,2,3 or 4}
       \item{plot:}{Plot within block, 1,2,\ldots, 12}
       \item{conc:}{Concentration of fumigant, 0= zero concentration, 1=base concentration, 2=twice the conetration as the base.}
       \item{fumigant:}{Fumigant used, CO=none (control),
       CN=chlorodinitrobenzene,
       CS=carbon disulphide jelly,
       CM=Cymag (propriety preparation),
       CK=Seekay (propriety preparation)}
      \item{n.before:}{Number of eelworm eelworms in a soil sample before application                        of a fumigant}
      \item{n.after:}{Number of eelworm eelworms in a soil sample after application                        of a fumigant}
      \item{li:}{The concentration of fumigant, conc, standardized, such that it has mean 0 and variance 1}
      \item{qu:}{The square of the concentration of fumigant, \eqn{conc^2}, standardized and made orthogonal to the variable \kbd{li}, such that \kbd{sum(li*qu)=0}
 or said differently, the correlation \kbd{cor(li,qu)=0}.}
      \item{treat:}{Treatment generated from combining the 
 3 levels of concentration and the 4 fumigants. factor with 9 levels: 0.CO= no fumigant, 1.CK= concentration 1 and fumigant CK, 
2.CK =  concentration 2 and fumigant CK, etc.}
}}
\details{
The experiment was laid out as a randomized block experiment, 
with 4 blocks and 12 plots whithin each block.
The treatment combinations (concentration.fumigant)
0.CO, 0.CO, 0.CO, 0.CO,  1.CK, 2.CK, 1.CM, 2.CM, 1.CN, 2.CN, 1.CS, 2.CS
 were randomly applied to the 12 plots. 
The combination 0.CO was applied to 4 plots 
to ensure  an accurate standard against which to compare
the fumigants. 
ANOVA, ANCOVA, randomized block design}
\source{
Cochran W.G.  and Cox, Gertrude M. (1957) \emph{Experimental Design}
chapters 3.2 and 3.8, Table 3.1, p. 46. 
}

\references{
}

\examples{	
data(eelworm)
}
\keyword{datasets}
