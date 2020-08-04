\name{hydrolyse}
\alias{hydrolyse}
\docType{data}
\title{Influence of hydrolysis time on determination of amino acids}
\description{
The  amounts of the aminoacid  lysin  
was determined  for samples from 5 different feed and under 5 different times for hydrolysis time.
The anlysis for each combination of feed and hydrolysis time was performed
 at to independent samples, such that 50 independent  observations 
were obtained. 
The same experiment was repeated for the aminoacid serin.
}
\usage{data(hydrolyse)}
\format{
  A data frame with 100 observations on the following 4 variables.
  \describe{
    \item{amino}{aminoacid, lysin or serin}
    \item{feed}{feed:  barley, bonemeal, fishmeal, maize and soya}
    \item{time}{hydrolysis time: 8, 16, 24, 32 and 72 hours }
    \item{y}{weight of aminoacid g/16g N}
	
}}
\details{
}
\source{
Rudemo, M. Bech-Andersen, S. and Mason, V.C. (1979) Hydrolysate preparation
for amino acid determination in feed constituents. 5. The influence of hyrdolysis time
on amino acid recovery. Z. Tierpsychologie, Tierernährung und Futtermittelkunde, 43, pp. 27--34
}
\references{
Skovgaard Ib M. (1994) Statistisk Forsøgsplanlægning, Institut for Matematik of Fysik,
Den Kngl. Veterinær. og Landbohøjskole, 2.oplag, Jordbrugsforlag, København, Kapitel 2, side 68
}
\examples{
data(hydrolyse)
## maybe str(hydrolyse) ; plot(hydrolyse) ...
}
\keyword{datasets}
