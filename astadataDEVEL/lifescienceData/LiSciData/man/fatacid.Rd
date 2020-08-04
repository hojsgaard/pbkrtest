\name{fatacid}
\alias{fatacid}
\docType{data}
\title{Concentration of fish oil fatty acid x14 in tissue of pork}
\description{
A fish oil fatty acid x14 has been added in the concentrations in
different concentrations to the food for pigs in a study. Interest is in
seeing how much of the fatty acid can be found in the tissue.

}
\usage{data(fatacid)}
\format{
  A data frame with 49 observations on the following 5 variables.
  \describe{
    \item{sample}{a numeric vector}
    \item{dose}{a numeric vector}
    \item{pigid}{a numeric vector}
    \item{litter}{a numeric vector}
    \item{x14}{a numeric vector}
  }
}
\details{
The pigs are fed  with food containing the fatty acid.
  until their weight is 60 kg. 
From thereof and until they are
  slaughtered at 100kg, their food does not contain the fish oil.
At 60kg (sample=1) and 100kg (sample=2) muscle
  biopsies are made and the concentration of x14 is
  determined.
}
%\source{
%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(fatacid)
## maybe str(fatacid) ; plot(fatacid) ...
}
\keyword{datasets}
