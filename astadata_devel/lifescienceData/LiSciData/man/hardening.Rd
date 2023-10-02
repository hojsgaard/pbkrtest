\name{hardening}
\alias{hardening}
\docType{data}
\title{
  Heat development during hardening of Portland cement
}
\description{
    Heat development during hardening of Portland cement tiles
    as function of the chemical composition of the cement.
}
\usage{data(hardening)}
\format{
  A data frame with 13 observations on the following 5 variables.
  \describe{
    \item{\code{x1}}{[3CaO][Al2O3] in percentage of tile weight.}
    \item{\code{x2}}{[3CaO][SiO2] in percentage of tile weight.}
    \item{\code{x3}}{[4CaO][Al2O3][Fe03] in percentage of tile weight.}
    \item{\code{x4}}{[2CaO][AiO2] in percentage of tile weight.}
    \item{\code{y}}{Heat development in Calories per gram cement}
  }
}
\details{
  Cao: Calcium oxide; Al2O3: Aluminium oxide; SiO2: Silicium oxide;
  FeO3: Ferro oxide
}
\source{
  Anders Hald: Statistiske Metoder (1948). Akademisk
  forlag. Universitetsforlaget i Kobenhavn. p. 519
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(hardening)
## maybe str(hardening) ; plot(hardening) ...
}
\keyword{datasets}
