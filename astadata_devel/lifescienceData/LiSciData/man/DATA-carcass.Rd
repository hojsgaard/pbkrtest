\name{carcass}
\alias{carcass}
\alias{carcassall}
\title{Lean meat contents of 344 pig carcasses}
\usage{
data(carcass)
data(carcassall)
}
\description{
  Measurement of lean meat percentage of 344 pig carcasses together with
  auxillary information collected at three Danish slaughter houses
}
\format{
  carcassall: A data frame with 344 observations on the following 17 variables.
  \describe{
    \item{\code{weight}}{Weight of carcass}
    \item{\code{lengthc}}{Length of carcass from back toe to head (when
      the carcass hangs in the back legs)}
    \item{\code{lengthf}}{Length of carcass from back toe to front leg
      (that is, to the shoulder)}
    \item{\code{lengthp}}{Length of carcass from back toe to the pelvic bone}
    \item{\code{Fat02, Fat03, Fat11, Fat12, Fat13, Fat14, Fat16}}{Thickness of fat
      layer at different locations on the back of the carcass (FatXX
      refers to thickness at (or rather next to) rib no. XX. Notice that
      02 is closest to the head}
    \item{\code{Meat11, Meat12, Meat13}}{Thickness of meat layer at different
      locations on the back of the carcass, see description above}
    \item{\code{LeanMeat}}{Lean meat percentage determined by dissection}
    \item{\code{slhouse}}{Slaughter house; a factor with levels \code{a} \code{b} \code{c}}
    \item{\code{sex}}{Sex of the pig; a factor with \code{a} \code{b}
      \code{c}. Notice that it is no an error to have three levels; the
      third level refers to castrates}
  }
  carcass: Contains only the variables  Fat11, Fat12, Fat13, Meat11, Meat12, Meat13, LeanMeat
}


\source{
  Busk, H., Olsen, E. V.,  Brøndum, J. (1999)
  Determination of lean meat in pig carcasses with the Autofom classification system,
  Meat Science, 52, 307-314
  }
\keyword{datasets}

