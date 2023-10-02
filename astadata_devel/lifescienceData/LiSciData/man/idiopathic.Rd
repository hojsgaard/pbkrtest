\name{idiopathic}
\alias{idiopathic}
\docType{data}
\title{Treatment of idiopathic respiratory disease}
\description{
  In a comparative trial of trishydroxymethyl-amino-methane (THAM)
  and sodium bicarbonate in the treatment of idiopathic respiratory
  distress syndrome
  25 children were treated with THAM and 25 with sodum bicarbonate.
  It was observed whether the child survived or not.
  The disease is an acute lung disease of the newborn
  (especially the premature newborn). The
  lungs cannot expand because of a wetting agent is lacking.
  \cr
binary logistic regression  
}
\usage{data(idiopathic)}
\format{
  A data frame with 50 observations on the following 5 variables.
  \describe{
    \item{child}{id number of child}
    \item{y}{survival: y='survived' child survived, y='died' child died}
    \item{treat}{0: child received sodium bicarbonate, 1: chld got THAM}
    \item{time}{time of onset of spontaneous respiration [minutes]}
    \item{weight}{birthweight of the child [kg]}
          }
}
% \details{

% }
 \source{
   Osborn, J. F. (1979) Statistical Exercises in Medical Research,
   Blackwell, Oxford,  p. 87, exercise 10.4.2
 }
% \references{
%   ~~ possibly secondary sources and usages ~~
% }
\examples{
data(idiopathic)
}
\keyword{datasets}
