\name{ptc}
\alias{ptc}
\docType{data}
\title{Taste threshold to phenylthiourea (PTC)}
\description{
For 104 medical students their taste threshold to phenylthiourea (PTC) was
measured. Although threshold vary widely between idnividuals
there seem to be two groups, taster and non-tasters.
\cr
Poisson
  }
\usage{data(ptc)}
\format{
  A data frame with 14 observations and  3 variables.
  \describe{
    \item{conc:}{threshold of PTC [mg/l] at which studend tasted something}
    \item{n:}{number of students with the threshold}
    \item{taster:}{classifications as taster ('yes') or non-taster ('non')}
          }
}
% \details{

% }
 \source{
   Osborn, J. F. (1979) Statistical Exercises in Medical Research,
   Blackwell, Oxford,  p. 94, exercise 11.4.2
 }
% \references{
%   ~~ possibly secondary sources and usages ~~
% }
\examples{
data(ptc)
}
\keyword{datasets}
