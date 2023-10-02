\name{plaque}
\alias{plaque}
\alias{plaqueWide}
\docType{data}
\title{Effect of apple and orange eating on plaque formation}
\description{
  In a trial the effect of eating of apples or oranges on the formation
  on dental plaque was analysed. Each child ate an apple after dinner
  the one week and the other week an orange.
  All children ate the same school meal and the sequence of apple and
  orange eaten was randomized.
  The extent of plaque was maesured with the Loe index defined as
  0: no plaque
  1: plaque invisible to the eye, but detactable by probes
  2: moderate plaque visible
  3: heavy deposits of plaque
}
\usage{data(plaque)}
\format{
  A data frame with 30 observations on the following 3 variables.
  \describe{
    \item{child}{id number of child}
    \item{apple}{Loe index for eating apples}
    \item{age}{Loe index for eating oanges}
      }
}
% \details{

% }
 \source{
   Osborn, J. F. (1979) Statistical Exercises in Medical Research,
   Blackwell, Oxford,  p. 106, exercoe 13.2.2
 }
% \references{
%   ~~ possibly secondary sources and usages ~~
% }
\examples{
data(plaque)
}
\keyword{datasets}
