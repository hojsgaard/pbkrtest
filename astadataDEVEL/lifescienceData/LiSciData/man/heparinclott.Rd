\name{heparinclott}
\alias{heparinclott}
\docType{data}
\title{Clotting time of blood with heparin (HEPARINCLOTT)}
\description{
  A standard preparation of heparin is compared to a test preparation
  by measuring the clotting time of blood for different doses of
  heparin. At each dose level two measurements are available. 
\cr
normal, gamma with inverse link
  }
\usage{data(heparinclott)}
\format{
  A data frame with 20  observations and  3 variables.
  \describe{
    \item{time:}{clotting time of blood (sec)}
    \item{dose:}{log-dose of heparin}
    \item{preparation:}{'standard' or 'test' preparation}
          }
}
% \details{

% }
 \source{
   Osborn, J. F. (1979) Statistical Exercises in Medical Research,
   Blackwell, Oxford,  p. 92, exercise 11.3.2
 }
% \references{
%   ~~ possibly secondary sources and usages ~~
% }
\examples{
data(heparinclott)
}
\keyword{datasets}
