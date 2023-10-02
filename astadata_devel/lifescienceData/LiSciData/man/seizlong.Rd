\name{seizlong}
\alias{seizlong}
\docType{data}
\title{Epiliptic Seizures}
\description{
  The dataset
  has the number of epiliptic seizures in each of four two-week
  intervals, and in a baseline eight-week inverval, for treatment
  and control groups with a total of 59 individuals.
}
\usage{data(seizlong)}
\format{
  A data frame with 295 observations on the following 7 variables.
  \describe{
    \item{trt}{a numeric vector, 0: placebo, 1: treatment}
    \item{age}{age of subject}
    \item{time}{time of registration, 0: prior to randomization,
      1,2,3,4: follow-up visit}
    \item{y}{Number of epileptic seizures. For time 0: In the 8 week
      period prior to randomization. For time 1..4: In the 2 week period
    before each follow-up visit}
    \item{id}{Identification of subject}
    \item{D}{Variable given the length (2 or 8 weeks) of the
      registration period}
    \item{x}{a numeric vector. 0: prior to randomization, 1: after randomization}
  }
}
%\details{
%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
  
     Thall, P.F. and Vail S.C. (1990) Some covariance models for
     longitudinal count data with overdispersion. Biometrics *46*:
     657-671.
}
\references{
    Diggle, P.J., Liang, K.Y., and Zeger, S.L. (1994) Analysis of
     Longitudinal Data. Clarendon Press.

}
\examples{
data(seizlong)
## maybe str(seizlong) ; plot(seizlong) ...
}
\keyword{datasets}
