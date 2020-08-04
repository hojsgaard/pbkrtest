\name{respiratory}
\alias{respiratory}
\alias{respiratoryWide}
\docType{data}
\title{Data from a clinical trial comparing two treatments for a respiratory illness}
\description{
  The data are from a clinical trial of patients with respiratory
illness, where 111 patients from two different clinics were randomized
to receive either placebo or an active treatment. Patients were
examined at baseline and at four visits during treatment. At each
examination, respiratory status (categorized as 1 = good, 0 = poor) 
was determined. 
}
\usage{data(respiratory)}
\format{
  A data frame with 111 observations on the following 7 variables.
  \describe{
    \item{center}{a numeric vector}
    \item{id}{a numeric vector}
    \item{age}{a numeric vector}
    \item{baseline}{a numeric vector}
    \item{active}{a numeric vector}
    \item{center2}{a numeric vector}
    \item{female}{a numeric vector}
  }
}
% \details{

% }
% \source{
%   ~~ reference to a publication or URL from which the data were obtained ~~
% }
% \references{
%   ~~ possibly secondary sources and usages ~~
% }
\examples{
data(respiratory)
## maybe str(respiratory) ; plot(respiratory) ...
}
\keyword{datasets}
