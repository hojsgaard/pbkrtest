\name{MonoclonalGammopathy}
\alias{monogam}


\docType{data}
\title{MonoclonalGammopathy}
\description{
A monoclonal gammopathy indicates the presence of an abnormal protein in your
blood. The protein is produced by a group of cells in the bone marrow called
plasma cells. Plasma cells are normally found in the bone marrow and represent
approximately 1 percent of all marrow cells. They produce the antibodies that
help your body fight infection.
}
\usage{data(MonoclonalGammopathy)}
\format{
  A data frame with 20 observations on the following 5 variables.
  \describe{
    \item{Age}{a numeric vector}
    \item{Lifestyle}{a factor with levels \code{Rural} \code{Urban}}
    \item{Sex}{a factor with levels \code{F} \code{M}}
    \item{Diseased}{a numeric vector}
    \item{Total}{a numeric vector}
  }
}
\details{
The population in study was
classified according to the age (from 30 to 70 years old), the type of life
(farming and non-farming) and the gender (male and female).

A monoclonal gammopathy indicates the presence of an abnormal protein in your
blood. The protein is produced by a group of cells in the bone marrow called
plasma cells. Plasma cells are normally found in the bone marrow and represent
approximately 1 percent of all marrow cells. They produce the antibodies that
help your body fight infection.

Abnormal proteins circulating in the blood are not rare. They occur in 1 percent
of healthy people over the age of 50 and in 3 percent of people over the age of
70.

Generally, the abnormal protein in the blood produces no symptoms. Most
monoclonal gammopathies are detected incidentally as part of a complete physical
examination.

This abnormal protein does not represent cancer. However, there is a 20 percent
chance that the monoclonal gammopathy may develop into a more serious condition,
including some forms of cancer.

No evidence suggests a substantial increase in risk for the spouse or immediate
family members of someone with monoclonal gammopathy. They do not need to be
checked for an abnormal protein if they are feeling well.
}
% \source{
%   ~~ reference to a publication or URL from which the data were obtained ~~
% }
% \references{
%   ~~ possibly secondary sources and usages ~~
% }
\examples{
data(monogam)
plot(monogam)
}
\keyword{datasets}
