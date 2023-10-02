\name{cowmilk}
\alias{cowmilk}
\docType{data}
\title{
  Daily milk yield for 21 dairy cows
  %%   ~~ data name/kind ... ~~
}
\description{
    Daily milk yield for 21 dairy cows
    %%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(cowmilk)}
\format{
  A data frame with 6938 observations on the following 4 variables.
  \describe{
    \item{\code{cow}}{Cow identifier - a numeric vector}
    \item{\code{lactno}}{Lactation number (parity number) - a numeric vector}
    \item{\code{dfc}}{Days from calving -- a numeric vector}
    \item{\code{milk}}{Daily milk yield (in kg) - a numeric vector}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(cowmilk)
## maybe str(cowmilk) ; plot(cowmilk) ...
}
\keyword{datasets}
