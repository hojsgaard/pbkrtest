\name{cow_treat}
\alias{cow_treat}
\docType{data}
\title{
  Records of time of treatments for a specific disease for a set of cows.
}
\description{
    Records of time of treatments for a specific disease for a set of cows.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(cow_treat)}
\format{
  A data frame with 744 observations on the following 3 variables.
  \describe{
    \item{\code{animal}}{Animal identifier - a numeric vector}
    \item{\code{trt_date}}{a Date}
    \item{\code{trt_code}}{a numeric vector}
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
data(cow_treat)
## maybe str(cow_treat) ; plot(cow_treat) ...
}
\keyword{datasets}
