\name{cow_yield}
\alias{cow_yield}
\docType{data}
\title{
  Milk yield recordings for for each miling on a larger set of cows.
}
\description{
    Milk yield recordings for for each miling on a larger set of cows.
}
\usage{data(cow_yield)}
\format{
  A data frame with 5000 observations on the following 4 variables.
  \describe{
    \item{\code{cow_no}}{Animal identifier - a numeric vector}
    \item{\code{milk_date}}{a Date}
    \item{\code{dfc}}{Days from calving - a numeric vector}
    \item{\code{yield}}{The yield per milking - a numeric vector}
  }
}
\details{
  It is not a bug that there recodings of a different number of milkings
  per day. Cows were milked in a miling robot and cows could go to the
  milking robot several times during a day
%%  ~~ If necessary, more details than the __description__ above ~~
}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(cow_yield)
## maybe str(cow_yield) ; plot(cow_yield) ...
}
\keyword{datasets}
