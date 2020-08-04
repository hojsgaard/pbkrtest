\name{milkyield}
\alias{milkyield}
\docType{data}
\title{
  Milk yield from dairy cows
}
\description{
  Milk yield from 100 dairy cows. Cows were milked by automatic milking
  systems (AMS), also sometimes called milking robots. 
}
\usage{data(milkyield)}
\format{
  A data frame with 83792 observations on the following 11 variables.
  \describe{
    \item{\code{cowno}}{a numeric vector}
    \item{\code{race.id}}{a factor with levels \code{RDM} \code{Holstein} \code{Jersey}}
    \item{\code{parity}}{a numeric vector}
    \item{\code{cowpar}}{a character vector}
    \item{\code{dfc}}{a numeric vector}
    \item{\code{yield}}{a numeric vector}
    \item{\code{log.cells}}{a numeric vector}
    \item{\code{interval}}{a numeric vector}
    \item{\code{visitres}}{a factor with levels \code{AC} \code{AI} \code{MI} \code{MC} \code{UN}}
    \item{\code{calv.date}}{a POSIXlt}
    \item{\code{amstime}}{a POSIXlt}
  }
}
\details{
  A cow can go and get milked whenever she wants to except that there
  has been imposed the restriction that there has to be at least 6 hours
  between milkings (so if a cow goes to the ams before that, then she is
  not milked and will leave again).
  
  The variable 'amstime' is the time when a milking ends. The 'interval'
  is the time since elapsed since the previous milking. 
  
%%  ~~ If necessary, more details than the __description__ above ~~
}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(milkyield)
## maybe str(milkyield) ; plot(milkyield) ...
}
\keyword{datasets}
