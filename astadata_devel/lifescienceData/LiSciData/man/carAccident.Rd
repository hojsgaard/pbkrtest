\name{carAccident}
\alias{carAccident}
\docType{data}
\non_function{}
\title{Injuries with car accidents}
\usage{data(carAccident)}
\description{
The  severeness of injuries was registered for car drivers together either the type of accdient and
whther the car driver had been ejected or not.}
\format{
   Data frame containing
  \describe{
	\item{injury:}{sebereness of injury: light/severe}
    \item{eject:}{Driver ejected from the car: yes/no}
	\item{actyp:}{Type of accident: collison (collis) or rollover (rollov)}
	\item{count:}{count of car drivers involved in an accident}
}}
\details{Log linear models}
\source{Kihlberg, Narragon and Campbell (1964)}
\references{
 Fienberg (1980) The Analysis of cross classified categogrical data
Christensen, R. (1990)  Log-Linear Models , Springer, Heidelberg, p. 75 example 3.2.4
}

\examples{
data(carAccident)
}





\keyword{datasets}
