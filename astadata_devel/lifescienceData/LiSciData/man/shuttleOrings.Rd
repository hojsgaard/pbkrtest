\name{shuttleOrings}
\alias{shuttleOrings}
\docType{data}
\non_function{}
\title{Failure of O-rings for shuttle flights}
\usage{data(shuttleOrings)}
\description{
The failure of gas sealing O-rings caused the explosion of the Challanger-shutttle shortly after launch.
}
\format{
   Data frame containing
  \describe{
    \item{nr:}{number of flight}
    \item{date:}{date of flight}
    \item{n:}{number of damage of the (primary) o-field rings out of 6 possible}
    \item{temp:}{average temperature (Fahrenheit)}
    \item{pres:}{Pressure applied to a gas for testing the sealing quaility of the rings}
  }
}
\details{
On January, 28, 1986, the shuttle Challanger (flight 61-I) exploded shortly after the start.
Later it was found  that 'a combustion gas leak' caused the catastrophe.
The leakage may by attributed to the failure of one or several of six O-rings, which task was to seal the 
field joints of the motor.

The night before the event engineers discussed the problem in a three hour telephone conference.
In the conference a plot of the failure of O-rings in former missions were discussed, but only data
for flights with failures were used, the flights without failures were considered not to contribute valuable data.
Later the Rogers Commission noted that this was a failure of a proper data analysis.  
}
\source{
Dalal, S.R., Fowlkes, E.D. and Hoadley, B. \emph{Risk Analysis of the Space Shuttle: Pre-Challanger Prediction of Future},
JASA, Vol. 84, p. 945-957}
\references{
}	
\examples{
data(shuttleOrings)
}





\keyword{datasets}
