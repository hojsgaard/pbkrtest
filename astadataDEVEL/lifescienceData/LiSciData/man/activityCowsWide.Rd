\name{activityCowsWide}
\alias{activityCowsWide}
\docType{data}
\title{Daily activity of cows.}
\description{
Twenty loggers placed on twenty cows transmit automatically daily activity measures to a PC. 
The data contain additional information about the farms on which the measurements were taken.
The data are in wide format as the three  activity measurments for each cow are given in one row.
For the long-format where the activity measurments are given in one variable see
\code{\link{activityCowsLong}}
}
\usage{data(activityCowsWide)}
\format{
A dataframe with  8 variables.
\describe{
\item{logger:}{id of logger}
\item{cow:}{id of cow}
\item{farm:}{id of farm}
\item{owner:}{Owner of the farm}
\item{type:}{Production type of the farm}
\item{day1:}{activity measurement for day 1}
\item{day2:}{activity measurement for day 2}
\item{day3:}{activity measurement for day 3}
}}

\source{
}

\seealso{\code{\link{activityCowsLong}}}

\references{

}




\examples{
data(activityCowsWide)  
}
\keyword{datasets}	




