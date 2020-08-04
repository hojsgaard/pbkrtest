\name{corn}
\alias{corn}
\docType{data}
\non_function{}

\title{corn}
\usage{data(corn)}
\description{
Data are collected relating corn yield (in bushels per acre) to
metereological variables for several years.
  }
  \format{
  \describe{
       \item{yield:}{corn yield in bushels per acre}
       \item{pre_seasonPreci:}{total precipitation (in inches) for the year prior to the start of the growing season}
       \item{may_emp:}{average daily temperature (in degrees Fahrenheit) for May}
       \item{june_rain:}{total rain (in inches) for June}
       \item{june_temp:}{average daily temperature (in degrees
	 Fahrenheit) for June}
       \item{july_rain:}{total rain (in inches) for July}
       \item{july_temp:}{average daily temperature (in degrees
	 Fahrenheit) for July}
       \item{aug_rain:}{total rain (in inches) for August}
       \item{aug_temp:}{average daily temperature (in degrees
	      Fahrenheit) for August}
   }}
\details{linear regression, model selection }
\source{
\url{http://support.sas.com/learn/statlibrary/statlib_eg4.1/linreg_4.htm#KB4}
}

\references{
}

\examples{
data(corn)
pairs(corn)
}
\keyword{datasets}
