\name{insectdose}
\alias{insectdose}
\docType{data}
\non_function{}
\title{Symptoms for insetcs }
\usage{data(insectdose)}
\description{
Insects are expose to different doses of two preparations of an insecticide: a standard and a test preparation.}
\format{
   Data frame containing
  \describe{
	\item{prep:}{preparation: stand:standard, test:test preparation}
    \item{dose:}{dose of insecticide}
	\item{symptoms:}{Symptoms the insects show after 1 h of exposition: None, Mild, Severe}
	\item{count:}{counts of insectst for the symptom categoriers}
}}
\details{Porportional odds}
\source{\url{http://support.sas.com/documentation/cdl/en/statug/63962/HTML/default/viewer.htm#statug_probit_sect035.htm}
Example 74.2 Multilevel Response for probit models, online documentation of SAS}
\references{
}

\examples{
data(insectdose)
}





\keyword{datasets}
