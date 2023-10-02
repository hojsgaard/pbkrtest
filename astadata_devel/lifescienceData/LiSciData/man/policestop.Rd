\name{policestop}
\alias{policestop}
\docType{data}
\title{Stop by police.}
\description{
Over a 15 month period  in 1998-1999 the police in New York recorded the ethneticity of and the type of crime of the 
stopped person
}
\usage{data(policestop)}
\format{
A dataframe with   the  variables.
\describe{
\item{stops:}{number of persons stopped}
\item{pop:}{size of the population}
\item{past.arrests:}{number of persons arrested in the year before}
\item{precinct:}{id number of the precinct}
\item{eth:}{ethnicity: 1=black, 2=hispanic, 3=white}
\item{crime:}{crime type: 1=violent, 2=weapons, 3=property, 4=drug}
}}


\source{

}

\references{
Gelman, Andrew and Hill, Jennifer (2007), Data Analysis Using regression and Multilevel Hierrarchical Models
}




\examples{
data(policestop)  
}
\keyword{datasets}	




