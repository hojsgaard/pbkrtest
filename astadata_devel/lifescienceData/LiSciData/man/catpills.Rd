\name{catpills}
\alias{catpills}
\docType{data}
\title{Palatibility of pill.}
\description{
Four diffeerent formulation of a drug were tested for palatibility on 27 cats.
On days 1,2,3,4,8,9,10 and 11 the degree of accept of the drug was recorded
}
\usage{data(catpills)}
\format{
A dataframe with  variables.
\describe{
\item{catid:}{cat id}
\item{sex}{sex of cat, 1: make, 0: female}
\item{breed}{breed of cat}
\item{weight}{weight of a cat at the begin of the study}
\item{age}{age of a cat at the begin of the study}
\item{fi}{formulation of the drug at day i}
\item{yi}{score at day i: 1=readily accepted, 2:accepted, 3: reluctantly accepted, 4: accepted in food, 5: refused}
}
}

\source{
}

\references{
Hand, David and Crowder, Martin (1996)  Practical Longitudinal Data Analysis, 
Chapman and Hall, Example 10.1, page 146
}




\examples{
data(catpills)  
catL<-reshape(catpills,direction='long',
     varying=list(paste('f',c(1:4,8:11),sep=''),
                  paste('y',c(1:4,8:11),sep='')),
     v.names=c('form','score'),timevar='day',
     idvar='catid')
catL$u<-ifelse(catL$form  <=3 ,'accept','NONaccept')




}
\keyword{datasets}	




