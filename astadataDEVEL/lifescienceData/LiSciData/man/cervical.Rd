\name{cervical}
\alias{cervical}
\docType{data}
\non_function{}

\title{Cervical cancer deaths}
\usage{data(cervical)}
\description{
The deaths from cervical cancer of women and the thousand women-years at risk are
given for four age groups and four Eurpean countries for the year between 1969 and 1973.
}
\format{
  This data frame contains:
  \describe{
       \item{country:}{European country}
       \item{age:}{age group}
       \item{deaths:}{number of deaths from cervical cancer}
       \item{womenyear:}{1000 womenyear at risk, i.e the total time divided by 1000 that all women of a country belong 
to the corresponding age-group.
           }
  }}
\details{
}
\source{
Whitttemore, A.S. and Gong G. (1991) Poisson regression with misclassified 
counts: application to cervical cancer mortality rates,
\emph{JRSS, C}, 40, 81-93
        }
\references{
Lindsey, J.K. (2002) \emph{Modelling Frequency and Count Data}, Oxford: Claredon Press, Exercise 7.10(3), p. 179
}

\examples{
data(cervical)
}
\keyword{datasets}
