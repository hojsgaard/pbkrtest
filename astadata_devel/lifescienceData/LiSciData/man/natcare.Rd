\name{natcare}
\alias{natcare}
\docType{data}
\title{Effect of antenatal care and clinic  infant survival}
\description{
  Number of babies that died in two clinics 
  classified according top the levels of antenatal care.
    \cr
 (non binary) logistic regression, log linear models
\cr
}
\usage{data(natcare)}
\format{
A dataframe with 4 variables. 
\describe{
\item{clinic:}{The two clinics A and B}
\item{care:}{The intensity of the antenatal care: 'low' and 'high'}
\item{survived:}{number of babies that survived}
\item{dead:}{number of babies that died}
}}

\source{
  Bishop, Y.M.M. (1969) Full contingency tables, logits and split contingency
tables. Biometrics, 25, 119-128, Table 7.2, p. 212
}

\references{
Krzanowski, Wojtek J. (1998)
An Introduction to Statistical Modelling
Arnold, London, 
}


\examples{
data (natcare)
}
            
\keyword{datasets}




