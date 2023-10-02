\name{minerswheeze}
\alias{minerswheeze}
\docType{data}
\title{Breathing problems of coal miners}
\description{
  For for coal miners aged
  between 20 and 64 it was recorded whether  they suffered from
  breathlessness and whether they sufferend from wheezing, i.e.
  difficulty in breathing usually with a whistling sound.
    \cr
  logistic model, log linear model with two binary and one
  multicategrorical variable
}
\usage{data(minerswheeze)}
\format{
A dataframe with 3 variables. 
\describe{
\item{age:}{9 age groups= 20-24, 25-29, ...,60-64}
\item{breathless:}{yes or no}
\item{wheeze:}{wheezing: yues or no}
\item{number:}{persons observed}
}
}

\source{
Ashford, J.R. and Sowden, R.R (1970) Multivariate probit analysis,
Biometrics, 26, 535-546
}

\references{
Krzanowski, W. (1998) An Introduction to Statistical Modelling,
Arnold, London, p. 213, Table 7.3  
}

\examples{
data(minerswheeze)
}
\keyword{datasets}




