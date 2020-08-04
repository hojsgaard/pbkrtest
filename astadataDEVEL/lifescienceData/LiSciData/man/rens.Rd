\name{rens}
\alias{rens}
\docType{data}
\non_function{}

\title{rens}
\usage{data(rens)}
\description{
Preference between a new washing powder 'new' and a standard washing powder 'old'.
Both washing powders were sent to 1008 consumers who were asked which 
powder they preferred, at which temperature they washed, the hardness of their water
and whether they formerly had used the standard washing powder 'old'.
}
\format{
  This data frame contains:
  \describe{
       \item{hardness:}{hardness of water: soft, medium, hard}
       \item{pref:}{preference of washing powder 'new' the consumer preffered 'new' to 'old',
                            'old': ther consumer preferred 'old' to 'new'.}
       \item{consumer:}{yes: the consumr had formerly usd the 'old' powder, 
                        no: the consumer never used the 'old' powder before}
       \item{temp:}{washing temperature: high, low}
       \item{count:}{number of consumers} 
  }}
\details{logistic regression, log-linear model, four-ways table}
\source{
Ries, P. N and Smith, H. (1963) The use of chi-square for preference testing in
multidimensional problems, Chemical engineering progress, 59, 39-43
}
\references{
Søren Andersen (1989) Statistiske Analyse af Tælledata, Matematisk Institut, KVL
p. 33, Kapitel 4, Eks. 3
}

\examples{
data(rens)
}
\keyword{datasets}
