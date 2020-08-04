\name{sinclair}
\alias{sinclair}
\docType{data}
\non_function{}

\title{Wildebeest [gnus]}
\usage{data(sinclair)}
\description{
Wildbeest [dansk=gnu] carcasses from the Serengeti were cross-classified by sex, cause of death and and bone marrow type.
The bone marrow type \emph{solid white fatty} indicates a healthy animal not undernourished.
}
\format{
  This data frame contains:
  \describe{
       \item{sex:}{male, female}
       \item{marrow:}{swf=solid white fatty, og= opaque gelatinous, tg= translucent gelatinous}
       \item{death:}{pred: killed by predator, npred: non-predator death cause}
       \item{count:}{number of wilbeests}
  }}
\details{Logistic regression, log-linear model, three-way table}
\source{
Sinclair, A.R.E and Arcese, P. (1995)
Population consequences of of predation-sensitive foraging: the Serengeti wildebeest [gnu],
Ecology, 76, pp. 882--891
}

\references{
Quinn, Gerry. P. and Keough, Michael J. (2002) \emph{Experimental Design and Data Analysis for Biologists}, University Press, Cambridge
Table 14-3 and Box 14-5
}

\examples{
data(sinclair)
tab<-xtabs(count~death+sex+marrow,data=sinclair)
print(ftable(tab))
}
\keyword{datasets}
