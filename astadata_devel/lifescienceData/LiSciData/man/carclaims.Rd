\name{carclaims}
\alias{carclaims}
\docType{data}
\non_function{}
\title{Insurance claims for cars}
\usage{data(carclaims)}
\description{
The data provide the average claims for damage of an owner's car (privately owned and
comprehensively insured). Besides the average claim the number of claims in different classes are given}
\format{
   Data frame containing
  \describe{
    \item{pa:}{age group of police holder: 1=17-20, 2= 21-24, 3= 25-29; 4=30-34,
                 5=35-39, 6=40-49, 7=50-59, 8= 60-}
    \item{cg:}{car group; A-D}
    \item{va:}{car's age 100-3, 2= 4-7, 3= 8-9, 4=10-}
    \item{y:}{average claim in pounds}
    \item{number:}{number of claims}
  }
}
\details{
}
\source{
Mc Cullagh, P.  and Nelder, J.A. \emph{Generalized Linear Models}, Chapman and Hall, 1989, 2nd edition, p. 298
        }
\references{Baxter, L.A. Coutts, S.M. and Ross, G.A.F. \emph{Applications of linear models
in motor insurance}, Proceedings of the 21st International
Congress of Actuaries, Zürich, pp. 11-29
}	
\examples{
data(carclaims)
}





\keyword{datasets}
