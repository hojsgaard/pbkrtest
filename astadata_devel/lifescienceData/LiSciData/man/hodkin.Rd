\name{hodkin}
\alias{hodkin}
\docType{data}
\non_function{}
\title{Damage of carrots by carrotr fly larvae}
\usage{data(hodkin)}

\description{
T4 cells/\eqn{mm^3} in blood samples from 20 patients in
remission from Hodgkin's disease and 20 patients in remission
from disseminated malignancies.
}
\format{
   Data frame containing
  \describe{
    \item{hodk:}{nuber of cells T4 per \eqn{mm^3} from patients with Hodkin disease}
    \item{nonhodk:}{nuber of cells T4 per \eqn{mm^3} from patients with Non-Hodkin disease}
  }
}
\details{
}
\source{
Altman, D. G. (1991) \emph{ Practical Statistics for Medical Research}, Chapman and Hall, London, p. 199
}

\references{
Lindsey, J.K. and Jones, B. (1998) \emph{Choosing among generalized linear models applied to medical data} 
Statistics in Medicine, 17, p. 59-68
}


\examples{
data(hodkin)
#reshape the data into long fromat
hodkinL<-reshape(hodkin,direction='long',varying=list(c('hodk','nonhodk')),v.names='ncell',times=c('yes','non'),timevar='hodkin')
hodkinL<-transform(hodkinL,hodkin=factor(hodkin))
}





\keyword{datasets}
