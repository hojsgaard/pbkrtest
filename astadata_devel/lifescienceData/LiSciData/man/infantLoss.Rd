\name{infantLoss}
\alias{infantLoss}
\docType{data}
\non_function{}
\title{Problemtic children and stillbirths}
\usage{data(infantLoss)}
\description{
In a   study for children classified by their teachers as
problematic (cases) and unproblematic(controls) the birth order of the child and the number of 
stillbirths (births with infant death) were recorded.
 The study was performed as a retrospective study, 
i.e.  one chose first the number of problematic/non-problematic children and then collected the additional information.
}
\format{
   Data frame containing
  \describe{
    \item{order:}{birth order of the child, e.g. 1-2: first or second child}
    \item{loss:}{stillbirth: yes=a child died at birth}
    \item{state:}{control or case (=child classified)}
    \item{n:}{number of children}
  }
}
\details{
}
\source{
Cochran (1959) Some methods for strengthening the common \eqn{\chi^2} tests, \emph{Biometrics} 10, 417-451
        }
\references{
Fienberg, Stephen f. (1980) \emph{The Analysis of Cross-Classified Categorical Data}, MIT Press, Cambridge, 2nd edition. p. 136 Table 7-7}

\examples{
data(infantLoss)
}





\keyword{datasets}
