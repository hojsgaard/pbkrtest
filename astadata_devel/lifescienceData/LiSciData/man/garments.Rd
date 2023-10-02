\name{garments}
\alias{garments}
\docType{data}
\non_function{}

\title{garments}
\usage{data(garments)}
\description{
Four materials (A, B, C and D) used to fabricate permanent-press garments are 
tested for weight loss and shrinkage.
The materials were placed in an heat chamber that had four 
control settings (also called positions).
The tests were performed in four runs, in each run all
4 materials were tested using all 4 positions.
  }
\format{
  This data frame contains:
  \describe{
       \item{run:}{1,2,3,4: number of run}
       \item{pos:}{1,2,3,4: heat chamber setting}
       \item{material:}{garment material}
       \item{wtloss:}{loss in weight of the garment}
       \item{shrink:}{shrinkage of the garment}
   }}
\details{linear regression, latin square}
\source{
Littel, R.C., Stroup, W.W.  and Freund, R.J. (2002) 
SAS for Linear Models, 4th edition,
output 3.23, page 72.
}

\references{
}

\examples{
data(garments)
pairs(garments)
}
\keyword{datasets}
