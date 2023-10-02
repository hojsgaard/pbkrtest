\name{KenwardRoger primitive function}
\alias{.KRmodcompPrimitive}
\title{Ftest and degrees of freedom based on Kenward-Roger approximation}
\description{
  An approximation Ftest based on the Kenward-Roger approximation fo the
  residuals degress of freedom and the scale F-statistic. 
}

\usage{
 .KRmodcompPrimitive(largeModel,L,beta0) 
}

\arguments{
  \item{largeModel}{A  lmer model fitted with REML=TRUE}
  \item{L}{A restriction matrix L}
  \item{beta0}{A number of a vector of the beta of the hypothesis, 
    e.g. L beta=beta0. beta0=0 if modelSmall is a model not a
    restriction matrix.}
}

\details{
  L is a restriction matrix, such that modelLarge is a lmer-model with  fixed
  effect model matrix X and fixed effects parameters beta, then
  {alpha| L beta=0} defines a submodel model0 of modelLarge with
  <X(model0)> in <X(modelLarge)>
}
\seealso{
  \code{\link{KRmodcomp},\link{lmer}}.
}
\examples{
     ## save all data
	 x<-3
	 }
\keyword{function}
