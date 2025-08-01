% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_info_functions.R
\name{get_ddf_Lb}
\alias{get_ddf_Lb}
\alias{get_Lb_ddf}
\alias{get_Lb_ddf.lmerMod}
\alias{Lb_ddf}
\alias{get_ddf_Lb.lmerMod}
\alias{ddf_Lb}
\title{Adjusted denominator degrees of freedom for linear estimate for linear
mixed model.}
\usage{
get_Lb_ddf(object, L)

\method{get_Lb_ddf}{lmerMod}(object, L)

Lb_ddf(L, V0, Vadj)

get_ddf_Lb(object, Lcoef)

\method{get_ddf_Lb}{lmerMod}(object, Lcoef)

ddf_Lb(VVa, Lcoef, VV0 = VVa)
}
\arguments{
\item{object}{A linear mixed model object.}

\item{L}{A vector with the same length as \code{fixef(object)} or a matrix
with the same number of columns as the length of \code{fixef(object)}}

\item{V0, Vadj}{The unadjusted and the adjusted covariance matrices for the fixed
effects parameters. The unadjusted covariance matrix is obtained with
\code{vcov()} and adjusted with \code{vcovAdj()}.}

\item{Lcoef}{Linear contrast matrix}

\item{VVa}{Adjusted covariance matrix}

\item{VV0}{Unadjusted covariance matrix}
}
\value{
Adjusted degrees of freedom (adjustment made by a Kenward-Roger
approximation).
}
\description{
Get adjusted denominator degrees freedom for testing Lb=0 in a
linear mixed model where L is a restriction matrix.
}
\examples{

(fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
(fm0 <- lmer(Reaction ~ 1 + (Days|Subject), sleepstudy))
anova(fm1, fm0)

KRmodcomp(fm1, fm0)  ## 17 denominator df's
get_Lb_ddf(fm1, c(0, 1)) ## 17 denominator df's

# Notice: The restriction matrix L corresponding to the test above
# can be found with
L <- model2restriction_matrix(fm1, fm0)
L

}
\references{
Ulrich Halekoh, Søren Højsgaard (2014)., A Kenward-Roger
Approximation and Parametric Bootstrap Methods for Tests in Linear Mixed
Models - The R Package pbkrtest., Journal of Statistical Software,
58(10), 1-30., \url{https://www.jstatsoft.org/v59/i09/}
}
\seealso{
\code{\link{KRmodcomp}}, \code{\link{vcovAdj}},
\code{\link{model2restriction_matrix}},
\code{\link{restriction_matrix2model}}
}
\author{
Søren Højsgaard, \email{sorenh@math.aau.dk}
}
\keyword{inference}
\keyword{models}
