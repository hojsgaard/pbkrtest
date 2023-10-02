\name{geeglm}
\alias{geeglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fit Generalized Estimating Equations (GEE)}
\description{
  The geeglm function fits generalized estimating equations using the 'geese.fit' function 
  of the 'geepack' package for doing the actual computations. geeglm has a syntax similar to 
  glm and returns an object similar to a glm object. 
	An important feature of geeglm,
  is that an anova method exists for these models.
}
\usage{
geeglm(formula, family = gaussian, data=parent.frame(), weights, subset, 
                  na.action, start = NULL, etastart, mustart, offset,
                  control = geese.control(...), 
                  method = "glm.fit", x = FALSE, y = TRUE,
                  contrasts = NULL, 
                  id, waves=NULL, zcor=NULL, 
                  corstr = "independence",
                  scale.fix = FALSE,
                  scale.value =1, std.err="san.se",
                  ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{See corresponding documentation to \code{glm}}
  \item{family}{See corresponding documentation to \code{glm}}
  \item{data}{See corresponding documentation to \code{glm}}
  \item{weights}{See corresponding documentation to \code{glm}}
  \item{subset}{See corresponding documentation to \code{glm}}
  \item{na.action}{No action is taken. Indeed geeglm only works on
    complete data.}
  \item{start}{See corresponding documentation to \code{glm}}
  \item{etastart}{See corresponding documentation to \code{glm}}
  \item{mustart}{See corresponding documentation to \code{glm}}
  \item{offset}{See corresponding documentation to \code{glm}}
  \item{control}{See corresponding documentation to \code{glm}}
  %\item{model}{See corresponding documentation to \code{glm}}
  \item{method}{See corresponding documentation to \code{glm}}
  \item{x}{See corresponding documentation to \code{glm}}
  \item{y}{See corresponding documentation to \code{glm}}
  \item{contrasts}{See corresponding documentation to \code{glm}}

  \item{id}{a vector which identifies the clusters.  The length of `id'
          should be the same as the number of observations.  Data are
          assumed to be sorted so that observations on a cluster are
          contiguous rows for all entities in the formula. }
\item{waves}{Wariable specifying the ordering of repeated mesurements on the same unit. 
	Also used in connection with missing values. See examples below.}
\item{zcor}{Used for entering a user defined working correlation structure.}
  \item{corstr}{a character string specifying the correlation structure. The
          following are permitted: '"independence"', '"exchangeable"',
          '"ar1"', '"unstructured"' and '"userdefined"' }
  \item{scale.fix}{a logical variable; if true, the scale parameter is fixed at
          the value of 'scale.value'. }
  \item{scale.value}{numeric variable giving the value to which the scale
    parameter should be fixed; used only if 'scale.fix == TRUE'.}
  \item{std.err}{Type of standard error to be calculated. Defualt
    'san.se' is the usual robust estimate.  Other options are 'jack':
    if approximate jackknife variance estimate
          should be computed.
     'j1s': if 1-step jackknife variance estimate
          should be computed.
     'fij': logical indicating if fully iterated jackknife variance
          estimate should be computed. }
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  In the case of corstr="fixed" one must provide the zcor vector
  if the clusters have unequal sizes. 
  
  Clusters with size one must not be represented in zcor.
}
\value{
  An object of type 'geeglm'
}
\references{Liang, K.Y. and Zeger, S.L. (1986) Longitudinal data analysis
     using generalized linear models. Biometrika, *73* 13-22.

     Prentice, R.L. and Zhao, L.P. (1991). Estimating equations for
     parameters in means and covariances of multivariate discrete and
     continuous responses. Biometrics, *47* 825-839.}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{See the documentation for the 'geese' function 
  for additional information. geeglm only works for complete data. Thus
  if there are NA's in data you can specify data=na.omit(mydata).
}

\section{Warning }{
  Use "unstructured" correlation structure only with great care. (It may
  cause R to crash).
} 

\seealso{\code{\link{geese}}, \code{\link{glm}},\code{\link{anova.geeglm}} }
\examples{
data(dietox)
dietox$Cu     <- as.factor(dietox$Cu)
mf <- formula(Weight~Cu*(Time+I(Time^2)+I(Time^3)))
gee1 <- geeglm(mf, data=dietox, id=Pig, family=poisson("identity"),corstr="ar1")
gee1
summary(gee1)

mf2 <- formula(Weight~Cu*Time+I(Time^2)+I(Time^3))
gee2 <- geeglm(mf2, data=dietox, id=Pig, family=poisson("identity"),corstr="ar1")
anova(gee2)

}
\keyword{models}% at least one, from doc/KEYWORDS

