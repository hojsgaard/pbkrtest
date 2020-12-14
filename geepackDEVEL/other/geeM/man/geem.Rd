\name{geem}
\alias{geem}
\alias{geeM-package}
\alias{geeM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit Generalized Estimating Equations
}
\description{
Calculate coefficients and nuisance parameters using generalized estimating equations.  Link and Variance functions can be specified by the user.  Similar to \code{\link{glm}}.
}
\usage{
geem(formula, id, waves=NULL, data = parent.frame(), family = gaussian,
 corstr = "independence", Mv = 1, weights = NULL, corr.mat = NULL, init.beta = NULL,
 init.alpha = NULL, init.phi = 1, scale.fix = FALSE, nodummy=FALSE, sandwich = TRUE, 
 useP = TRUE, maxit = 20, tol = 1e-05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
a formula expression similar to that for \code{\link{glm}}, of the form \code{response~predictors}.  An offset is allowed, as in \code{glm}.
}
  \item{id}{
a vector identifying the clusters. By default, data are assumed to be sorted such that observations in a cluster are in consecutive rows and higher numbered rows in a cluster are assumed to be later.  If NULL, then each observation is assigned its own cluster.
}
  \item{waves}{
an integer vector identifying components of a cluster. For example, this could be a time ordering. If integers are skipped within a cluster, then dummy rows with weight 0 are added in an attempt to preserve the correlation structure (except if \code{corstr = "exchangeable"} or \code{"independent"}). This can be skipped by setting \code{nodummy=TRUE}.
}
  \item{data}{
an optional data frame containing the variables in the model.
}
  \item{family}{
will determine the link and variance functions.  The argument can be one of three options: a \code{family} object, a character string, or a list of functions.

For more information on how to use \code{family} objects, see \code{\link{family}}.
  
If the supplied argument is a character string, then the string should correspond to one of the family objects.

In order to define a link function, a list must be created with the components \code{(LinkFun, VarFun, InvLink, InvLinkDeriv)}, all of which are vectorized functions.  If the components in the list are not named as

\code{(LinkFun, VarFun, InvLink, InvLinkDeriv)}, then \code{geem} assumes that the functions are given in that order.  LinkFun and VarFun are the link and variance functions. InvLink and InvLinkDeriv are the inverse of the link function and the derivative of the inverse of the link function and so are decided by the choice of the link function.
}
  \item{corstr}{
a character string specifying the correlation structure.  Allowed structures are: \code{"independence"}, \code{"exchangeable"}, \code{"ar1"}, \code{"m-dependent"}, \code{"unstructured"}, \code{"fixed"}, and \code{"userdefined"}.  Any unique substring may be supplied.  If \code{"fixed"} or \code{"userdefined"}, then \code{corr.mat} must be specified.  If \code{"m-dependent"}, then \code{Mv} is relevant.
}
  \item{Mv}{
for \code{"m-dependent"}, the value for \code{m}.
}
\item{weights}{
  A vector of weights for each observation.  If an observation has weight 0, it is excluded from the calculations of any parameters.  Observations with a \code{NA} anywhere (even in variables not included in the model) will be assigned a weight of 0.  Note that these weights are now the same as PROC GEE weights and not PROC GENMOD.
}
  \item{corr.mat}{
the correlation matrix for \code{"fixed"}.  Matrix should be symmetric with dimensions >= the maximum cluster size.  If the correlation structure is \code{"userdefined"}, then this is a matrix describing which correlations are the same.
}
  \item{init.beta}{
an optional vector with the initial values of beta.  If not specified, then the intercept will be set to \code{InvLink(mean(response))}.  \code{init.beta} must be specified if not using an intercept.
}
  \item{init.alpha}{
an optional scalar or vector giving the initial values for the correlation.  If provided along with \code{Mv>1} or \code{unstructured} correlation, then the user must ensure that the vector is of the appropriate length.
}
  \item{init.phi}{
an optional initial overdispersion parameter.  If not supplied, initialized to 1.
}
  \item{scale.fix}{
if set to \code{TRUE}, then the scale parameter is fixed at the value of \code{init.phi}.
}
  \item{nodummy}{
  if set to \code{TRUE}, then dummy rows will not be added based on the values in \code{waves}.
  }
  \item{sandwich}{
if \code{TRUE}, calculate robust variance.
}
  \item{useP}{
  if set to \code{FALSE}, do not use the n-p correction for dispersion and correlation estimates, as in Liang and Zeger. This can be useful when the number of observations is small, as subtracting p may yield 
  correlations greater than 1.
  }
  \item{maxit}{
maximum number of iterations.
}
  \item{tol}{
tolerance in calculation of coefficients.
}
}
\details{
Users may specify functions for link and variance functions, but the functions must be vectorized functions.  See \code{\link{Vectorize}} for an easy way to vectorize functions.  \code{Vectorize} should be used sparingly, however, as it can lead to fairly slow function calls.  Care must be taken to ensure that convergence is possible with non-standard functions.

Offsets must be specified in the model formula, as in glm.

For the \code{"userdefined"} correlation option, the function accepts a matrix with consecutive integers.  \code{geem} only looks at the upper triangle of the matrix.  Any entry given as 0 will be fixed at 0.  All entries given as 1 will be assumed to be the same as each other and will be assumed to be possibly different from entries with a 2, and so on.

If observations are dropped because they have a weight of 0, then the denominator for the moment estimates of the correlation matrices are calculated using the number of non-zero Pearson residuals for the correlation structures \code{unstructured}, \code{userdefined} and \code{m-dependent} with \code{Mv>1}.  Therefore residuals numerically equal to 0 may cause problems in the calculation of correlation parameters.
}
\value{
An object of class "geem" representing the fit.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Lee McDaniel and Nick Henderson
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{glm}}, \code{\link{formula}}, \code{\link{family}}
}
\examples{
### Generated Negative Binomial Data
generatedata <- function(beta,alpha,gamma,X,T,n)  {
     mean.vec <- exp(crossprod(t(X),beta))
     y <- matrix(0,nrow=n,ncol=T)
     y[,1] <- rnbinom(n,mu = mean.vec[1],size=mean.vec[1]/gamma)
     for (i in 1:n)  {
        for (t in 2:T)  {
           innovation.mean <- mean.vec[t] - alpha*(sqrt(mean.vec[t]*mean.vec[t-1]))
           I <- rnbinom(1,mu= innovation.mean,size= innovation.mean/gamma)           
           first.shape <- alpha*sqrt(mean.vec[t]*mean.vec[t-1])/gamma
           second.shape <- mean.vec[t-1]/gamma - first.shape
           u <- rbeta(1,shape1 = first.shape,shape2=second.shape)
           a <- rbinom(1,size=y[i,t-1],prob=u)
           y[i,t] = a + I
        }
     }
     longform <- c(t(y))
     print(apply(y,2,mean))
     simdata <- data.frame(count = longform, time = rep(X[,2],times=n),subject=rep(c(1:n),each=T))
     return(simdata)
}

X <- cbind(rep(1,5),c(-.5,-.25,0,.25,.5))
testdat <- generatedata(beta=c(1,.5),alpha=.2,gamma=.5,X=X,T=5,n=3000)
far1 <- geem(count~ time, id=subject ,data = testdat, family=poisson, 
	corstr="ar1")

### Ohio respiratory data from geepack
if(require(geepack)){
	data("ohio", package="geepack")
	resplogit <- geem(resp ~ age + smoke + age:smoke, id=id, data = ohio, family = binomial,
	corstr = "m-dep" , Mv=1)
  
  LinkFun <- function(arg){qcauchy(arg)}
  InvLink <- function(arg){pcauchy(arg)}
  InvLinkDeriv <- function(arg){dcauchy(arg)}
  VarFun <- function(arg){arg*(1-arg)}
  FunList <- list(LinkFun, VarFun, InvLink, InvLinkDeriv)
  
  respcauchit <- geem(resp ~ age + smoke + age:smoke, id=id, data = ohio, family = FunList,
  corstr = "m-dep" , Mv=1)
}

### Seizure data from geepack
if(require(geepack)){
	data("seizure", package="geepack")
	seiz.l <- reshape(seizure,
	varying=list(c("base","y1", "y2", "y3", "y4")),
		v.names="y", times=0:4, direction="long")
		seiz.l <- seiz.l[order(seiz.l$id, seiz.l$time),]
		seiz.l$t <- ifelse(seiz.l$time == 0, 8, 2)
		seiz.l$x <- ifelse(seiz.l$time == 0, 0, 1)

	seiz <- geem(y~ x + trt + x:trt+ offset(log(t)), id=id,data = seiz.l, 
	family = poisson, corstr = "exchangeable")
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ robust }% __ONLY ONE__ keyword per line
