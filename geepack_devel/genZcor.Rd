\name{genZcor}
\alias{genZcor}
\alias{humbelbee}
\title{genZcor}
\description{
  constructs the design matrix for the correlation structures:
  independence, echangeable, ar1 and unstructured
  The user will need this function only as a basis to construct a user defined
  correlation structure: use genZcor to get the design matrix Z  for
  the unstructured correlation and define the specific correlation structure by
  linear combinations of the columns of Z.

}
\usage{
genZcor(clusz, waves, corstrv)
}

\arguments{
  \item{clusz}{integer vector giving the number of observations in each cluster}
  \item{waves}{integer vector, obervations in the same cluster with
           values of wave i and j have the correlation \eqn{latex}{sigma_ij}}
	 \item{corstrv}{correlation
	   structures: 1=independence,2=exchangeable,3=ar1,
	      4=unstructured}
}
% \details{
% }
\value{
  \item{}{the design matrix for the correlation structure}  
}
%\references{ a }
\author{Jun Yan \email{jyan.stat@gmail.com}}
% \note{  }



\seealso{\code{\link{fixed2Zcor}} }
\examples{
#example to construct a Toeplitz correlation structure
#    sigma_ij=sigma_|i-j|

#data set with 5 clusters and maximally 4 observations (visits) per cluster
 gendat <- function() {
       id <- gl(5, 4, 20)
       visit <- rep(1:4, 5)
       y <- rnorm(id)
       dat <- data.frame(y, id, visit)[c(-2,-9),]
}

set.seed(88)
dat<-gendat()

#generating the design matrix for the unstructured correlation
zcor <- genZcor(clusz = table(dat$id), waves = dat$visit, corstrv=4)
# defining the Toeplitz structure 
zcor.toep<-matrix(NA, nrow(zcor),3)
zcor.toep[,1]<-apply(zcor[,c(1,4,6)],1,sum)
zcor.toep[,2]<-apply(zcor[,c(2,5)],1,sum)
zcor.toep[,3]<-zcor[,3]

zfit1 <- geese(y ~ 1,id = id, data = dat,
                   corstr = "userdefined", zcor = zcor.toep)


zfit2 <- geeglm(y ~ 1,id = id, data = dat,
                   corstr = "userdefined", zcor = zcor.toep)


}
\keyword{ regression }% at least one, from doc/KEYWORDS
