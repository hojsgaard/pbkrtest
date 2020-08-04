\name{fixed2Zcor}
\Rdversion{1.1}
\alias{fixed2Zcor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Construct zcor vector (of fixed correlations) from a fixed working
  correlation matrix
}
\description{
  Construct zcor vector (of fixed correlations) from a fixed working
  correlation matrix, a specification of clusters and a specifcation of
  waves. 
}
\usage{
fixed2Zcor(cor.fixed, id, waves)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cor.fixed}{
    Matrix
}
  \item{id}{
    Clusters
}
  \item{waves}{
    Vector giving the ordering of observations within clusters.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A vector which can be passed as the zcor argument to geeglm. 
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
  Søren Højsgaard <sorenh@agrsci.dk>
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{genZcor}}, \code{\link{geeglm}}
}
\examples{

timeorder <- rep(1:5, 6)
tvar      <- timeorder + rnorm(length(timeorder))
idvar <- rep(1:6, each=5)
uuu   <- rep(rnorm(6), each=5)
yvar  <- 1 + 2*tvar + uuu + rnorm(length(tvar))
simdat <- data.frame(idvar, timeorder, tvar, yvar)
head(simdat,12)

simdatPerm <- simdat[sample(nrow(simdat)),]
simdatPerm <- simdatPerm[order(simdatPerm$idvar),]
head(simdatPerm)

cor.fixed <- matrix(c(1    , 0.5  , 0.25,  0.125, 0.125,
                      0.5  , 1    , 0.25,  0.125, 0.125,
                      0.25 , 0.25 , 1   ,  0.5  , 0.125,
                      0.125, 0.125, 0.5  , 1    , 0.125,
                      0.125, 0.125, 0.125, 0.125, 1     ), nrow=5, ncol=5)
cor.fixed

zcor <- fixed2Zcor(cor.fixed, id=simdatPerm$idvar, waves=simdatPerm$timeorder)
zcor

mod4 <- geeglm(yvar~tvar, id=idvar, data=simdatPerm, corstr="fixed", zcor=zcor)
mod4


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{regression}

