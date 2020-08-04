\name{rlsfit}
\alias{rlsfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rlsfit(xx, yy, tvar=xx, degree = 1, lambda = 0.99,  b=NULL, Pi=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xx}{ ~~Describe \code{xx} here~~ }
  \item{yy}{ ~~Describe \code{yy} here~~ }
  \item{tvar}{ ~~Describe \code{yy} here~~ }
  \item{b}{ ~~Describe \code{b} here~~ }
  \item{Pi}{ ~~Describe \code{Pi} here~~ }
  \item{lambda}{ ~~Describe \code{lam} here~~ }
  \item{degree}{ ~~Describe \code{degree} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (xx, yy, b, Pi, lam = 0.5, degree = 1) 
{
    lenx <- length(xx)
    J <- diag(1, degree + 1)
    if (degree == 0) {
        X <- matrix(1, nrow = lenx, ncol = 1)
    }
    else {
        if (degree == 1) {
            X <- cbind(1, xx)
        }
        else {
            X <- cbind(1, xx, xx^2)
        }
    }
    if (missing(Pi)) 
        Pi <- diag(100, degree + 1)
    if (missing(b)) 
        b <- rep(0, degree + 1)
    ans1 <- matrix(NA, nrow = lenx, ncol = length(b))
    ans2 <- matrix(NA, nrow = lenx, ncol = 4)
    colnames(ans2) <- c("yt1.hat", "yt.hat", "et1", "et")
    for (tt in 1:lenx) {
        xt <- X[tt, ]
        yt <- yy[tt]
        if (!is.na(yt)) {
            k <- (Pi \%*\% xt/lam)/as.numeric(1 + t(xt) \%*\% Pi \%*\% 
                xt/lam)
            yt1.hat <- as.numeric(t(xt) \%*\% b)
            et1 <- yt - yt1.hat
            b <- b + k * et1
            Pi <- (J - k \%*\% t(xt)) \%*\% Pi/lam
            yt.hat <- as.numeric(t(xt) \%*\% b)
            et <- yt - yt.hat
            ans1[tt, ] <- b
            ans2[tt, ] <- c(yt1.hat, yt.hat, et1, et)
        }
    }
    ans <- cbind(ans1, ans2)
    ans
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
