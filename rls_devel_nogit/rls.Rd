\name{rls}
\alias{rls}
\alias{print.rls}
\alias{predict.rls}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rls(x, y = NULL, tvar=NULL, degree = 1, lambda = 0.99, b=NULL, Pi=NULL, na.action, optim = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{tvar}{ ~~Describe \code{y} here~~ }
  \item{degree}{ ~~Describe \code{degree} here~~ }
  \item{lambda}{ ~~Describe \code{lambda} here~~ }
  \item{b}{ ~~Describe \code{y} here~~ }  
  \item{Pi}{ ~~Describe \code{y} here~~ }
  \item{na.action}{ ~~Describe \code{na.action} here~~ }
  \item{optim}{ ~~Describe \code{optim} here~~ }
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
function (x, y = NULL, degree = 1, lambda = 0.8, na.action, optim = TRUE) 
{
    if (is.null(y)) {
        y <- as.numeric(x)
        x <- seq_along(y)
    }
    if (optim) {
        f <- function(lam) {
            ans <- rlsfit(x, y, lam = lam, degree = degree)
            e <- ans[, "et1"]
            sum(e[!is.na(e)]^2)
        }
        optpar <- optim(lambda, f, method = "L-BFGS-B", lower = 0, 
            upper = 1)$par
        cat("optpar:", optpar, "\n")
        ans <- rlsfit(x, y, lam = optpar, degree = degree)
    }
    else {
        ans <- rlsfit(x, y, lam = lambda, degree = degree)
    }
    class(ans) <- "rls"
    return(ans)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
