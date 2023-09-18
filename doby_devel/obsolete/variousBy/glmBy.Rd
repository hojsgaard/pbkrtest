\name{glmBy}
\alias{glmBy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
glmBy(formula, family = gaussian, data, weights, subset, na.action, start = NULL, etastart, mustart, offset, control = glm.control(...), model = TRUE, method = "glm.fit", x = FALSE, y = TRUE, contrasts = NULL, group, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{ ~~Describe \code{formula} here~~ }
  \item{family}{ ~~Describe \code{family} here~~ }
  \item{data}{ ~~Describe \code{data} here~~ }
  \item{weights}{ ~~Describe \code{weights} here~~ }
  \item{subset}{ ~~Describe \code{subset} here~~ }
  \item{na.action}{ ~~Describe \code{na.action} here~~ }
  \item{start}{ ~~Describe \code{start} here~~ }
  \item{etastart}{ ~~Describe \code{etastart} here~~ }
  \item{mustart}{ ~~Describe \code{mustart} here~~ }
  \item{offset}{ ~~Describe \code{offset} here~~ }
  \item{control}{ ~~Describe \code{control} here~~ }
  \item{model}{ ~~Describe \code{model} here~~ }
  \item{method}{ ~~Describe \code{method} here~~ }
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{contrasts}{ ~~Describe \code{contrasts} here~~ }
  \item{group}{ ~~Describe \code{group} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
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
function (formula, family = gaussian, data, weights, subset,
    na.action, start = NULL, etastart, mustart, offset, control = glm.control(...),
    model = TRUE, method = "glm.fit", x = FALSE, y = TRUE, contrasts = NULL,
    group, ...) {

  if (missing(group)){
    stop("A grouping of data has not been specified")
  }
  mf<- match.call(expand.dots = TRUE)

  newdata <- eval(mf$data)
  newdata$.NUM. <- 1:nrow(newdata)
  mf$data <- newdata

  mf$modelClass <- 'glm'
  mf[[1]] <- as.name('fitBy')
  value <- eval(mf)
  attr(value,'data')<- newdata
  class(value) <- c('glmBy','fitBy')
  return(value)
  }
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
