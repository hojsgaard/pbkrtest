\name{dose.LD50}
\alias{dose.LD50}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate LD50}
\description{
 Calculate the LD50 (the dose at which 50 pct of the subjects die) for a
 model of the form logit(p)=beta1 x1 + ... + betap xp + gamma d where
 none of the explanatory variables x1 ... xp contains the dose d.  
}
\usage{
dose.LD50(x, lambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A glm object (for logistic regression)}
  \item{lambda}{A vector of the same length as the number of parameters
    in x.}
}
\details{
 lambda contains an NA at the entry corresponding to dose d. The other
 entries of lambda must be the values of the covariates x1 ... xp at
 which the ld50 is to be calculated.
 }
\value{
A data frame
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard}
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
\examples{
data(budworm)
m1 <- glm(ndead/20 ~ sex + log(dose), data=budworm, weight=ntotal, family=binomial)
coef(m1)

dose.LD50(m1,c(1,1,NA))
dose.LD50(m1,c(1,0,NA))
}
\keyword{models}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
