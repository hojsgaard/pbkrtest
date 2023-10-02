\name{spruce}
\alias{spruce}
\non_function{}
\title{Log-size of 79 Sitka spruce trees}
\usage{data(spruce)}
\description{
The \code{spruce} data frame has 1027 rows and 6 columns. The data
consists of measurements on 79 sitka spruce trees over two growing
seasons. The trees were grown in four controlled environment chambers,
of which the first two, containing 27 trees each, were treated with
introduced ozone at 70 ppb whilst the remaining two, containing 12 and
13 trees, were controls.

}
\format{
  This data frame contains the following columns:
  \describe{
    \item{chamber}{a numeric vector of chamber numbers}
    \item{ozone}{a factor with levels \code{enriched} and \code{normal}}
    \item{id}{a numeric vector of tree id}
    \item{time}{a numeric vector of the time when the measurements were
      taken, measured in days since Jan. 1, 1988}
    \item{wave}{a numeric vector of the measurement number}
    \item{logsize}{a numeric vector of the log-size}
  }
}
\source{
  Diggle, P.J., Liang, K.Y., and Zeger, S.L. (1994) Analysis of Longitudinal
  Data, Clarendon Press.
}
\examples{
data(spruce)
spruce$contr <- ifelse(spruce$ozone=="enriched", 0, 1)
sitka88 <- spruce[spruce$wave <= 5,]
sitka89 <- spruce[spruce$wave > 5,]
fit.88 <- geese(logsize ~ as.factor(wave) + contr +
                          I(time/100*contr) - 1,
                id=id, data=sitka88, corstr="ar1")
summary(fit.88)

fit.89 <- geese(logsize ~ as.factor(wave) + contr - 1,
                id=id, data=sitka89, corstr="ar1")
summary(fit.89)
}
\keyword{datasets}
