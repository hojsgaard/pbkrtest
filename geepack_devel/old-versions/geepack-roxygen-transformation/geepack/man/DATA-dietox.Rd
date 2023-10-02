\name{dietox}
\alias{dietox}
\non_function{}
\title{Growth curves of pigs in a 3x3 factorial experiment}
\usage{data(dietox)}
\description{
The \code{dietox} data frame has 861 rows and 7 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Weight}{a numeric vector}
    \item{Feed}{a numeric vector}
    \item{Time}{a numeric vector}
    \item{Pig}{a numeric vector}
    \item{Evit}{a numeric vector}
    \item{Cu}{a numeric vector}
    \item{Litter}{a numeric vector}
  }
}
%\details{
% ~~ If necessary, more details than the description above ~~
%}
\source{
  Lauridsen, C., Højsgaard, S.,Sørensen, M.T. C. (1999) Influence of
Dietary Rapeseed Oli, Vitamin E, and Copper on Performance and
  Antioxidant and Oxidative Status of Pigs. J. Anim. Sci.77:906-916
}
%\references{
%~~ possibly secondary sources and usages ~~
%}
\examples{
data(dietox)
dietox$Cu     <- as.factor(dietox$Cu)
gee01 <- geeglm (Weight ~ Time + Cu + Cu * Time, id =Pig, data = dietox,
         family=gaussian,corstr="ex")

mf <- formula(Weight~Cu*(Time+I(Time^2)+I(Time^3)))
gee1 <- geeglm(mf, data=dietox, id=Pig, family=poisson("identity"),corstr="ar1")
summary(gee1)
anova(gee1)

}
\keyword{datasets}
