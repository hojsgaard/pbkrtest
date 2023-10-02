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
    \item{Start}{a numeric vector}
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
str(dietox) 
plot(dietox)
\dontrun{
* SAS example;
proc print data=aaa;
run;
}

}
\keyword{datasets}
