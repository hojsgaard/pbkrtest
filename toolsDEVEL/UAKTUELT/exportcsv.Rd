\name{exportcsv}
\alias{exportcsv}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Function to export a dataframe to a comma separated file}
\description{
  Function to export a dataframe to a comma separated file
}
\usage{
exportcsv(data, file)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{data}{ Dataframe}
  \item{file}{ Filename }
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  TRUE
}

\author{ Søren Højsgaard, sorenh@agrsci.dk}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\note{ ~~further notes~~ }

\seealso{ \code{\link{importcsv}} }

\examples{
#data(dietox)
#exportcsv(dietox, file="mydietox.csv")
#mydietox <- importcsv(file="mydietox.csv")
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line


