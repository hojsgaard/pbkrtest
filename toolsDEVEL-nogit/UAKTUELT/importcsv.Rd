\name{importcsv}
\alias{importcsv}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Function to import a comma separated file}
\description{
  Function to import a comma separated file
}
\usage{
importcsv(file)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{file}{CSV file to be imported}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  A dataframe
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Søren Højsgaard, sorenh@agrsci.dk}
%\author{ ~~who you are~~ }
%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{\code{\link{exportcsv}} }

\examples{
#data(dietox)
#exportcsv(dietox, file="mydietox.csv")
#mydietox <- importcsv(file="mydietox.csv")
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
