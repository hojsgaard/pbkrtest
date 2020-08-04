\name{readExcel}
\alias{readExcel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read Execel file}
\description{
Reading an Excel spreadsheet file into an R data frame
}
\usage{
readExcel(file, sheet = "Ark1")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{Filename}
  \item{sheet}{Sheet in the spread sheed, default is "Ark1"}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  A data frame
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
\examples{
\dontrun{
readExcel2("SomeData.xls", sheet="Sheet1")
}
}
\keyword{utilities}% at least one, from doc/KEYWORDS
