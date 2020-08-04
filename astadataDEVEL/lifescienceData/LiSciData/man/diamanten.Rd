\name{diamonds}
\alias{diamonds}
\docType{data}
\non_function{}

\title{Diamonds}
\usage{data(diamonds)}
\description{
Characterisation of diamonds.
}
\format{
  This data frame contains:
  \describe{
       \item{lydefrihed:}{degree of flawlessness; 
           IFV: very very slightly imperfect; VS: very slighly imperfect }
       \item{klarhed:}{colour of diamond: E the lightest colour,
               H the darkest colour.}
       \item{karat:}{weight of the diamond in carat.}
       \item{pris:}{price of diamond in Singapore dollars}
       \item{logpris:}{log(price)}
}}
\details{The data are a reduction of the original data.
Only diamonds of carat between 0.3 and 0.7 and colours E to H
are considered. lydefrihed (in the original data called clarity)
 is condensed to IFV for catageories IF, VVS1  and VVS2, and VS subsumes VS1 and VS2.
}
\source{Singfat Chu (2001) \emph{Journal of Statistics education},
Vol. 9,\url{http://www.amstat.org/publications/JSE/jse_data_archive.html} 
}

\references{
Jens Ledet Jensen (2009) Et Nanokurs i Statistik, chapter 10.
}

\examples{	
data(diamonds)
}
\keyword{datasets}
