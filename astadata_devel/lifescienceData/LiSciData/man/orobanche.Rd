\name{orobanche}
\alias{orobanche}
\title{germination of orobache aegyptica}
\description{
For two varieties of seeds from \emph{Orobanche} 
(parasitic plant growing on the
roots of flowering plants)
their ability to germinate being exposed to
root extract frrom either bean or cumcumber was analysed.
For each factor combination several plates were prepared with
a root extract and seeded.
\\
 a 2x2 factorial experiment
\\
two types of seed: O. aegyptiaco 73 (O73)
                   O. aegyptiaco 75 (O75)
\\
two types of root extract
                  bean (b)
                  cucumber (c)
}
\usage{data(orobanche)}
\format{
A dataframe with 4 variables. 
\describe{
\item{extract:}{bean; cucumber}
\item{seed:}{o73; o75}
\item{gem:}{number seed germinated}
\item{total:}{total number of seeds per plate}
}
}

\source{
Cox & Snell (1989) \emph{Analysis of Binary data}, Chapman and Hall: London,  Example 3.2 p. 114
}
\references{
}


\examples{
data(orobanche)
attach(orobanche)

}

\keyword{datasets}




