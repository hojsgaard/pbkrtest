\name{leafcalcium}
\alias{leafcalcium}
\docType{data}
\title{Calcium contents of four leaves four times measured}
\description{
For four leaves four times their 
calcium content was determined.
}
\usage{data(leafcalcium)}
\format{
A dataframe with 3 variables. 
\describe{
\item{leaf}{id of leaf}
\item{nr}{id of measurement within leaf}
\item{ca}{calcium measurment}
}
}

\source{
}

\references{
}



\examples{
library(lattice)
data(leafcalcium)
dotplot(leaf~jitter(ca),groups=nr,data=leafcalcium,xlab='calcium')
}
\keyword{datasets}




