\name{waistloss}
\alias{waistloss}


\non_function{}
\title{Effect on weigth loss by participatin on a 'waist loss' program}
\usage{data(waistloss)}
\description{
The weights of 20 men were recorded before and after 
participation in a 'waist loss' program
}
\format{
   Data frame containing
  \describe{
    \item{id:}{id number of participant}
    \item{before:}{weight before start of program}
    \item{after:}{weight at the end of the program}
  }
}
\details{
Data are from the exercise 2.5-2.2 of A. Dobson
}
\source{
Dobson, A. (2002) \emph{An introduction to Generalized Linear Models},
London: Chapman \& Hall, 2nd edition
        }
\references{
}
\examples{
data(waistloss)
attach(waistloss)
diff.weight<-after-before
boxplot(diff.weight)
abline(h=0,col=2)
title('weight difference after-before')
}





\keyword{datasets}
