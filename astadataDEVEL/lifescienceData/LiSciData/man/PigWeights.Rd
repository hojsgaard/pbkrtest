\name{PigWeights}
\alias{PigWeights}
\docType{data}
\title{Weight and feed consumption of pigs}
\description{
Weight and feed consumption of 25 pigs on 5 different treatments
  in 4 replicates of (10,5,5,5) pigs. The replicates started at approximately
  weekly intervals }
\usage{data(PigWeights)}
\format{
  A data frame with 161 observations on the following 9 variables.
  \describe{
    \item{\code{Group}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5},
                       indicating the treatment the pig received}
    \item{\code{Pig}}{The identification of the pig}
    \item{\code{WeightNo}}{a numeric vector indicating the number of the weighing}
    \item{\code{Weight}}{The weight of the pig at the weighing, a numeric vector}
    \item{\code{ExpDays}}{Days since the experiment started, a numeric vector.
            Different replicates have different \code{ExpDays} when the
            pigs start in the experiment (\code{Days==0})
    }
    \item{\code{FUp}}{Feed intake since last weighing (Feed Unit pig), a numeric vector}
    \item{\code{Gain}}{Weight Gain since last weighing, a numeric vector}
    \item{\code{AccFUp}}{Accumulated feed intake since the pig started in the experiment, anumeric vector}
    \item{\code{Days}}{Days since the pig started in the experiment, a numeric vector}
  }
}
\details{


}
\source{ The data are from an experiment conducted by Niels Oksbjerg
}
\references{
}
\examples{

 library(lattice)
 data(PigWeights)
 with(PigWeights,xyplot(Weight~Days|Pig))
}
\keyword{datasets}
