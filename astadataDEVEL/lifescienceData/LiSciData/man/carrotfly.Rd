\name{carrotfly}
\alias{carrotfly}
\docType{data}
\non_function{}
\title{Damage of carrots by carrot fly larvae}
\usage{data(carrotfly)}
\description{
Wheatley and Freeman (1982) analysed an experiment on the 
impact of two different insecticides, applied in different soil depths, 
on the damage of carrots by carrot fly larvae.
Additionally to the two insecticides two control experiments without applied insecticides were performed.
The experiment was repeated three times.

From each plot on which a treatment had been applied a number 
of carrots were chosen, washed and classified as damaged or not.
}
\format{
   Data frame containing
  \describe{
    \item{insecticide:}{diazinon, disulfoton, control}
    \item{depth:}{depth the insecticide was applied [cm]}
    \item{dam1:}{number of damaged carrots in replicate 1}
    \item{exam1:}{number of examined carrots in replicate 1}
    \item{dam2:}{number of damaged carrots in replicate 2}
    \item{exam2:}{number of examined carrots in replicate 2 }
    \item{dam3:}{number of damaged carrots in replicate 3}
    \item{exam3:}{number of examined carrots in replicate 3}
  }
}
\details{
}
\source{
Wheatley and Freeman (1982) \emph{
A method of using the proportions of undamaged carrots or 
parsnips to estimate the relative population densities of carrot fly (Psila rosae) larvæ, and its practical applications}, Annals of Applied Biology, p. 229-244 
        }
\references{
James W. Hardin, Joseph M. Hilbe,(2007) \emph{Generalized Linear Models and Extensions}, 2nd edition, Stata Press,
Texas, p. 79
}


\examples{
data(carrotfly)
#reshape the data into long fromat
carrot<-reshape(carrotfly,direction='long',varying=list(c('dam1','dam2','dam3'),
                           c('exam1','exam2','exam3')),
                     v.names=c('dam','exam'),
                     times =c(1,2,3),timevar='replicate')
#removing the id variable
carrot<-subset(carrot,select=-id)
}





\keyword{datasets}
