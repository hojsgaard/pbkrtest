\name{miteapple}
\alias{miteapple}
\title{number of mites in apple tree leaves}
\description{
contrived data of mites counted on leaves of two varietes of apple trees
}
\usage{data(miteapple)}
\format{
A dataframe
\describe{
\item{count:}{counts of mites per leaf}
\item{varietey:}{A and B}
\item{size:}{size of a leaf in cm*cm}
}
}

\source{
contrived data, see examples
}



\examples{
# the data were generated as follows:
set.seed(89559)
variety<-rep(c('A','B'),c(50,50))
size<-20+rnorm(100)
logSize<-log(size)
rate<-ifelse(variety=='A',5,6)
loglambda<- logSize+ log(rate)  +rnorm(rate)*0.05
lambda<-exp(loglambda)
count<-rpois(length(lambda),lambda)
}
\keyword{datasets}




