\name{kyphosis}
\alias{kyphosis}
\title{kyphosis in children after laminectomy surgery}
\description{
Kyphosis is the forward flexion of the spine of at least 40 degrees from vertical following a corrective spinal surgery.

For 83 childen potential risk factor as age, starting vertrebrae level of the surgery and number of vetrebrae levels were recorded.
}
\usage{data(kyphosis)}
\format{
A dataframe with 4 variables. 
\describe{
\item{kyphosis:}{absent, present}
\item{age:}{age of patient in months}
\item{number:}{number of vertebrae levels involved}
\item{start:}{starting level of vertebrae level of the surgery}
}
}


\source{
Bell, D., Walker, J., O'Connor, G., Orrel, J., Tibshirani, R. (1989)
\emph{Spinal deformation following multi-level thoracic and lumbar 
laminectomy in children}
}
\references{
Hasti, T.J, Tibshirani, R.J. (1990) \emph{Generalized Additive Models}, Chapman and Hall : London
}


\examples{
data(kyphosis)
attach(kyphosis)
}


\keyword{datasets}




