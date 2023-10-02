\name{primulaMendel}
\alias{primulaMendel}
\docType{data}
\non_function{}
\title{Mendel's law and primulæ}
\usage{data(primulaMendel)}
\description{
  Are some simple primula features consistent with simple Mendelian law}
\format{
   Data frame containing
  \describe{
    \item{leaf:}{type of leaf  of a primula classified}
    \item{eye:}{type of eye of a classified primula}
    \item{n:}{number of plants classified}
  }
}
\details{
A total of 560 plants were classified by type of their leafs (flat or crimped) and the type of their eye
(normal or Primrose Queen). According to simple Mendelian law one would expect
a 3:1 ratio for the dominant to the  recessive type of each characteristic.
For leafs  420 plants would be expected to have
flat  leaves  and  140 plants crimped, and for the eye 420 normal and  140  Primrose Queen.
}
\source{Fisher, R. A. (1958) Statistical Methods for Research Workers. Edinburgh, Oliver and Boyd.
p. 82        }
\references{
Lindsey, J.K  (1995) \emph{Modelling Frequency and Count Data}, Oxford University Press, p. 52, table 2.12.
Mead. R, Curnow R.N., Hasted,  A.M. (1993) \emph{ Statistical Methods in Agriculture and Experimental Biology},
end edition, Chapman and Hall: London,  chapter 14.2, p. 302
}
\examples{
data(primulaMendel)
attach(primulaMendel)
}





\keyword{datasets}
