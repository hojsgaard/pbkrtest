\name{linearAlgebra}
\alias{.spur}
\alias{.spurAB}
\alias{.fatAB}
\alias{.fatBL}
\alias{.orthComplement}
\title{Linear Algebra}
\description{
  Several function for linear algebra calculations.
}
\usage{
	 .spur(U)
	 .spurAB(A,B)
	 .fatAB(A,B)
	 .fatBL(B,L)
	 .orthComplement(W)
     	 }
\arguments{
  \item{U}{square matrix}
  \item{A}{a X b matrix}
  \item{B}{b X a matrix}
  \item{L}{a X l matrix}
  \item{W}{k X r matrix}
}
\details{
  \code{.spur} returns the trace of a square matrix, \code{.spurAB} the trace of a product,
  \code{.fatAB} the matrix \code{L} such that \code{<A>= \{ B b |  Lb=0 \}} ,
  \code{.fatBL} the matrix \code{A} such that \code{<A>= \{ B b |  Lb=0 \}} .
  and \code{.orthComplement} returns the orhogonal complement of <W>.
}
\seealso{
  \code{\link{save}}.
}
\examples{
     ## save all data
	 x<-3
	 }
\keyword{file}
