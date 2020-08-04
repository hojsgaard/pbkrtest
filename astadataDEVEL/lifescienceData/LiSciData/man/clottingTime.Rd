\name{clottingTime}
\alias{clottingTime}
\title{clotting time of blodd}
\description{
mean clotting times in [s] of blood
for 9% concentration of normal plasma (pasmaconc)
and two lots of clotting agents
}
\usage{data(clottingTime)}
\format{
A dataframe with 3 variables. 
\describe{
\item{conc:}{plasma concentration}
\item{lot1:}{clotting agent 1}
\item{lot2:}{clotting agent 2}
}
}


\source{
Hurn, M.W., Barker, N.W. and Magath, T.D. (1945) \emph{ 
The determination 
of prothrombin time following the administration of
dicumarol with
specific reference to thromboplastin}, J. Lab Clin. Med., 30, 432-447
}
\references{
McCullagh, P.  and  Nelder, J. A. (1989) \emph{Generalized Linear Models}, London, U.K.: Chapman & Hall, p. 300-302
}


\examples{
data(clottingTime)
attach(clottingTime)

}

\keyword{datasets}




