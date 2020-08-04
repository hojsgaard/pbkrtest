\name{kalk}
\alias{kalk}
\docType{data}
\non_function{}

\title{Kalk}
\usage{data(kalk)}
\description{
Experiment about the effectiveness of lime as a diet supplement.
10 men received a diet with added lime, 11 without.
Their blood pressure was measured at the  begin of the study and
12 week after.
}
\format{
  This data frame contains:
  \describe{
       \item{treatment:}{kalk (=lime added) or placebo}
       \item{before:}{blood presssure at start}
       \item{after:}{blood presssure 12 weeks later}
}}
\details{
Anova, paired
}
\source{
Blood prssure and metabolic effects of calcium supplementation
in normotensive white and black men, \emph{JAMA}, 1987, p. 1722-1776
}

\references{
Jens Ledet Jensen (2009) Et Nanokurs i Statistik, chapter 7, p. 71.
}

\examples{	
data(kalk)
}
\keyword{datasets}
