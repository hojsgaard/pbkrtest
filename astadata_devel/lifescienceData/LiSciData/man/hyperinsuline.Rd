\name{hyperinsuline}
\alias{hyperinsuline}
\non_function{}
\title{obesity and hyperinsulinism}
\usage{data(hyperinsuline)}
\description{
The data are from an experiment analysing the relation between obesity
and hyperinsulinism. The plasma inorganic phosphate levels one hour 
after a standard glucose tolerance test were obtained for
hyperinsulinemic obese and  nemon-obese patients and a control group.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{state:}{yes: hyperinsulinemic obese patient;
                  no:  hyperinsulinemic non-obese patient;
                  control: controls}
    \item{phos:}{level of phosphate levels [mg/dl] one hour after
                 glucose tolerance test}
    \item{rep:}{number of repeated laboratory measurements of the same plasma 
                                      sample}
  } 
}
\details{
The original data  from Jones (1987) do not have the information on
repetitive laboratory determinations. They were only included for
exercise pupose. 
}

\source{
Jones, R.H. (1987) Serial correlations in unbalanced mixed models.
\emph{Bulletin International Statistical Institute} 52, book4, 105-122
}

\references{
Dobson, A. (1990) \emph{An introduction to Generalized Linear Models},
London: Chapman \& Hall, exercise 7.1
}

\examples{
data(hyperinsuline)
#
}

\keyword{datasets}
