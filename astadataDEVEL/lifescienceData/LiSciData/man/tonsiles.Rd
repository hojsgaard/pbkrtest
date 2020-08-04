\name{tonsiles}
\alias{tonsiles}
\docType{data}
\non_function{}
\title{reaction on chemotherapy  with tonsiles}
\usage{data(tonsiles)}
\description{
Size of tonsiles for corries and non-carries of Streptococcus pygenes 
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{carrier:}{yes: patient carries 
Streptococcus pygenes; no: patient does not carry the bacterium}
    \item{size:}{size of tonsile:
                     0: present but not enlarged
                     1: enlarged
                     2: greatly enlarged}
    \item{n}{number of persons}
  } 
}
\details{
}

\source{
McCullagh, P. (1980) Regression models for ordinal data
JRSS,B, 109-142
}

\references{
}

\examples{
data(tonsiles)
#



\dontrun{
* SAS example;
data prostaticCancer; 
infile "prostaticCancer.txt" firstobs=2;
input  age acid Xray Tsize  Tgrade  nodal;
proc print; run;
} 

}
\keyword{datasets}
