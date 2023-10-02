\name{cisplatin}
\alias{cisplatin}
\non_function{}
\title{reaction on chemotherapy  with cisplatin}
\usage{data(cisplatin)}
\description{
The \code{cisplatin} data frame describe the severity of nausea
for patients under chemotherapy, receiving cisplatinum or not.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{cisplatinum:}{no: patient received no cisplatinum, yes: received }
    \item{severity:}{level of serum acid phosphatase in King-Amstrong units}
    \item{number}{number of persons with the symptoms}
  } 
}
\details{
}

\source{
Farewell(1982) A note on regression analysis of ordinal data with
variability of classification, Biometrika, 69, 533-538 
}

\references{
Lindsey, J. K (1995), Modelling Frequency and Count Data, Clarendon Press,
Oxford, p. 99 ,reprint 2002
}

\examples{
data(cisplatin)
#
library(MASS)


\dontrun{
* SAS example;
data prostaticCancer; 
infile "prostaticCancer.txt" firstobs=2;
input  age acid Xray Tsize  Tgrade  nodal;
proc print; run;
} 

}
\keyword{datasets}
