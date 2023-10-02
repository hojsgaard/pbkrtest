\name{NIRmilk}
\alias{NIRmilk}
\docType{data}
\non_function{}

\title{NIRmilk}
\usage{data(NIRmilk)}
\description{
  Near infra red light  (NIR) measurments are made at 152 wavelengths on 17 milk samples. 
  While milk runs through a glass tube, infra red light is sent through
  the tube and the amount of light passing though the tube is measured
  at different wavelengths.
  Each milk sample was additionally analysed for fat, lactose, protein
  and drymatter.
}
\format{
  This data frame contains 18 rows and 158 columns.
  The first row contains the variable names, sample (=sample number),
  Xwww the infra red light amount at wavelength www,
  and the variables fat, protein, lactose and drymatter (dm).
}
\details{
  PCA regression
}
% \source{
%   SHD
% }

\references{
}

\examples{	
data(NIRmilk)
}
\keyword{datasets}
