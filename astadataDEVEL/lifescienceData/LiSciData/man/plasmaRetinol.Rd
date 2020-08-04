\name{plasmaRetinol}
\alias{plasmaRetinol}
\docType{data}
\title{Determinants of Plasma Retinol and Beta-Carotene Levels}
\description{
Observational studies have suggested that low dietary intake or low plasma 
concentrations of retinol, beta-carotene, or other carotenoids might 
be associated with increased risk of developing certain types of cancer. 
 However, relatively few studies have investigated the determinants of plasma concentrations of 
these micronutrients. We designed a cross-sectional study to investigate the relationship 
between personal characteristics and dietary factors, and plasma concentrations of retinol,
 beta-carotene and other carotenoids. Study subjects (N = 315) were patients who had an 
elective surgical procedure during a three-year period to biopsy or remove a lesion of the lung, 
colon, breast, skin, ovary or uterus that was found to be non-cancerous. 

Authorization: Contact Authors

Reference: These data have not been published yet but a related reference is
Nierenberg DW, Stukel TA, Baron JA, Dain BJ, Greenberg ER.  Determinants of plasma levels of beta-carotene and retinol.  American Journal of Epidemiology 1989;130:511-521.

Therese Stukel
Dartmouth Hitchcock Medical Center
One Medical Center Dr.
Lebanon, NH 03756
e-mail: stukel(a)dartmouth.edu
}
\usage{data()}
\format{
A dataframe with  14 variables.
\describe{
\item{age}{age in years}
\item{sex}{sex (1=Male, 2=Female)}
\item{smokstat}{Smoking status (1=Never, 2=Former, 3=Current Smoker)}
\item{bmi}{Quetelet's body mass index (\eqn{weight/height^2})}
\item{vituse}{Vitamin Use (1=Yes, fairly often, 2=Yes, not often, 3=No)}
\item{kcal}{Number of calories consumed per day. (kcal)}
\item{fat}{Grams of fat consumed per day}
\item{fiber}{Grams of fiber consumed per day}
\item{alcohol}{Number of alcoholic drinks consumed per week}
\item{cholesterol}{Cholesterol consumed (mg per day)}
\item{betadiet}{Dietary beta-carotene consumed (mcg per day)}
\item{retdiet}{Dietary retinol consumed (mcg per day)}
\item{betaplasma}{Plasma beta-carotene (ng/ml)}
\item{retplasma}{Plasma Retinol (ng/ml)}
}
}

\source{
Data are from lib-stat \url{http://lib.stat.cmu.edu/datasets/Plasma_Retinol}
}

\references{
James W. Hardin, Joseph M. Hilbe,(2007) \emph{Generalized Linear Models and Extensions}, 2nd edition, Stata Press,
Texas, p. 79
}




\examples{
data(plasmaRetinol)  
}
\keyword{datasets}	




