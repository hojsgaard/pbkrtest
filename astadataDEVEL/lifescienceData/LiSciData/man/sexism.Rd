\name{sexism}
\alias{sexism}


\non_function{}
\title{Attitude to sexism}
\usage{data(sexism)}
\description{
  A study was conducted to compare the sexist attitudes of students
  at various types of colleges in the US. The colleges-types are:
  mixed (gender) college with at least 75\% male students, mixed
  college with less than 75\% male students, and single sex college.
  For each gender, random samples of each 10 undergraduate students
  were selected from each of the three types of colleges. Each student
  filled in a questionnaire, from which a score for `degree of
  sexism'-defined as the extent to which a student considered males
  and females to have diffeerent life roles-was determined
  (the higher the score, the more sexist the attitude). 
\cr
ANOVA, 2-way
}
\format{
   Data frame containing
  \describe{
    \item{sexism:}{Score for degree of sexism}
    \item{type:}{Categorical variable identifying the college type
      (1: mixed with more than 75\% males, 
      2: mixed with  less then 75\% males, 3: single sex)} 
    \item{gender:}{gender of respondent; male or female}
  }
}
\source{
 Kleinbaum, D.G., Kupper, L.L., Muller, K.E. and Nizam, A., (1998), Applied Regression Analysis and Other Multivariable Methods Duxbury Press, Brooks/Cole, p. 549. 
        }
\references{
}
\examples{
data(sexism)
}

\keyword{datasets}
