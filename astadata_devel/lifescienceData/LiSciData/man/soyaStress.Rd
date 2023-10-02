\name{soyaStress}
\alias{soyaStress}
\docType{data}
\non_function{}

\title{SoyaStress soyaStresss}
\usage{data(soyaStress)}
\description{
Experiment to analyse the influence of meachanical stress on the growth of 
soya bean plants.
52 seedlings were potted, one seedling per plot.
The seedlings were randomly splitted up into four groups of
13 plants.
In two groups the plants were shaken for 20 minutes daily.
The groups that received the same amount of stress were grown under two different light conditions.
After 16 days the plants were harvested and the total area of leaves
for each plant was measured.
}
\format{
  This data frame contains:
  \describe{
       \item{treat:}{1=low light, low stress, 2= low light hight stress,
                     3=high light, low stress, 4=high light, high stress}
       \item{area:}{Total leaf area}
       \item{light:}{low or high light level}
       \item{stress:}{no= not shaken; yes: shaken}
}}
\details{
}
\source{
Blæsild, Preben and Grandfeldt, Jørgen (2003) 
\emph{Statistics with applications in
Biology and Geology}, Chapman and Hall,
Example 3.3, pp. 96. 
}

\references{
}

\examples{	
data(soyaStress)
}
\keyword{datasets}
