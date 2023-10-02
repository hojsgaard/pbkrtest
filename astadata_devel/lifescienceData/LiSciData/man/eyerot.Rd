\name{eyerot}
\alias{eyerot}
\docType{data}
\non_function{}

\title{eyerot}
\usage{data(eyerot)}
\description{
In an experiment to control the attack of cereals by
the fungus \emph{Pseudocercosporella her-potri-choides [dansk=knækkefodssyge]}  10 treatments were tested 
in a complete block-trial with 4 replications.
For treatments 2 to 10 pesticides of varying type and concentration were used where
treatment 1 was the control treatment with no application of a pesticide.

From each parcel about 50 plants were taken the infestation of  their stems were evaluated.


(knækkefodssyge =et snyltesvampe-angreb på planter, især kornarterne. 
Kendetegnet ved, at kun de nederste stængeldele og rodhalsen angribes. 
De vigtigste er gold-fodsyge, der fremkaldes af kulkernesvamp, 
angreb af hvededræbersvampen, og knækket fodsyge, forårsaget af øjepletsvampen.
Either of two fungal diseases that affect grasses and are characterized by oval lesions.)
}
\format{
  This data frame contains:
  \describe{
       \item{treat:}{treatment, 1= no application of pesticide, 2 to 10: treatments using pesticides}
       \item{block:}{block number, 1 to 4}
       \item{score:}{score giving the degree of infestation with fungus: 1= no infestation,
             2=low (less than 30\% of the circumference of a stem is infested, where infestation was strongest),
             3= moderate (more the 50\% of circumference is infested, tissue is fast), 4= the whole
               circumference is infested, tissue is decayed.}
       \item{freq:}{the number of animals found in each category}
  }}
\details{log-linear model, ordinal respone, proportional odds}
\source{
Jørgensen, L. N. (1986) : Knækkefodsyge (Pseudocercosporella her-potri-choides) 
- typeopdeling og resistensundersrgelser for ben-zimidazoler og prochloraz. 
Nordisk Planteværnskonference 1986 : 93-102.
}

\references{
Søren Andersen (1989) Statistiske Analyse af Tælledata, Matematisk Institut, KVL
p. 52, Kapitel 8, Eks. 1
}

\examples{
data(eyerot)
tab<-xtabs(freq~treat+block+score,data=eyerot)
print(ftable(tab,row.vars=1))
}
\keyword{datasets}
