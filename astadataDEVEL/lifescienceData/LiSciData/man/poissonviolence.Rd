\name{poissonviolence}
\alias{poissonviolence}
\docType{data}
\non_function{}
\title{Violence of teens}
\usage{data(poissonviolence)}
\description{
Data from 2006 that give the proportion of violent teens  aged betwen 15 and 17 for danish 
councils.
}
\format{
   Data frame containing
  \describe{
    \item{rang:}{rank of the council wrt. the ratio of violent teens}
    \item{kommune:}{name of the council}
    \item{nviolent:}{number if teens aged 15-17 that were sentenced for either severe or non-severe violence 
                     and threatening.}
    \item{ratio:}{number of violent teens per 1000 teens aged between 15-17}
    \item{diff:}{difference to the ratio of violent teens from 2003 to 2006}
}}
\details{In an article from 17.12.2006 in the Danish newspaper Politiken 
it is supposed that
there is a tendency that the small villages are those with the highest proportion of violent teens
(Title of the article \emph{Violent teens live in small province villages - not in
in the large city (Voldelige unge bor i små provinsbyer - ikke i storbyen)}) 
It is suggested that this might be due to families with low income moving to the small councils with 
cheaper accommodation. The data were collected by Danmarks Statistik.
The council Farsø Kommune has an institution for sentenced teens.
}
\source{
Article in Politiken, 17. December 2007,
\url{http://politiken.dk/indland/article447317.ece}
The data
\url{http://politiken.dk/archive/00225/Ungdomsvoldsamletli_225019a.doc}
}
\references{
A reference from Aalborg University:
\url{http://vbn.aau.dk/research/smaa_byer_topper_listen_over_voldelige_unge(13378246)}
and a message from TV2 \url{http://nyhederne-dyn.tv2.dk/article.php/id-9748236.html?rss}
}


\examples{
data(poissonviolence)
}





\keyword{datasets}
