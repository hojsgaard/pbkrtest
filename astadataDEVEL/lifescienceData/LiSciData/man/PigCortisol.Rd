\name{PigCortisol}
\alias{PigCortisol}
\docType{data}
\title{Diurnal rythm of cortisol in pigs on two different treatments }
\description{
In an experiment investigating the effect of stress on pigs (control
vs. intermittent stress), the cortisol level
in the blood was sampled every half hour from 12 to 36 hours after implantation of
a catheter. Cortisol is a hormone involved in the stress response.
The pigs were blocked according to litter.
}
\usage{data(PigCortisol)}
\format{
  A data frame with 900 observations on the following 7 variables.
  \describe{
    \item{\code{time}}{Time in hours post operation, a numeric vector}
    \item{\code{cortisol}}{The cortisol level, a numeric vector}
    \item{\code{sint}}{sin(time*2*pi/24), a numeric vector}
    \item{\code{cost}}{cos(time*2*pi/24), a numeric vector}
    \item{\code{pig}}{The ID of the pig, a factor with a level for each pig }
    \item{\code{litter}}{The Litter of the pig, a factor with levels \code{1},...,\code{8}}
    \item{\code{treat}}{The treatment of the pig, a factor with levels \code{1} \code{2}}
  }
}
\details{}
\source{
Jensen, K.H., Pedersen, L.J., Heller, K.E., Ladewig, J.
	and Jørgensen, E. (1996). Intermittent Stress in pigs: effects on
behavior, pituarity-adrenocortical axis, growth and gastric ulceration.
Physiology \& Behaviour, (59), p 741-748,
}
\references{
}
\examples{
data(PigCortisol)
with(PigCortisol,plot(time, cortisol,log="y"))
}
\keyword{datasets}
