\name{plotBy}
\alias{plotBy}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Function for making groupwise plots of y against x for subjects
  in different groups}
\description{
  Function for making groupwise plots of y against x for subjects
  in different groups.
}
\usage{
plotBy(x, y = NULL, subject = NULL, group = NULL, data = parent.frame(), gFun = NULL, Fun = NULL, title = "PLOT", lines = FALSE, same.axis = TRUE, group.col = NULL, group.pch = NULL, group.lty = NULL, silent = TRUE, xlim = range(x), ylim = range(y), ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{The x variables or a formula of the form y~x}
  \item{y}{If x is a formula, say z~x, then y becomes the z from the
    formula. y defaults to NULL in which case x is
    plottet against its index (i.e. 1:length(x)) }
  \item{subject}{ variable identifying the subjects }
  \item{group}{ vector of names identifying grouping variables}
  \item{data}{ data frame}
  \item{gFun}{ user defined funcion to be applied to data group
    wise, see examples below}
  \item{Fun}{ user defined funcion to be applied to data subject
    wise, see examples below }
  \item{title}{ title of each plot }
  \item{lines}{ if TRUE points from the same subject are joined by lines }
  \item{same.axis}{ if TRUE the same axis are used for all plots}
  \item{group.col}{ colours to be used for the groups}
  \item{group.pch}{ symbols to be used for the groups}
  \item{group.lty}{ line types to be used for the groups}
  \item{silent}{ if FALSE the colours, symbols etc. are printed}

  \item{xlim}{ Limits on the x-axis}
  \item{ylim}{ Limits on the x-axis}

  \item{\dots}{ graphical parameters such as par, col, lty}

}
\details{
 Colours, plotting characters and linetypes can be specified accoriding
 to levels of variables. See Examples below for an example.
}
\value{
Returns a data frame containing information about the colors, linetypes
etc. used in the plots.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ }
%~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{ SEE ALSO
  \code{\link{histBy}},
  \code{\link{qqnormBy}},
  \code{\link{summaryBy}}
}

\examples{
##  Plot of weight against time for each subjects within groups

data(dietox)
dietox$Cu     <- as.factor(dietox$Cu)
dietox$Evit   <- as.factor(dietox$Evit)

par(mfrow=c(3,3),omi=c(0,0,.5,0))

plotBy(Time,Weight,subject=Pig,group=c(Evit,Cu),title="Evit*Cu=", data=dietox,group.pch=1:10, col=1:100)
mtext("Plot of weight against Time",outer=TRUE)

plotBy(Weight~Time,subject=Pig,group=c(Evit,Cu),title="Evit*Cu=", data=dietox,group.pch=1:10, col=1:100)
mtext("Plot of weight against Time",outer=TRUE)

plotBy(Time,Weight,subject=Pig,group=c(Evit,Cu),title="Evit*Cu=",lines=TRUE,group.col=1:30,pch=1:30, 
    data=dietox, silent=FALSE)
mtext("Plot of weight against Time, subjects connected by lines",outer=TRUE)

plotBy(Time,Weight,subject=Pig,group=c(Evit,Cu),title="Evit*Cu=", 
     data=dietox, lines=TRUE, lty=Cu,
     col=list(Evit,c("black","orange","blue")), pch=Cu, silent=FALSE)

plotBy(Time,Weight,subject=Pig,group=c(Evit,Cu),title="Evit*Cu=",lines=TRUE,group.col=1:30,pch=1:30, 
    data=dietox, lwd=3, lty =4)

plotBy(Time,Weight, group=c(Evit,Cu),title="Evit*Cu=",lines=FALSE,group.col=1:30,pch=1:30,data=dietox)

plotBy(Weight,subject=Pig,group=c(Evit,Cu),title="Evit*Cu=",lines=TRUE,col=1:30,
data=dietox)

plotBy(Weight,subject=c(Cu,Pig),group=Evit,title="Evit*Cu=",lines=TRUE,col=1:30, data=dietox)

group.f        <- function(){
    s<-lm(group.y~group.x+I(group.x^2)); 
    lines(group.x[order(group.x)], s$fitted[order(group.x)],lwd=3,col=1)
    cat("Coefficients :", s$coef, "Mean :", mean(group.y), fill=TRUE)
}
subject.f   <- function(){
    r<-spline(subject.x,subject.y);
    lines(r$x,r$y,col=3,lty="dashed")
}
plotBy(Time,Weight,subject=Pig,group=c(Evit,Cu),title="Evit*Cu =", col=1:30,lines=FALSE, lwd=2, 
    gFun="group.f",Fun="subject.f",data=dietox )
mtext("Weight against Time with mean curve",outer=TRUE)

par(mfrow=c(1,1))

plotBy(Weight,subject=Pig,title="Plot of all pigs",lines=TRUE,col=1:300, data=dietox)
}
\keyword{ hplot}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
