.drawAxes <- function(){abline(h=0,col="blue");abline(v=0,col="blue")}
scoresPlot <- function(pca,scores=1:2, 
                       labels=dimnames(pca$scores)[[1]],col=1,cex=1)
{
  plot(pca$scores[,scores[1]],pca$scores[,scores[2]],col=col,
       xlab=paste("PC",scores[1]),ylab=paste("PC",scores[2]))
  .drawAxes()
  text(pca$scores[,scores[1]],pca$scores[,scores[2]], labels=labels,cex=cex,
       adj=c(0,1))
}
loadingsPlot <- function(pca,loadings=1:2,
                         labels=attr(pca$loadings,"dimnames")[[1]],col=2,cex=1)
{
  x.range<- range(pca$loadings[,loadings[1]])+c(-.1,.1)
  y.range<- range(pca$loadings[,loadings[2]])+c(-.1,0)
  plot(x.range,y.range,type="n",
       xlab=paste("PC",loadings[1]),ylab=paste("PC",loadings[2]))
  points(pca$loadings[,loadings[1]],pca$loadings[,loadings[2]],col=col,lwd=2)
  .drawAxes()
  text(pca$loadings[,loadings[1]],pca$loadings[,loadings[2]], labels=labels,
       cex=1, adj=c(0.5,1))
}
