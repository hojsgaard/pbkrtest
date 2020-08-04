savePlotList <- function(plotList, sdir=tempdir(),type = c("wmf", "emf", "png", "jpeg", 
    "jpg", "bmp", "ps", "eps", "pdf")){
  type <- match.arg(type)  
  file.remove(list.files(sdir,full.names=TRUE))
  for (ii in 1:length(plotList)){
    replayPlot(plotList[[ii]])
    fn <- paste(sdir,ii + 1000,sep="/")
    savePlot(fn, type=type)
  }
}

makeMovie <- function(plotList, opt=NULL, filename="movie.gif"){
  sdir <- tempdir()
  savePlotList(plotList,sdir=tempdir(),type="png")
  ss1<- paste(sdir, "*",sep="/")
  ss2<- paste("convert", opt, " ", ss1, filename)
  print(ss2)
  shell(ss2,shell="cmd")
}
