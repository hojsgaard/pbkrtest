gsview <- gsv <- function(filename){
  sss <- sprintf("gsview64 %s.pdf", filename)
  #print(sss)
  shell(sss, wait=FALSE)
}

sumatrapdf <- spdf <- function(filename){
  sss <- sprintf("sumatrapdf %s.pdf", filename)
  #print(sss)
  shell(sss, wait=FALSE)
}

pdflatex <- function(filename){
  sss <- sprintf("pdflatex %s", filename)
  #print(sss)
  shell(sss, wait=FALSE)
}
