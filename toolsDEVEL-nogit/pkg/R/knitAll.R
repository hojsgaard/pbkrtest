
knitAll <- function(file){
  knit( paste(file, "Rmd", sep="."), tangle=FALSE, encoding="latin1" )
  knit( paste(file, "Rmd", sep="."), tangle=TRUE,  encoding="latin1" )
  markdownToHTML( paste(file, "md", sep="."), paste(file, "html", sep=".") )
}

knitHTML <- function(file){
  knit( paste(file, "Rmd", sep="."), tangle=FALSE, encoding="latin1" )
  knit( paste(file, "Rmd", sep="."), tangle=TRUE,  encoding="latin1" )
  f1 <- paste(file, "md", sep=".")
  f2 <- paste(file, "html", sep=".")

  ## pandoc -s --mathjax -t slidy lecture.md -o lecture.html

  ss <- paste("pandoc -s --mathjax -t slidy", f1, "-o", f2)
  system(ss, invisible = FALSE, show.output.on.console = TRUE,
            wait = TRUE)
}

