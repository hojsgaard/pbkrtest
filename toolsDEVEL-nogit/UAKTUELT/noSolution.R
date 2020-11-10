noSolution <- function(infile){
  # Removes solution environment from tex file
  inf <- infile
  infile <- paste(infile,".tex",sep="")
  str <- scan(infile,"",sep="\n",blank.lines.skip=FALSE)
  print(infile)
  outfile <- paste(inf,"-nosol.tex",sep="")
  
  begin <- grep("\\\\begin{solution}",str)
  end  <- grep("\\\\end{solution}",str)

  if(length(begin)>0){
    idx <- NULL
    for (i in 1:length(begin))
      idx <- c(idx,seq(begin[i],end[i]))
    newstr <- str[-idx]
  } else {
    newstr <- str
  }
  write(newstr,outfile)
  system(paste("pdflatex ", outfile)) 
}

prepEx <- function(){
  noSolution("shell")
  extractSource("shell-nosol.tex")
  extractSource("shell.tex")
  
}
