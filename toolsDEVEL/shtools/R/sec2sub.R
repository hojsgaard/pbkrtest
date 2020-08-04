

sec2sub <- function(infile){
  str <- scan(infile,"",sep="\n",blank.lines.skip=FALSE)

  str <- gsub("\\\\subsubsection","\\\\paragraph",str)
  str <- gsub("\\\\subsection","\\\\subsubsection",str)
  str <- gsub("\\\\section","\\\\subsection",str)

  infile <- unlist(strsplit(infile,"\\."))
  outfile <- paste(infile[1],"-sub.",infile[2],sep="")
  ext <- infile[2]
  write(str,outfile)
}
