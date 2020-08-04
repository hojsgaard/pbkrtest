
toOneFile <- function(infile,fig=T,tex="latex"){
  hasIncludeInput <- function(s){
    l <- c(strsplit(s,"\\\\include\{"),strsplit(s,"\\\\input\{"))
    l[[which.max(lapply(l,length))]]
  }

  str <- scan(infile,"",sep="\n",blank.lines.skip=FALSE)
  infileName <- infile
  infile <- unlist(strsplit(infile,"\\."))
  outfile <- paste(infile[1],"-one.",infile[2],sep="")
  ext <- infile[2]
  cat("toOneFile: Outfile:",outfile,"\n")
  write(paste("%  Automatically generated file - DO NOT EDIT MANUALLY"),file=outfile)
  write(paste("%  All input in file", infileName," is merged into this file "),
        file=outfile,append=T)
  write(paste("%  using toOneFile function\n"),file=outfile,append=T)
  for (s in str){
    sin <- hasIncludeInput(s) #unlist(strsplit(s,c("\\input","\\include")))
    if (length(sin)>1){ # then it is an input statement
      if(length(grep ("%",sin))==0){
        infile <- gsub(c("[\{,\}, ]"),"",sin[2])
        getext <- unlist(strsplit(infile,"\\."))
        if (length(getext)>1){
        }
        else
          infile <- paste(infile,".tex",sep="") ### OK
        
        print(infile)         
        infilestr <- scan(infile,"",sep="\n",blank.lines.skip=FALSE)
        
        dirinfo  <- unlist(strsplit(infile,"/"))
        if (length(dirinfo)>1 && fig==TRUE){
          newfig <- paste("{",dirinfo[1],"/fig/",sep="")
          infilestr<- gsub("{fig/",newfig,infilestr)
        }
        write(paste("\n%\% Input from file\n%", infile), file=outfile, append=T)
        write(infilestr,  file=outfile, append=T)
        write(paste("% Input from file", infile, "completed!\n"), file=outfile, append=T)
      }
    } else {
      write(s,file=outfile,append=T)
    }
  }
  if (ext=="tex" & tex!=FALSE){
    if (tex == "pdf") 
      f2 <- paste("pdflatex", outfile)
    else 
      f2 <- paste("latex", outfile)
    system(f2, invisible = TRUE,show.output.on.console=F)
    cat("LaTeX'ing done...\n")
  }
  return(outfile)
}



extractSource <- function(infile){
  #infile <- paste(infile,".tex",sep="")
  ## From infile, which is a tex-file, the source (anything between begin{document}
  ## and end{document}) is extracted to the file infile-source
  head    <- gsub("\\..+","",infile)
  tail    <- gsub(".+\\.","",infile)
  prefix  <- paste(head,"-mrg.",sep='')
  outfile <- paste(head,"-source.",tail,sep='')
  cat("Main file:", infile, "\n")
  str   <- scan(file=infile, what="character",sep="\n",blank.lines.skip=FALSE)
  begin <- max(c(grep("begin{document}",str),grep("maketitle",str),
                 grep("tableofcontents",str)
                 ))+1
  end   <- min(grep("end{document}",str))-1
  range <- begin:end
  
  write(paste("%%%\n%%% Automatically generated file from ",infile,
              "- DO NOT EDIT MANUALLY\n%%%"),
        file=outfile,append=FALSE)
  write(str[range],file=outfile, append=T)
  cat("Created source file", outfile, "from", infile,"\n")
  return(outfile)
}

# weaveAll <- function(infile,tex="latex"){
#   inf <- unlist(strsplit(infile,"\\."))
#   if (length(inf)==1){
#     cat("Assuming", infile, "is really", paste(infile, ".Rnw\n",sep=""))
#     infile <- paste(infile,".Rnw",sep="")
#   }
#   if (unlist(strsplit(infile,"\\."))[2]!="Rnw")
#     stop("allWeave needs a .Rnw file...")

#   head <- strsplit(infile,"\\.")[[1]][1]
  
#   cat("Infile:",infile,"\n")
#   out <- toOneFile(infile)
#   cat("--Outfile:",out,"\n")
#   Stangle(out)
#   Sweave(out)
#   extractSource(out)

#   file.rename(gsub(".Rnw",".tex",out),gsub("-one.Rnw",".tex",out))
#   file.rename(gsub(".Rnw",".R",out),gsub("-one.Rnw",".R",out))
  
#   if (tex != FALSE) {
#     if (tex == "pdf") 
#       f2 <- paste("pdflatex", gsub("-one.Rnw",".tex",out))
#     else 
#       f2 <- paste("latex", gsub("-one.Rnw",".tex",out))
#     system(f2, invisible = TRUE,show.output.on.console=F)
#     cat("LaTeX'ing done...\n")
#     extractSource(gsub("-one.Rnw",".tex",out))
#   }
#   cat("Status:\n")
#   cat("Input file", infile," processed.\n")
#   cat("All sorce files are collected in",out,"\n")
# }


