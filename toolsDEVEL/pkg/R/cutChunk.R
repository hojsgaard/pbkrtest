##require(SASRweave)

sasexec <- '"C:/Program Files/SAS Institute/SAS/V8/sas.exe"'

SASw <- function(file)  SASweave(file, sasexec)
SASRw <- function(file) SASRweave(file, sasexec, latex=T)

cutChunk <- function(infile){
  head    <- gsub("\\..+","",infile)
  tail    <- gsub(".+\\.","",infile)  
  if (tail!="R") 
    stop("File must have extension .R")
  cat("Main file:", infile, "\n")
  str <- scan(file=paste(head,".R",sep=''), what="character",sep="\n",
              blank.lines.skip=FALSE)
  outfile <- tempfile("tmpR",tmpdir=".")
  write(paste("###\n### Automatically generated file from ",infile,"\n###"),
        file=outfile,append=FALSE)
  for (i in 1:length(str)){
    s <- str[i]
    #print(s)
    tokens <- c("###","bPlot","ePlot","bwrapPlot","ewrapPlot")
    v <- sapply(tokens, grep, s)
    v <- sum(sapply(v,length))
    
    Rsrc.only <- ifelse(v==0,TRUE,FALSE)
    if (Rsrc.only){
      write(s, file=outfile, append=T)
    }
  }  
  file.rename(outfile,infile)   
}



