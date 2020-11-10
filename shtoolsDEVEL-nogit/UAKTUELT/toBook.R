
toBook <- function(mainFile,tex="latex"){
  ext <- ".tex"
  header <- strsplit(mainFile,"\\.")[[1]][1]
  str <- scan(mainFile,what="",sep="\n")
  str2 <- sapply(str, function(s) pmatch(c("\\input","\\include"), s))
  idx <- which(apply(str2,2,any))
  str3 <- str[idx]
  
  filnavn <- str3
  dirnavn <- str3
  for (i in 1:length(str3)){
    s <- str3[i]
    s <- gsub("\\\\input{","",s)
    s <- gsub("\\\\include{","",s)
    s <- gsub("}","",s)
    ss <- strsplit(s, "/")
    s <- ss[[1]][1]
    filnavn[i] <- ss[[1]][2]
    dirnavn[i] <- s
  }
  
  str[idx] <- paste("\\input{", paste(paste(dirnavn,filnavn,sep="/"),
                                      "-path.tex",sep=''),"}")
  write(str,paste(header,"-path.tex",sep=""))
  
  oldFiles <- paste(dirnavn,filnavn,sep="/")
  newFiles <- paste(oldFiles,"-path",sep="")
  oldFiles <- paste(oldFiles,ext,sep="")
  newFiles <- paste(newFiles,ext,sep="")

  ##cat("Old files:\n"); print(oldFiles)
  ##cat("New files:\n"); print(newFiles)
  
  for (f in 1:length(oldFiles)){
    oldF <- oldFiles[f]
    newF <- newFiles[f]

    if (file.exists(newF)){
      dtime <- as.numeric(file.info(newF)$mtime-file.info(oldF)$mtime)
      if (dtime>0){
        cat(paste("file",newF, "is updated\n"))
        doIt <- FALSE
      } else {
        cat(paste("file",newF,"needs to be updated\n"))
        doIt <- TRUE
      }      
    } else {
      cat(paste("file",newF, "does not exist - creating it\n"))
      doIt <- TRUE
    }

    if (doIt){
      cat("Current file:", oldF, "\n")
      ff <- scan(oldF,what="",sep="\n")
      
      write("% Automatically generated file - do not modify manually",file=newF)
      write(paste("% Created from", oldF), file=newF, append=T)
      for (i in 1:length(ff)){
        ss <- ff[i]
        if (length(grep("\\includegraphics",ss)) > 0){
          print(s)
          beg <- strsplit(ss,"{")[[1]][1]
          fil <- strsplit(ss,"{")[[1]][2]
          news <- paste(beg,"{", dirnavn[f],"/", fil,sep="")
          print(news)
          write(news, file=newF,append=T)
        } else {
          write(ss, file=newF,append=T)
        }        
      }
    }

  }
  

  if (tex == "pdf")
    sss <- paste("pdflatex ", paste(header,"-path.tex",sep=""))
  else 
    sss <- paste("latex ", paste(header,"-path.tex",sep=""))

  system(sss, show.output.on.console=TRUE)
}
