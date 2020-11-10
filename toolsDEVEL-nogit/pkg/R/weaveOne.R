weaveOne <- function(infile=NULL, tex='pdf'){

  if (is.null(infile)){
    fl <- list.files(pattern="\\.Rnw",full.names=T)
    fl <- fl[(1:length(fl))[-grep("~",fl)]]
    if (length(fl)>1){
      stop("More than one .Rnw file")
    } else {
      infile <- unlist(strsplit(fl,"\\.Rnw"))
    }
  }
  #SorenTangle(paste(infile,".Rnw",sep=''))
  Stangle(paste(infile,".Rnw",sep=''), encoding="latin1")
  Sweave(paste(infile,".Rnw",sep=''), encoding="latin1")

  ##igattr(paste(infile,".tex",sep=''), overwrite=TRUE)

  if (tex != FALSE) {
    if (tex == "pdf")
      f2 <- paste("pdflatex", paste(infile,".tex",sep=''))
    else
      f2 <- paste("latex",    paste(infile,".tex",sep=''))
    system(f2, invisible = FALSE, show.output.on.console=TRUE,wait=TRUE)
    cat("LaTeX'ing done...\n")
  }
}


weaveOnei <- function(infile=NULL, tex='pdf'){

  if (is.null(infile)){
    fl <- list.files(pattern="\\.Rnw",full.names=T)
    fl <- fl[(1:length(fl))[-grep("~",fl)]]
    if (length(fl)>1){
      stop("More than one .Rnw file")
    } else {
      infile <- unlist(strsplit(fl,"\\.Rnw"))
    }
  }
  #SorenTangle(paste(infile,".Rnw",sep=''))
  Stangle(paste(infile,".Rnw",sep=''), encoding="latin1")
  Sweave(paste(infile,".Rnw",sep=''), encoding="latin1")

  igattr(paste(infile,".tex",sep=''), overwrite=TRUE)

  if (tex != FALSE) {
    if (tex == "pdf")
      f2 <- paste("pdflatex", paste(infile,".tex",sep=''))
    else
      f2 <- paste("latex",    paste(infile,".tex",sep=''))
    system(f2, invisible = FALSE, show.output.on.console=TRUE,wait=TRUE)
    cat("LaTeX'ing done...\n")
  }
}

weaveOne2 <- function (infile = NULL, tex = "pdf")
{
  if (is.null(infile)) {
    fl <- list.files(pattern = "\\.Rnw", full.names = T)
    fl <- fl[(1:length(fl))[-grep("~", fl)]]
    if (length(fl) > 1) {
      stop("More than one .Rnw file")
    }
    else {
      infile <- unlist(strsplit(fl, "\\.Rnw"))
    }
  }
  #SorenTangle(paste(infile, ".Rnw", sep = ""))
  Stangle(paste(infile,".Rnw",sep=''), encoding="latin1")
  Sweave(paste(infile,".Rnw",sep=''), encoding="latin1")

  if (tex != FALSE) {
    if (tex == "pdf")
      f2 <- paste("pdflatex", paste(infile, ".tex", sep = ""))
    else f2 <- paste("latex", paste(infile, ".tex", sep = ""))
    system(f2, invisible = FALSE, show.output.on.console = TRUE,wait=TRUE)
    cat("LaTeX'ing done...\n")
    }
}
