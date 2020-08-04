
RweaveHTMLreport <- function(){
  list(setup    = RweaveHTMLreportSetup,
       writedoc = RweaveHTMLreportWritedoc,
       finish   = RweaveHTMLreportFinish)
}

Rmarkup <- function(srcfile, driver=RweaveHTMLreport(), destdir=".", postfix=NULL, cssfile=NULL,
                    cleanup=TRUE, parms=list(height=400,width=400), encoding="", details=0, ...){
  
  if (details>0)
    cat("Preprocessing... \n")
  
  if(!file.exists(srcfile)){
    cat(sprintf("Error: file %s does not exist\n", srcfile))
    cat(sprintf("Your working directory is currently: %s\n", getwd()))
    stop(sprintf("Exiting...\n"),call.=FALSE)
  }
  
  if (.Platform$OS.type == "windows"){
    srcfile <- gsub("\\\\", "/", srcfile)
    destdir <- gsub("\\\\", "/", destdir)
  }
  
  if (!file.exists(destdir)){
    cat(sprintf("Error: destdir (destination directory) %s does not exist\n", destdir))
    cat(sprintf("Your working directory is currently: %s\n", getwd()))
    stop("Exiting...", call.=FALSE)
  }
  
  ## File has format: path/filename.extension
  ##
  if (!is.null(postfix)){
    postfix <- paste("-", postfix, sep="-")
  }
  path.filename  <- gsub("(.*)\\..*" ,"\\1", srcfile)            ## path/filename
  filename       <- gsub(".*/(.*)$","\\1",   path.filename)      ## filename
  ddir.fname.pfix         <- paste(destdir, "/", filename, postfix, sep="")
  ddir.fname.pfix.html    <- paste(ddir.fname.pfix,".html", sep="")
  fname.pfix              <- paste(filename, postfix, sep="")
  
  ## We need two temporary files
  ##
  tfile <- paste("_",paste(filename,tempfile(pattern="",tmpdir=""),sep="-"),sep="")
  if (.Platform$OS.type == "windows")
    tfile <- gsub("\\\\", "/", tfile)
  tfile       <- gsub("/","", tfile)
  tfile.html  <- paste(tfile,".html",sep="")

  if(details>0){
    cat(sprintf(" srcfile               : %s\n",  srcfile))
    cat(sprintf(" filename              : %s\n",  filename))
    cat(sprintf(" fname.pfix            : %s\n",  fname.pfix))
    cat(sprintf(" ddir.fname.pfix       : %s\n",  ddir.fname.pfix))
    cat(sprintf(" ddir.fname.pfix.html  : %s\n",  ddir.fname.pfix.html))
    cat(sprintf(" tfile                 : %s\n",  tfile))
    cat(sprintf(" tfile.html            : %s\n",  tfile.html))
  }
  
  ff <- file.copy(srcfile, tfile, overwrite=TRUE)   ## Create copy of the source file
  if (!ff){
    stop("Can not create temporary file in working directory. Is the directory write protected?")
  }
  
  inlines   <- readLines(tfile)
  outlines  <- driver$setup(inlines, details)  ##RweaveHTMLreportSetup,
  write(outlines, file=tfile)
  #cat("Preprocessing done...\n")
    
  #Sweave(tfile, driver=RweaveHTML(), encoding=encoding)
  Sweave(tfile, driver=RweaveHTML2(), encoding=encoding)

  #cat("Postprocessing... \n")
  inlines    <- readLines(tfile.html)
  outlines   <- driver$writedoc(inlines, fname.pfix, tfile, parms) ##RweaveHTMLreportWritedoc
  
  if (!is.null(cssfile)){
    cssline <- paste("<link rel=stylesheet href=\"",cssfile,"\" type=text/css>",sep="")
    outlines <- c(cssline, outlines)
  }





  
  write(outlines, file=ddir.fname.pfix.html)
  driver$finish(tfile, fname.pfix, ddir.fname.pfix) ##RweaveHTMLreportFinish
  #cat("Postprocessing done... \n")
  
  if (cleanup){

    if(details>1)
      cat(sprintf("Cleaning up: removing temporary files\n   %s\n",
                  toString(c(tfile,tfile.html))))
    file.remove(tfile)
    file.remove(tfile.html)
  }
  
  return(invisible(NULL))
}


RweaveHTMLreportSetup <- function(srclines, details=0){
  
  key <- c(
           "^$",				## empty line
           "^#",    				## text line
           "^ *##? *@@|^ *##? *<<",	        ## begin code chunk
           "^ *##? *@ *$" 			## end code chunk
           )
  
  state <- c("RCODE", "emptyLine", "textLine", "beginCode", "endCode")
  
  get.token <- function(lll, key){
    ans <- which(c(lapply(key, grepl, lll),recursive=T))
    if (length(ans)==0)
      ans <- 0
    ans
  }
  
  anslines <- srclines
  inCode   <- 0
  for (ii in 1:length(srclines)){
    lll <- srclines[ii]
    tok <- max(get.token(lll, key))    
    if (tok==3){
      inCode <- 1
    } else if (tok==4) {
      inCode <- 0
    }

    if (details>0)
      cat(sprintf("   -> tok: %2s; state: %10s;  inCode: %i;  line: %s\n",
                  toString(tok), state[tok+1], inCode, lll))
    
    if (inCode==0){
      lll <- .handleTextLine(lll,tok)
    } else {
      if (inCode==1){
        lll <- .handleRCodeLine(lll,tok)
      }
    }    
    anslines[ii] <- lll
  }
  anslines
}

RweaveHTMLreportWritedoc <- function(srclines, fname.pfix, tfile, parms){

  
  get.token <- function(lll, key){
    ans <- which(c(lapply(key, grepl, lll),recursive=T))
    if (length(ans)==0)
      ans <- 0
    ans
  }

  anslines <- srclines
  for (ii in 1:length(srclines)){
    lll <- srclines[ii]

##     sss <- sprintf("<img height=%i width=%i ", parms$height, parms$width)
##     lll <- gsub("<img height= width= ",          sss, lll)

    ##lll <- gsub("<img height= width= ",          "<img ", lll)

    ## <xmp> is deprecated; use <PRE> instead
    ## lll <- gsub("<p><xmp class=command>>(.+)</xmp></p>","<PRE>&gt\\1 </PRE>", lll)

    lll <- gsub("<p align= center >",            "<p align= left >", lll)
    lll <- gsub("<p align='center'>",            "<p align= left >", lll)
    lll <- gsub("(<!--\\\\end\\{Schunk\\}!-->)", "</p> \\1",lll)
    
    lll <- gsub(tfile, fname.pfix, lll)
    anslines[ii] <- lll
  }
  ##anslines


  ## Last bit is a hack

##   for (ii in 1:length(anslines)){
##     ss <- anslines[ii]
##     if (length(grep("<!--[^<!--]*!-->",ss))>0){
##       ss <- gsub("[ |\\]*","",ss)
##       anslines[ii] <- ss
##     }
##   }
  
  key <- c(
           "<!--begin\\{Schunk\\}",
           "<!--begin\\{Sinput\\}",  
           "<!--end\\{Sinput\\}" ,  
           "</p><!--end\\{Schunk\\}"
           )
  
  inCode   <- 0
  inChunk  <- 0
  inresvec <- inchvec <- tokvec   <- rep.int(0, length(anslines))
  
  for (ii in 1:length(anslines)){
    lll <- anslines[ii]
    tokvec[ii] <- tok <- max(get.token(lll, key))    
    if (tok==1){
      inChunk = 1
    } else if (tok==4) {
      inChunk <- 0
    }
    
    if (tok==3){
      inCode = 1
    } else if (tok==4) {
      inCode <- 0
    }
    inresvec[ii] <- inCode
    inchvec[ii] <- inChunk
  }

  ##cbind(tokvec, inchvec, inresvec, anslines)

##   for (ii in 1:length(anslines)){
##     if (inchvec[ii]==1){
##       lll <- anslines[ii]
##       ##lll <- gsub("<[^>]*>","",lll)
##       ##lll <- gsub("[[:blank:]]*,",",",lll)
##       ##lll <- sprintf("<PRE>%s</PRE>", lll)
##       anslines[ii] <- lll
##     }
##   }

  anslines
}

RweaveHTMLreportFinish <- function(tfile, filename, destdir.filename){

##   cat("... RweaveHTMLreportFinish\n")
##   cat(sprintf("... tfile=%s \n... filename=%s \n... destdir.filename=%s\n",
##               tfile, filename, destdir.filename))

  ## Rename graphics files
  
  figfiles    <- glob2rx(paste(tfile, "-*.png", sep=""))
  currfiglist <- list.files(pattern=figfiles)
  #cat(sprintf(" currfiglist    : %s\n", toString(currfiglist)))
  if (length(currfiglist)>0){
    newfiglist  <- gsub(tfile, destdir.filename, currfiglist)
    #cat(sprintf(" newfiglist     : %s\n", toString(newfiglist)))
    for (ii in 1:length(currfiglist)){
      file.copy(currfiglist[ii], newfiglist[ii], overwrite=TRUE)
      file.remove(currfiglist[ii])
    }
  }
  ## FIXME: There might be a problem if we assign names to code chunks
}

.handleTextLine <- function(lll,tok){
    ## blank lines -> <br>
    input <- lll
    lll <- gsub("^*$", "<br>", lll)
    lll <- gsub("^##?$", "<br><br>", lll)

    ## Headers
    lll <- gsub("##+[[:blank:]]*======[[:blank:]]*(.*)[[:blank:]]*======[[:blank:]]*", "<h6>\\1</h6>", lll)
    lll <- gsub("##+[[:blank:]]*=====[[:blank:]]*(.*)[[:blank:]]*=====[[:blank:]]*",   "<h5>\\1</h5>", lll)
    lll <- gsub("##+[[:blank:]]*====[[:blank:]]*(.*)[[:blank:]]*====[[:blank:]]*",     "<h4>\\1</h4>", lll)
    lll <- gsub("##+[[:blank:]]*===[[:blank:]]*(.*)[[:blank:]]*===[[:blank:]]*",       "<h3>\\1</h3>", lll)
    lll <- gsub("##+[[:blank:]]*==[[:blank:]]*(.*)[[:blank:]]*==[[:blank:]]*",         "<h2>\\1</h2>", lll)
    lll <- gsub("##+[[:blank:]]*=[[:blank:]]*(.*)[[:blank:]]*=[[:blank:]]*",           "<h1>\\1</h1>", lll)
    ## Italics
    lll <- gsub("//([^/]*)//",
                '<span style="font-style: italic;">\\1</span>', lll)
    ## Bold
    lll <- gsub("\\*\\*([^\\*]*)\\*\\*",
                '<span style="font-weight: bold;">\\1</span>', lll)
    ## Underline
    lll <- gsub("__([^_]*)__",
                '<span style="text-decoration: underline;">\\1</span>', lll)
    ## True type (Courier New)
    lll <- gsub("&&([^&]*)&&",
                '<span style="font-family: Courier New;">\\1</span>', lll)
    ## Date
    lll <- gsub("##+[[:blank:]]*%%date",
                '\\<\\<echo=FALSE\\>\\>=\nas.character(Sys.time())\n@', lll)
    ## 3 hashes -> a comment
    lll <- gsub("^[[:blank:]]*###+(.*)",
                "<!-- \\1 !-->", lll)
    ## 1 or 2 hashes -> text
    lll <- gsub("^[[:blank:]]*##?[[:blank:]]*(.*)",
                "\\1", lll)
    ans <- lll
    ##cat(sprintf(".handleTextLine:\n input: %s\n ans:    %s\n",input, ans))
    ans
}









.handleRCodeLine <- function(lll,tok){
##   input <- lll
  lll <- gsub("^[[:blank:]]*##? *<<(.*)", "\\<\\<\\1", lll)
    
### Vanilla code chunk (<<>>=)
  lll <- gsub("^[[:blank:]]*##? *@@ *$", "<<>>=", lll)
      
### Vanilla code chunk with figure (<<fig=T>>=)
  lll <- gsub("^[[:blank:]]*##? *@@@ *$", "<<fig=T>>=", lll)
          
## cat(sprintf("...handleRCodeLine:\n.....input: %s\n.....  ans: %s\n",input, lll))
  lll
}



HTMLreport <- function(srcfile, driver=RweaveHTMLreport(), destdir=".", postfix="REPORT", cssfile=NULL,
                       cleanup=TRUE, ...){
  cat("HTMLreport is deprecated; please use Rmarkup instead.\n")
}

Rscript2HTML <- function(srcfile, driver=RweaveHTMLreport(), destdir=".", postfix="REPORT", cssfile=NULL,
                         cleanup=TRUE, ...){
  cat("Rscript2HTML is deprecated; please use Rmarkup instead.\n")
}





## ## blank lines -> <br>
## lll <- gsub("^*$", "<br>", lll)
## lll <- gsub("^##?$", "<br>", lll)

## ## Headers
## lll <- gsub("##+[[:blank:]]*======[[:blank:]]*(.*)[[:blank:]]*======[[:blank:]]*", "<h6>\\1</h6>", lll)
## lll <- gsub("##+[[:blank:]]*=====[[:blank:]]*(.*)[[:blank:]]*=====[[:blank:]]*",   "<h5>\\1</h5>", lll)
## lll <- gsub("##+[[:blank:]]*====[[:blank:]]*(.*)[[:blank:]]*====[[:blank:]]*",     "<h4>\\1</h4>", lll)
## lll <- gsub("##+[[:blank:]]*===[[:blank:]]*(.*)[[:blank:]]*===[[:blank:]]*",       "<h3>\\1</h3>", lll)
## lll <- gsub("##+[[:blank:]]*==[[:blank:]]*(.*)[[:blank:]]*==[[:blank:]]*",         "<h2>\\1</h2>", lll)
## lll <- gsub("##+[[:blank:]]*=[[:blank:]]*(.*)[[:blank:]]*=[[:blank:]]*",           "<h1>\\1</h1>", lll)
## ## Italics
## lll <- gsub("//([^/]*)//",
##             '<span style="font-style: italic;">\\1</span>', lll)
## ## Bold
## lll <- gsub("\\*\\*([^\\*]*)\\*\\*",
##             '<span style="font-weight: bold;">\\1</span>', lll)
## ## Underline
## lll <- gsub("__([^_]*)__",
##             '<span style="text-decoration: underline;">\\1</span>', lll)
## ## True type (Courier New)
## lll <- gsub("&&([^&]*)&&",
##             '<span style="font-family: Courier New;">\\1</span>', lll)

## ## Vanilla code chunk (<<>>=)
## lll <- gsub("^[[:blank:]]*##?@@$", "<<>>=", lll)

## ## Vanilla code chunk with figure (<<fig=T>>=)
## lll <- gsub("^[[:blank:]]*##?@@fig$", "<<fig=T>>=", lll)

## ## Date
## lll <- gsub("##+[[:blank:]]*%%date",
##             '\\<\\<echo=FALSE\\>\\>=\nSys.time()\n@', lll)
## ## 3 hashes -> a comment
## lll <- gsub("^[[:blank:]]*###+(.*)",
##             "<!-- \\1 !-->", lll)
## ## 1 or 2 hashes -> text
## lll <- gsub("^[[:blank:]]*##?[[:blank:]]*(.*)",
##             "\\1", lll)


