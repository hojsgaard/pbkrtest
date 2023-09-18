RweaveHTML2 <-function () 
{
    list(setup = RweaveHTMLSetup2,
         runcode = RweaveHTMLRuncode2, 
         writedoc = RweaveHTMLWritedoc,
         finish = RweaveHTMLFinish, 
         checkopts = RweaveHTMLOptions
         )
}

"HTMLdefault2"<-
function(x, file=get(".HTML.file"),append=TRUE,...)
{
  ##   cat("HTML.default2\n")
  ##   print(x)
  x2 <- capture.output(x)
  x2 <- gsub(" ","&nbsp;",x2)
  x2 <- paste("<tt>", x2, "</tt>")
  ##print(x2)
  x3 <- paste(x2,collapse="\n<br>\n")
  HTML(x3,file=file,append=append,...)
  
  #x3 <<- paste(capture.output(x),collapse="\n<br>\n")
  #HTML(paste(capture.output(x),collapse="\n<br>\n"),file=file,append=append,...)
  ##cat("...done\n")
  invisible(x)
  
}

"HTMLCommand2" <- function(x,file=get(".HTML.file"),Num="",menu=FALSE,target="index<-main.html",append=TRUE,...)
  {
##     cat("HTMLCommand\n")
##     print(x)
    cat("\n",file=file,append=append,...)
    if (menu==TRUE)
      cat(paste("<br><li><a class=command href='./",target,"#Num",Num,"' target=main> ",paste(x,collapse=""),"</a>",sep=""),file=file,append=TRUE,sep="")
    else {
      if (Num!="") cat(paste("<a name=Num",Num,">&nbsp;</a>",sep=""),file=file,append=TRUE,sep="")
      #cat(paste("\n<p><xmp class=command>> ",x,"</xmp></p>\n",sep=""),file=file,append=TRUE,sep="")
      cat(paste("\n<p><PRE>&gt ",x,"</pre></p>\n",sep=""),file=file,append=TRUE,sep="") ## sorenh
    }
##     cat("...done\n")
  }


RweaveHTMLSetup2 <- 
  function (file, syntax, output = NULL, quiet = FALSE, debug = FALSE, 
            echo = TRUE, eval = TRUE, split = FALSE, cssfile = "R2HTML.css", 
            havecss = FALSE, width = 500, height = 500, border = 1, png = TRUE) 
{
  if (is.null(output)) {
    prefix.string <- basename(sub(syntax$extension, "", file))
    output <- paste(prefix.string, "html", sep = ".")
  }
  else {
    prefix.string <- basename(sub("\\.html$", "", output))
  }
  if (!quiet) 
    cat("Writing to file ", output, "\n", "Processing code chunks ...\n", 
        sep = "")
  output <- file(output, open = "w+")
  options <- list(prefix = TRUE, prefix.string = prefix.string, 
                  engine = "R", print = FALSE, eval = eval, fig = FALSE, 
                  png = png, width = width, height = height, term = TRUE, 
                  echo = echo, results = "Robj", split = split, strip.white = TRUE, 
                  include = TRUE, align = "center", caption = NULL, bg = "white", 
                  pointsize = 12)
  list(output = output, debug = debug, quiet = quiet, syntax = syntax, 
       options = options, chunkout = list(), cssfile = cssfile, 
       havecss = havecss)
}


"RweaveHTMLRuncode2" <- function(object, chunk, options)
{
    if(!(options$engine %in% c("R", "S"))) return(object)
    if(!object$quiet){
        cat(formatC(options$chunknr, width=2), ":")
        if(options$echo) cat(" echo")
        if(options$eval){
            if(options$print) cat(" print")
            if(options$term) cat(" term")
            cat("", options$results)
            if(options$fig){
                if(options$png) cat(" png")
            }
        }
        if(!is.null(options$label))
            cat(" (label=", options$label, ")", sep="")
        cat("\n")
    }

    
    #chunkprefix <- utils:::RweaveChunkPrefix(options)
    chunkprefix <- RweaveChunkPrefix(options)

    if(options$split){
        chunkout <- object$chunkout[[chunkprefix]]
        if(is.null(chunkout)){
            chunkout <- file(paste(chunkprefix, "html", sep="."), "w")
            if(!is.null(options$label))
                object$chunkout[[chunkprefix]] <- chunkout
        }
    }
    else
        chunkout <- object$output

    assign(".HTML.file",chunkout,pos=.GlobalEnv, immediate=TRUE)
    #utils:::SweaveHooks(options, run=TRUE)
    SweaveHooks(options, run=TRUE)
    
    chunkexps <- try(parse(text=chunk), silent=TRUE)
    #utils:::RweaveTryStop(chunkexps, options)
    RweaveTryStop(chunkexps, options)
    openSinput <- FALSE
    openSchunk <- FALSE
    
    if(length(chunkexps)==0)
        return(object)

    for(nce in 1:length(chunkexps))
    {
        ce <- chunkexps[[nce]]
        #dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
        if(object$debug)
            cat("\nRnw> ", paste(ce, collapse="\n+  "),"\n")
        if(options$echo){
            if(!openSinput){
                if(!openSchunk){
                    cat("<!-- begin{Schunk} !-->\n",
                        file=chunkout, append=TRUE)
                    openSchunk <- TRUE
                }
                cat("<!-- begin{Sinput} !-->",
                    file=chunkout, append=TRUE)
                openSinput <- TRUE
            }
            #cat("\n", paste(R2HTML:::HTMLCommand(deparse(ce)),                 ### sorenh
            cat("\n", paste(HTMLCommand2(deparse(ce)),                 ### sorenh
                      collapse=paste("\n", getOption("continue"), sep="")),
                file=chunkout, append=TRUE, sep="")
        }

        # tmpcon <- textConnection("output", "w")
        # avoid the limitations (and overhead) of output text connections
         tmpcon <- file()
         sink(file=tmpcon)
        err <- NULL
        #if(options$eval) err <- utils:::RweaveEvalWithOpt(ce, options)
        if(options$eval) err <- RweaveEvalWithOpt(ce, options)
         cat("\n") # make sure final line is complete
         sink()
         output <- readLines(tmpcon)
         close(tmpcon)
        # delete empty output
        if(length(output)==1 & output[1]=="") output <- NULL

        #utils:::RweaveTryStop(err, options) #### !!!  err$value peut etre exporte via HTML(err.value)
        RweaveTryStop(err, options) #### !!!  err$value peut etre exporte via HTML(err.value)
        
        if(object$debug)
            cat(paste(output, collapse="\n"))

        if(length(output)>0 & (options$results!="hide")){
            if(!openSchunk){
                cat("<!--begin{Schunk}!--> \n",
                    file=chunkout, append=TRUE)
                openSchunk <- TRUE
            }
            if(openSinput){
                cat("\n<!--end{Sinput}!-->\n", file=chunkout, append=TRUE)
                openSinput <- FALSE
            }
            if (options$results=="Robj") HTMLdefault2(err$value, file=chunkout, append=TRUE)
            if (options$results=="html") cat(err$value, file=chunkout, append=TRUE)
            remove(output)

        }
    }
    if(openSinput){
        cat("\n<!--end{Sinput}!-->\n", file=chunkout, append=TRUE)
    }
    if(openSchunk){
        cat("\n<!--end{Schunk}!-->\n", file=chunkout, append=TRUE)
    }

    if(is.null(options$label) & options$split)
        close(chunkout)

    if(options$fig && options$eval){
        if(options$png){
            png(filename=paste(chunkprefix, "png", sep="."),width=options$width,height=options$height,bg=options$bg,pointsize=options$pointsize)

            #err <- try({utils:::SweaveHooks(options, run=TRUE);
            err <- try({SweaveHooks(options, run=TRUE);
                        eval(chunkexps, envir=.GlobalEnv)})
            dev.off()
            if(inherits(err, "try-error")) stop(err)
        }
        if(options$include){
          
          
          cat("<p align='",options$align,"'><img height=",options$height, " width=",options$width," src='", chunkprefix, ".png'",if (!is.null(options$border)) paste("border=",options$border,sep=""),">",if(!is.null(options$caption)) paste("<br><font class='caption='>",options$caption,"</font>",sep=""),"</p>", sep="",
              file=object$output, append=TRUE)
        }
      }


    return(object)
}
