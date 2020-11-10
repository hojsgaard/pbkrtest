SorenRuncode <-  function(object, chunk, options)
{
    chunkout <- object$output
    
    if(object$annotate){
        cat("### chunk number ", options$chunknr,
            ": ", options$label,
            ifelse(options$eval, "", " eval=FALSE"), "\n",
            file=chunkout, append=TRUE, sep="")
    }

    hooks <- SweaveHooks(options, run=FALSE)

    if(options$eval){
        for(k in hooks)
            cat("getOption(\"SweaveHooks\")[[\"", k, "\"]]()\n",
                file=chunkout, append=TRUE, sep="")
    }
    else
        chunk <- paste("##", chunk)
    
    cat(chunk,"\n", file=chunkout, append=TRUE, sep="\n")

    if(is.null(options$label) & options$split)
        close(chunkout)

    return(object)
}

SorenTangleDriver <-  function()
{
    list(setup = RtangleSetup,
         runcode = SorenRuncode,
         writedoc = RtangleWritedoc,
         finish = utils:::RtangleFinish,
         checkopts = RweaveLatexOptions)
}

SorenTangle <- function(file, driver=SorenTangleDriver(),
                        syntax=getOption("SweaveSyntax"), ...)
{
    Sweave(file=file, driver=driver, ...)
}
