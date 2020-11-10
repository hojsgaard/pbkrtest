## # Notes about using this driver:
## To use this driver, the file Sweavealter.R must be sourced in R.
## Simply use Sweave as usual (this driver replaces the default one).
## This driver cannot currently be used in batch mode (R CMD
## ...).
## Note that png pixmaps can only be handled by pdf-oriented latex
## drivers, such as pdflatex.
## Also note that when picture files exists in different versions (eps,
## pdf, png), some drivers may use png version by default. 


## # Capabilities:
## The following additional argument may be specified in a chunck head:
## - png: logical, used to produce png pictures (TRUE, default) or not (FALSE)
## - center: logical, to specify whether the figure should be centred in the pdf (TRUE) or not (FALSE)
## - size: float from 0 to 1, giving the size of the figure as a proportion of the text width
## - caption: a character string quoted by " ", which is added as a caption to the figure.

## # Author: Thibaut Jombart <t.jombart@imperial.ac.uk>. Original
##   code by Friedrich Leisch.

## # Licence: GPL version 2



##                                         # Voici une version alternative de Sweave, qui permet d'intégrer les dernières modifications du driver RweaveInPng
## # Thibaut Jombart, novembre 2006

Sweave2 <- function (file, driver = RweaveInPng, syntax = getOption("SweaveSyntax"), 
    ...) 
{
    if (is.character(driver)) 
        driver <- get(driver, mode = "function")()
    else if (is.function(driver)) 
        driver <- driver()
    if (is.null(syntax)) 
        syntax <- utils:::SweaveGetSyntax(file)
    if (is.character(syntax)) 
        syntax <- get(syntax, mode = "list")
    drobj <- driver$setup(file = file, syntax = syntax, ...)
    on.exit(driver$finish(drobj, error = TRUE))
    text <- utils:::SweaveReadFile(file, syntax)
    syntax <- attr(text, "syntax")
    mode <- "doc"
    chunknr <- 0
    chunk <- NULL
    namedchunks <- list()
    for (line in text) {
        if (any(grep(syntax$doc, line))) {
            if (mode == "doc") {
                if (!is.null(chunk)) 
                  drobj <- driver$writedoc(drobj, chunk)
                mode <- "doc"
            }
            else {
                if (!is.null(chunkopts$label)) 
                  namedchunks[[chunkopts$label]] <- chunk
                if (!is.null(chunk)) 
                  drobj <- driver$runcode(drobj, chunk, chunkopts)
                mode <- "doc"
            }
            chunk <- NULL
        }
        else if (any(grep(syntax$code, line))) {
            if (mode == "doc") {
                if (!is.null(chunk)) 
                  drobj <- driver$writedoc(drobj, chunk)
                mode <- "code"
            }
            else {
                if (!is.null(chunkopts$label)) 
                  namedchunks[[chunkopts$label]] <- chunk
                if (!is.null(chunk)) 
                  drobj <- driver$runcode(drobj, chunk, chunkopts)
                mode <- "code"
            }
            chunkopts <- sub(syntax$code, "\\1", line)
            chunkopts <- .SweaveParseOptions(chunkopts, drobj$options, 
                driver$checkopts)
            chunk <- NULL
            chunknr <- chunknr + 1
            chunkopts$chunknr <- chunknr
        }
        else {
            if (mode == "code" && any(grep(syntax$coderef, line))) {
                chunkref <- sub(syntax$coderef, "\\1", line)
                if (!(chunkref %in% names(namedchunks))) 
                  warning(gettextf("reference to unknown chunk '%s'", 
                    chunkref), domain = NA)
                line <- namedchunks[[chunkref]]
            }
            if (is.null(chunk)) 
                chunk <- line
            else chunk <- c(chunk, line)
        }
    }
    if (!is.null(chunk)) {
        if (mode == "doc") 
            driver$writedoc(drobj, chunk)
        else drobj <- driver$runcode(drobj, chunk, chunkopts)
    }
    on.exit()
    driver$finish(drobj)
}



.SweaveParseOptions <- function (text, defaults = list(), check = NULL) 
{
    caption=NULL
    x <- sub("^[[:space:]]*(.*)", "\\1", text)
    x <- sub("(.*[^[:space:]])[[:space:]]*$", "\\1", x)
    
    # try and find a caption argument and its value 
    if(length(grep("CAPTION[[:space:]]*=[[:space:]]*\".+\"",toupper(x)))  > 0){
      temp <- regexpr("caption[[:space:]]*=[[:space:]]*\".+\"",x)
      caption <- substr(x,temp,temp+attr(temp,"match.length")-1) # isolate caption to treat separately
      caption <- gsub("\"" , "" , caption)
      caption <- sub("caption[[:space:]]*=[[:space:]]*", "", caption)
      caption <- sub("CAPTION[[:space:]]*=[[:space:]]*", "", caption)
      x <- sub("caption[[:space:]]*=[[:space:]]*\".+\"","",x)
      x <- sub("(^)[[:space:]]*," ,"" ,x)
      x <- sub(",[[:space:]]*($)" ,"" ,x)
      x <- sub(",[[:space:]]*,", ",", x)
    }
   
    x <- unlist(strsplit(x, "[[:space:]]*,[[:space:]]*"))
    x <- strsplit(x, "[[:space:]]*=[[:space:]]*")
    
    if (length(x) > 0) {
        if (length(x[[1]]) == 1) {
            x[[1]] <- c("label", x[[1]])
        }
        # add caption here
        if(!is.null(caption)) { x[[length(x)+1]] <- c("caption",caption) }
    }
    
    else return(defaults)
    if (any(sapply(x, length) != 2))
        stop(gettextf("parse error or empty option in\n%s", text), 
            domain = NA)
    options <- defaults
    for (k in 1:length(x)) options[[x[[k]][1]]] <- x[[k]][2]
    if (!is.null(options[["label"]]) && !is.null(options[["engine"]])) 
        options[["label"]] <- sub(paste(".", options[["engine"]], 
            "$", sep = ""), "", options[["label"]])
    if (!is.null(check)) 
        options <- check(options)
    options
}

# Sweave driver able to create 'png' compressed pictures, along with the usual eps and pdf pictures.  
# 
# Thibaut JOMBART (jombart@biomserv.univ-lyon1.fr), from Friedrich Leisch's RweaveLatex driver.


## Driver creation
RweaveInPng <- function () 
{
    list(setup = .RweaveInPngSetup, runcode = .RweaveInPngRuncode, writedoc = .RweaveInPngWritedoc, finish = .RweaveInPngFinish, checkopts = .RweaveInPngOptions)
}
#



## Function 'setup'

.RweaveInPngSetup <- function (file, syntax, output = NULL, quiet = FALSE, debug = FALSE, echo = TRUE, eval = TRUE, split = FALSE, stylepath = TRUE,  pdf = TRUE, eps = TRUE, png = TRUE,center=TRUE,size=0.5,caption=NULL) 
{
    if (is.null(output)) {
        prefix.string <- basename(sub(syntax$extension, "", file))
        output <- paste(prefix.string, "tex", sep = ".")
    }
    else {
        prefix.string <- basename(sub("\\.tex$", "", output))
    }
    if (!quiet) 
        cat("Writing to file ", output, "\n", "Processing code chunks ...\n", 
            sep = "")
    output <- file(output, open = "w+")
    if (stylepath) {
        styfile <- file.path(R.home("share"), "texmf", "Sweave")
        if (.Platform$OS.type == "windows") 
            styfile <- gsub("\\\\", "/", styfile)
        if (any(grep(" ", styfile))) 
            warning(gettextf("path to '%s' contains spaces,\n", styfile), gettext("this may cause problems when running LaTeX"), domain = NA)
    }
    else styfile <- "Sweave"
    options <- list(prefix = TRUE, prefix.string = prefix.string, engine = "R", print = FALSE, eval = eval, fig = FALSE, pdf = pdf, eps = eps, png=png, width = 6, height = 6, term = TRUE,  echo = echo, results = "verbatim", split = split, strip.white = "true", include = TRUE,center=center,size=size,caption=caption)
    options <- .RweaveInPngOptions(options)
    list(output = output, styfile = styfile, havesty = FALSE, debug = debug, quiet = quiet, syntax = syntax, options = options, chunkout = list())
}
# 



## Function 'runcode'
.RweaveInPngRuncode <- function (object, chunk, options) 
{
    if (!(options$engine %in% c("R", "S"))) {
        return(object)
    }
    if (!object$quiet) {
        cat(formatC(options$chunknr, width = 2), ":")
        if (options$echo) 
            cat(" echo")
        if (options$eval) {
            if (options$print) 
                cat(" print")
            if (options$term) 
                cat(" term")
            cat("", options$results)
            if (options$fig) {
                if (options$eps) 
                  cat(" eps")
                if (options$pdf) 
                  cat(" pdf")
                if(options$png) 
                  cat(" png")
            }
        }
        if (!is.null(options$label)) 
            cat(" (label=", options$label, ")", sep = "")
        cat("\n")
    }
    chunkprefix <- utils:::RweaveChunkPrefix(options)
    if (options$split) {
        chunkout <- object$chunkout[[chunkprefix]]
        if (is.null(chunkout)) {
            chunkout <- file(paste(chunkprefix, "tex", sep = "."), "w")
            if (!is.null(options$label)) 
                object$chunkout[[chunkprefix]] <- chunkout
        }
    }
    else chunkout <- object$output
    utils:::SweaveHooks(options, run = TRUE)
    chunkexps <- try(parse(text = chunk), silent = TRUE)
    utils:::RweaveTryStop(chunkexps, options)
    openSinput <- FALSE
    openSchunk <- FALSE
    if (length(chunkexps) == 0) 
        return(object)
    for (nce in 1:length(chunkexps)) {
        ce <- chunkexps[[nce]]
        dce <- deparse(ce, width.cutoff = 0.75 * getOption("width"))
        if (object$debug) 
            cat("\nRnw> ", paste(dce, collapse = "\n+  "), "\n")
        if (options$echo) {
            if (!openSinput) {
                if (!openSchunk) {
                  cat("\\begin{Schunk}\n", file = chunkout, append = TRUE)
                  openSchunk <- TRUE
                }
                cat("\\begin{Sinput}", file = chunkout, append = TRUE)
                openSinput <- TRUE
            }
            cat("\n", getOption("prompt"), paste(dce, collapse = paste("\n", 
                getOption("continue"), sep = "")), file = chunkout, 
                append = TRUE, sep = "")
        }
        tmpcon <- file()
        sink(file = tmpcon)
        err <- NULL
        if (options$eval) 
            err <- utils:::RweaveEvalWithOpt(ce, options)
        cat("\n")
        sink()
        output <- readLines(tmpcon)
        close(tmpcon)
        if (length(output) == 1 & output[1] == "") 
            output <- NULL
        utils:::RweaveTryStop(err, options)
        if (object$debug) 
            cat(paste(output, collapse = "\n"))
        if (length(output) > 0 & (options$results != "hide")) {
            if (openSinput) {
                cat("\n\\end{Sinput}\n", file = chunkout, append = TRUE)
                openSinput <- FALSE
            }
            if (options$results == "verbatim") {
                if (!openSchunk) {
                  cat("\\begin{Schunk}\n", file = chunkout, append = TRUE)
                  openSchunk <- TRUE
                }
                cat("\\begin{Soutput}\n", file = chunkout, append = TRUE)
            }
            output <- paste(output, collapse = "\n")
            if (options$strip.white %in% c("all", "true")) {
                output <- sub("^[[:space:]]*\n", "", output)
                output <- sub("\n[[:space:]]*$", "", output)
                if (options$strip.white == "all") 
                  output <- sub("\n[[:space:]]*\n", "\n", output)
            }
            cat(output, file = chunkout, append = TRUE)
            remove(output)
            if (options$results == "verbatim") {
                cat("\n\\end{Soutput}\n", file = chunkout, append = TRUE)
            }
        }
    }
    if (openSinput) {
        cat("\n\\end{Sinput}\n", file = chunkout, append = TRUE)
    }
    if (openSchunk) {
        cat("\\end{Schunk}\n", file = chunkout, append = TRUE)
    }
    if (is.null(options$label) & options$split) 
        close(chunkout)
    if (options$split & options$include) 
        cat("\\input{", chunkprefix, "}\n", sep = "", file = object$output, append = TRUE)
    if (options$fig && options$eval) {
        if (options$eps) {
            postscript(file = paste(chunkprefix, "eps", sep = "."), 
                width = options$width, height = options$height, 
                paper = "special", horizontal = FALSE)
            err <- try({
                utils:::SweaveHooks(options, run = TRUE)
                eval(chunkexps, envir = .GlobalEnv)
            })
            dev.off()
            if (inherits(err, "try-error")) 
                stop(err)
        }
        if (options$pdf) {
            pdf(file = paste(chunkprefix, "pdf", sep = "."), 
                width = options$width, height = options$height)
            err <- try({
                SweaveHooks(options, run = TRUE)
                eval(chunkexps, envir = .GlobalEnv)
            })
            dev.off()
            if (inherits(err, "try-error")) 
                stop(err)
        }
         if (options$png) {
            # modif png(file = paste(chunkprefix, "png", sep = "."), width = options$width, height = options$height)
            png(file = paste(chunkprefix, "png", sep = "."), width = min({if(!is.null(options$width)) options$width * 80 else 480},1200), height = min({if(!is.null(options$height)) options$height * 80 else 480},1200))
            err <- try({
                SweaveHooks(options, run = TRUE)
                eval(chunkexps, envir = .GlobalEnv)
            })
            dev.off()
            if (inherits(err, "try-error")) 
                stop(err)
        }        
        if (options$include) {
	if(options$center) cat("\\begin{center}\n",sep = "", file = object$output, append = TRUE)
            cat("\\resizebox{", options$size,"\\textwidth}{!}{\\includegraphics{", chunkprefix, "}}\n", sep = "", 
                file = object$output, append = TRUE)
	if(!is.null(options$caption)) cat("\\\\ {\\footnotesize ", options$caption,"}\n \\vspace{.3cm}", sep = "", file = object$output, append = TRUE)
	if(options$center) cat("\\end{center}\n",sep = "", file = object$output, append = TRUE)
	}
    }
    return(object)
}
# 



## Function '.RweaveInPngWritedoc'
.RweaveInPngWritedoc <- function (object, chunk) 
{
    if (any(grep("\\usepackage[^\\}]*Sweave.*\\}", chunk))) 
        object$havesty <- TRUE
    if (!object$havesty) {
        chunk <- gsub("\\\\begin\\{document\\}", paste("\\\\usepackage{", 
            object$styfile, "}\n\\\\begin{document}", sep = ""), 
            chunk)
        object$havesty <- TRUE
    }
    while (any(pos <- grep(object$syntax$docexpr, chunk))) {
        cmdloc <- regexpr(object$syntax$docexpr, chunk[pos[1]])
        cmd <- substr(chunk[pos[1]], cmdloc, cmdloc + attr(cmdloc, "match.length") - 1)
        cmd <- sub(object$syntax$docexpr, "\\1", cmd)
        if (object$options$eval) {
            val <- as.character(eval(parse(text = cmd), envir = .GlobalEnv))
            if (length(val) == 0) 
                val <- ""
        }
        else val <- paste("\\\\verb{<<", cmd, ">>{", sep = "")
        chunk[pos[1]] <- sub(object$syntax$docexpr, val, chunk[pos[1]])
    }
    while (any(pos <- grep(object$syntax$docopt, chunk))) {
        opts <- sub(paste(".*", object$syntax$docopt, ".*", sep = ""), "\\1", chunk[pos[1]])
        object$options <- utils:::SweaveParseOptions(opts, object$options, .RweaveInPngOptions)
        chunk[pos[1]] <- sub(object$syntax$docopt, "", chunk[pos[1]])
    }
    cat(chunk, sep = "\n", file = object$output, append = TRUE)
    return(object)
}
#



## Function 'finish'
.RweaveInPngFinish <- function (object, error = FALSE) 
{
    if (!object$quiet && !error ) 
        cat("\n", gettextf("You can now run LaTeX (use a PDF-oriented compiler for png) on '%s'", summary(object$output)$description), 
            "\n", sep = "")
    close(object$output)
    if (length(object$chunkout) > 0) 
        for (con in object$chunkout) close(con)
}
#



## Function '.RweaveInPngOptions'
.RweaveInPngOptions <- function (options) 
{
    c2l <- function(x) {
        if (is.null(x)) 
            return(FALSE)
        else return(as.logical(toupper(as.character(x))))
    }
    NUMOPTS <- c("width", "height","size")
    NOLOGOPTS <- c(NUMOPTS, "results", "prefix.string", "engine", "label", "strip.white","caption")
    for (opt in names(options)) {
        if (!(opt %in% NOLOGOPTS)) {
            oldval <- options[[opt]]
            if (!is.logical(options[[opt]])) {
                options[[opt]] <- c2l(options[[opt]])
            }
            if (is.na(options[[opt]])) 
                stop(gettextf("invalid value for '%s' : %s", opt, oldval), domain = NA)
        }
        else if (opt %in% NUMOPTS) {
            options[[opt]] <- as.numeric(options[[opt]])
        }
    }
    options$results <- tolower(as.character(options$results))
    options$results <- match.arg(options$results, c("verbatim", "tex", "hide"))
    options$strip.white <- tolower(as.character(options$strip.white))
    options$strip.white <- match.arg(options$strip.white, c("true", "false", "all"))
    options
}
#


