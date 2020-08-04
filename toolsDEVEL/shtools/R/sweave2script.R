sweave2script <- function(infile, spin=TRUE){

    .line.types <- c("^\\\\section\\{(.*)\\}",
                     "^\\\\subsection\\{(.*)\\}",
                     "^\\\\subsubsection\\{(.*)\\}",
                     "^\\\\label\\{(.*)\\}",
                     "^\\\\documentclass\\{(.*)\\}",
                     "^\\\\usepackage.*",
                     "^\\\\SweaveOpts\\{(.*)\\}",
                     "^\\\\newslide.*",
                     "^\\\\maketitle.*",          
                     "@",
                     "<<(.*)>>=",
                     "\\\\item (.*)",
                     "\\\\begin\\{displaymath\\}",
                     "\\\\end\\{displaymath\\}",
                     "\\\\begin\\{itemize\\}",                     
                     "\\\\end\\{itemize\\}",
                     "\\\\begin\\{document\\}",
                     "\\\\end\\{document\\}",
                     "SweaveHooks",
                     "\\\\author\\{(.*)\\}",
                     "\\\\title\\{(.*)\\}"
                     )
    
    .tokens <- c("none",
                 "section",
                 "subsection",
                 "subsubsection",
                 "label",
                 "documentclass",
                 "usepackage",
                 "sweaveopts",
                 "newslide",
                 "maketitle",
                 "@",
                 "<<>>",
                 "item",
                 "begindisplaymath",
                 "enddisplaymath",
                 "beginitemize",
                 "enditemize",
                 "begindocument",
                 "enddocument",
                 "sweavehooks",
                 "author",
                 "title"                 
                 )
    
    .scan.line <- function(line){
        jj <- which( sapply(.line.types, grep, line) == 1 )
        if (length(jj)==0)
            jj <- 0
        jj
    }
    
    
    .get.token <- function(line){
        jj <- .scan.line(line)
        .tokens[jj+1]
    }
    
    
    filename <- gsub("\\..*", "", infile)
    
    outfile <- paste0(filename, "-script.R")
    
    srclines <- readLines(infile)
    ## Remove trailing whites
    srclines <- gsub("([^ \t\r\n])[ \t]+$", "\\1", srclines)
    srclines
    outlines <- srclines
    keep <- rep(TRUE, length(srclines))
    instate <- "text"
###
    for (ii in 1:length(srclines)){
        line <- srclines[ii]; line
        code <- .scan.line(line)
        token <- .get.token(line)
        st <- paste0(instate,"-",token)
                                        print(paste0(code, "-", token, " : ", instate, "-> ", line))
                                        print(st)
        switch(st,
               "text-none"={
                   out <- paste0("#' ", line)
               },
               "text-title"={
                   ss <- gsub( .line.types[ code ], "\\1", line)             
                   out <- paste0("#' # ", ss)
               },
               "text-author"={
                   ss <- gsub( .line.types[ code ], "\\1", line)             
                   out <- paste0("#' #### ", ss)
               },
               "text-section"={
                   ss <- gsub( .line.types[ code ], "\\1", line)             
                   out <- paste0("#' ## ", ss)
               },
               "text-subsection"={
                   ss <- gsub( .line.types[ code ], "\\1", line)
                   out <- paste0("#' ### ", ss)
               },
               "text-subsubsection"={
                   ss <- gsub( .line.types[ code ], "\\1", line)             
                   out <- paste0("#' #### ", ss)
               },
               
               "text-label"=,
                   "text-documentclass"=,
                       "text-maketitle"=,
                           "text-usepackage"=,
                               "text-sweaveopts"={
                               keep[ii] <- FALSE             
                               out <- ""
                           },
               "text-newslide" ={
                   out <- "#' ---"
               },
               "text-begindisplaymath"=,
                   "text-enddisplaymath"={
                       out <- "#' $$"
                   },

               "text-beginitemize"=,
                   "text-enditemize"={
                           keep[ii] <- FALSE
                           out <- ""        
                       },
               "text-item"={
                   out <- paste("#' * ", gsub("\\\\item","", line))
               },
               "text-@"={
                   keep[ii] <- FALSE
                   out <- ""        
                   instate <- "code"
               },
               
               "code-@"={
                   keep[ii] <- FALSE
                   out <- ""
                   instate <- "text"
               },
               "code-<<>>"={
                   keep[ii] <- FALSE
                   out <- ""
               },
               "code-none"={
                   out <- line
               },
               "code-sweavehooks"={
                   keep[ii] <- FALSE
                   out <- ""
               }
               )
        print(paste0("    -> out = ", out))
        outlines[ii] <- out
    }
    
    
###
    outlines <- outlines[keep]
    outlines <- outlines[nchar(outlines)>0]
    ## Remove trailing whites
    outlines <- gsub("([^ \t\r\n])[ \t]+$", "\\1", outlines)
    ## To handle my Ã¥xxxÃ¥ markup of typewriter font
    outlines <- gsub("Ã¥", "`", outlines)
    
    cat(paste0("scriptfile: ", outfile, " has been created"))
    writeLines(outlines, outfile)
    
    if (spin)
        knitr::spin(outfile)
}
