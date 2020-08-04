texsec_up <- function(filename, overwrite=FALSE){

    if (!file.exists(filename))
        stop("file does not exist\n")
    
    hd <- gsub("\\..*", "", filename)
    tl <- gsub(".*\\.", "", filename)   
    inlines <- readLines(filename)
    outlines <- inlines
    
    outlines <- gsub("\\\\section", "\\\\chapter", outlines)
    outlines <- gsub("\\\\subsection", "\\\\section", outlines)
    outlines <- gsub("\\\\subsubsection", "\\\\subsection", outlines)
    outlines <- gsub("\\\\paragraph", "\\\\subsubsection", outlines)
    
    outlines
    
    if (overwrite){
        writeLines(outlines, filename)
        writeLines(inlines, paste0(hd, "-bak.", tl))
    } else {
        writeLines(outlines, paste0(hd, "-up.", tl)) 
    }
    cat("Done upgrading tex\n")
}

texsec_down <- function(filename, overwrite=FALSE){

    if (!file.exists(filename))
        stop("file does not exist\n")
    
    hd <- gsub("\\..*", "", filename)
    tl <- gsub(".*\\.", "", filename)   
    inlines <- readLines(filename)
    outlines <- inlines

    outlines <- gsub("\\\\subsubsection", "\\\\paragraph", outlines)
    outlines <- gsub("\\\\subsection", "\\\\subsubsection", outlines)
    outlines <- gsub("\\\\section", "\\\\subsection", outlines)    
    outlines <- gsub("\\\\chapter", "\\\\section", outlines)
   
    outlines
    
    if (overwrite){
        writeLines(outlines, filename)
        writeLines(inlines, paste0(hd, "-bak.", tl))
    } else {
        writeLines(outlines, paste0(hd, "-up.", tl)) 
    }
    cat("Done upgrading tex\n")
}





