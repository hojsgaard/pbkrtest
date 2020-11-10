igattr <- function(file, overwrite=FALSE){ ## A latex file (typically with .tex extension)

  ee <- 0
  if (overwrite){
    outfile <- file
  } else {
    outfile <- sprintf("%s2",file)
  }
  
  inlines   <- readLines(file)
  outlines  <- rep.int(character(0), length(inlines))
  
  ig  <- NULL
  igg <- NULL
  jj  <- 1
  for (ii in 1:length(inlines)){
    inline <- inlines[ii]    
    ##cat(sprintf("inline: %s\n",inline))
    (has.drop <- length(grep("# *dropline$", inline))>0)
    if (has.drop){
      print(inline)
      #outline <- " "
    } else {
      has.igg <- length(grep("^%% *iggOpts\\{.*\\}$", inline))>0
      if (has.igg){
        igg <- gsub(".*iggOpts\\{(.*)\\}$", "\\1", inline)
        if (ee) cat(sprintf("       -> igg : %s\n", igg))
        outline <- inline
      } else {      
        has.ig <- length(grep("^%% *igOpts\\{.*\\}$", inline))>0
        if (has.ig){
          ig <- gsub(".*igOpts\\{(.*)\\}$", "\\1", inline)
          if (ee) cat(sprintf("       -> ig : %s\n", ig))
          outline <- inline
        } else {     
          has.incg <- length(grep("\\\\includegraphics", inline))>0
          if (has.incg){
            incg <- gsub("\\\\includegraphics(.*)$", "\\1", inline)
            has.sqb <- length(grep("^.*\\[.*\\].*", inline))>0
            if (has.sqb){
              outline <- inline
            } else {        
              if (!is.null(ig)){
                outline <- sprintf("\\includegraphics[%s]%s %% notice: ig is set",ig,incg)
                ig <- NULL
              } else {
                if (!is.null(igg)){
                  outline <- sprintf("\\includegraphics[%s]%s  %% notice: igg is set",
                                     igg,incg)
                } else {
                  outline <- inline
                }
              }      
            }
          } else { ## not an includegraphics-line
            outline <- inline
          }
        }
      }
      outlines[jj] <- outline
      jj <- jj + 1
    }
  }
##     }
##     outlines[ii] <- outline
##     ##cat(sprintf("outline: %s\n",outline))
##   }
  
  write(c("%% This file has been modified by igattr",
          "%% Do not edit manually",
          outlines), file=outfile)
}  
