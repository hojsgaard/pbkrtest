## get.token <- function(lll, key){
##     ans <- which(c(lapply(key, grepl, lll),recursive=T))
##     if (length(ans)==0)
##       ans <- 0
##     ans
##   }

types <- c("^\\\\section\\{(.*)\\}",
           "^\\\\subsection\\{(.*)\\}",
           "^\\\\subsubsection\\{(.*)\\}",
           "^\\\\label\\{(.*)\\}",
           "^\\\\SweaveOpts\\{(.*)\\}",
           "^\\\\newslide .*",          
           "@",
           "<<(.*)>>=",
           "\\\\begin\\{displaymath\\}",
           "\\\\end\\{displaymath\\}",
           "SweaveHooks"
           )

tokens <- c("none","section",
            "subsection",
            "subsubsection",
            "label",
            "sweaveopts",
            "newslide",
            "@",
            "<<>>",
            "begindisplaymath",
            "enddisplaymath",
            "sweavehooks"
            )

.scan.line <- function(line){
    jj <- which( sapply(types, grep, line) == 1 )
    if (length(jj)==0)
        jj <- 0
    jj
}


.get.token <- function(line){
    jj <- .scan.line(line)
    tokens[jj+1]
}


infile <- "basic.Rnw"

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
    print(paste0(code, "-", token, " : ", instate, "-> ", line))
    st <- paste0(instate,"-",token)
    print(st)
    switch(st,
           "text-none"={
               out <- paste0("#' ", line)
           },
           "text-section"={
               ss <- gsub( types[ code ], "\\1", line)             
               out <- paste0("#' # ", ss)
           },
           "text-subsection"={
               ss <- gsub( types[ code ], "\\1", line)
               out <- paste0("#' ## ", ss)
           },
           "text-subsubsection"={
               ss <- gsub( types[ code ], "\\1", line)             
               out <- paste0("#' #### ", ss)
           },
           "text-label"=,
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
## To handle my åxxxå markup of typewriter font
outlines <- gsub("å", "`", outlines)

writeLines(outlines, outfile)
knitr::spin(outfile)





ss <- subSeq(outlines)
ss <- ss[ss$value == "#'",]
ss <- ss[ss$slength>1,]

2579

25790

0 * 1
9 * 10
7 * 100
5 * 1000
2 * 10000

2579000

0 * 1
0 * 10
0 * 100
9 * 1000
7 * 10000
5 * 100000
2 * 1000000

2.579.000

1.234
11.036
2.579.000



ss <- gsub( types[ code ], "\\1", line)

subSeq(outlines, item=="\\#'")


x <- sample(letters[1:5], 10, replace=T)
subSeq(x, item="a")

dput(outlines)

outlines <- c("#' ", "options(SweaveHooks=list(fig=function() par(mar=c(2, 2, 2, 2) + 0.1)))", 
"#' ", "#'   ", "#' ", "#' # Summarizing data", "#' ", "#' ", 
"#' The åshoeså data is a list of two vectors, giving the wear of shoes of", 
"#' materials åXå and åYå for one foot each of ten boys.", 
"data(shoes, package=\"MASS\")", "names(shoes) <- c(\"x\",\"y\")", 
"shoes", "#' ", "#' First focus on data for material åAå; ", 
"x <- shoes$x; x", "#' ", "#' We shall look at measures of where is the \"location\" or \"center\" of the data and what is the", 
"#' \"spread\" of data. ", "#' ", "#' ", "#' For the sum $x_1 + x_2 + x_3 + \\dots + x_7+ x_8$ we write", 
"#' $$", "#'   x_. = \\Sigma_{i=1}^8 x_i = x_1 + x_2 + x_3 + \\dots + x_7+ x_8", 
"#' $$", "#' ")

a <- subSeq(outlines)

a$value


subSeq(outlines, item="#' ")








    ## if (instate=="text" & token=="none"){
    ##     out <- paste0("#' ", line)
    ##     print(paste0("   out-> ", out)); outlines[ii] <- out        
    ##     next
    ## }
    ## if (instate=="text" & code==1){
    ##     ss <- gsub( types[ code ], "\\1", line)
    ##     out <- paste0("#' # ", ss)
    ##     print(paste0("   out-> ", out)); outlines[ii] <- out        
    ##     next
    ## }   
    ## if (instate=="text" & code==2){
    ##     ss <- gsub( types[ code ], "\\1", line)
    ##     out <- paste0("#' ## ", ss)
    ##     print(paste0("   out-> ", out)); outlines[ii] <- out        
    ##     next
    ## }   
    ## if (instate=="text" & code==3){
    ##     ss <- gsub( types[ code ], "\\1", line)
    ##     out <- paste0("#' ### ", ss)
    ##     print(paste0("   out-> ", out)); outlines[ii] <- out        
    ##     next
    ## }   
    ## if (instate=="text" & code %in% c(4,5,6)){
    ##     keep[ii] <- FALSE
    ##     out <- ""
    ##     print(paste0("   out-> ", out)); outlines[ii] <- out        
    ##     next
    ## }
    ## if (instate=="text" & token %in% c("begindisplaymath", "enddisplaymath")){
    ##     out <- "#' $$"
    ##     print(paste0("   out-> ", out)); outlines[ii] <- out        
    ##     next
    ## }
    ## if (instate=="text" & token=="@"){
    ##     keep[ii] <- FALSE
    ##     out <- ""        
    ##     instate <- "code"
    ##     print(paste0("   out-> ", out)); outlines[ii] <- out
    ##     next
    ## }
    ## if (instate=="code" & token=="@"){
    ##     keep[ii] <- FALSE
    ##     out <- ""
    ##     instate <- "text"
    ##     print(paste0("   out-> ", out)); outlines[ii] <- out
    ##     next
    ## }
    ## if (instate=="code" & code %in% c(8)){
    ##     out <- ""
    ##     print(paste0("   out-> ", out)); outlines[ii] <- out
    ##     next
    ## }
    ## if (instate=="code" & code==0){
    ##     out <- line
    ##     print(paste0("   out-> ", out)); outlines[ii] <- out        
    ##     next
    ## }
    ## if (instate=="code" & code==6){
    ##     out <- ""
    ##     instate <- "text"
    ##     print(paste0("   out-> ", out)); outlines[ii] <- out
    ##     next
    ## }








RweaveHTMLreportSetup <- function(srclines, details=0){
  
    key <- c(
        "^$",				## empty line
        "^#",    				## text line
        "^ *##? *@@|^ *##? *<<",	        ## begin code chunk
            "^ *##? *@ *$" 			## end code chunk
        )
  
  state <- c("RCODE", "emptyLine", "textLine", "beginCode", "endCode")
  
  
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



















library(doBy)

flist <- list.files("doBy/R",pattern=glob2rx("*.R"),full.names=TRUE);
sapply(flist,source)

lll <- " ## # sadaddas # asdk #"

Rmarkup("script/SweaveEx3.R", destdir="./report", encoding="latin1", details=4)


lll <- " # # # sadaddas # asdk #"
(tst <- (length(grep("^ *###", lll))>0))
if (tst) { # must create comment in output file
	cat("A todo comment\n")
   (lll2 <- gsub("^ *###[ |#]*(.*)", "<!-- \\1 !-->", lll))
} else {
	cat("A text\n")
   (lll2 <- gsub("^[ |#]*(.*)", "\\1", lll))
}

We know it is a text line.
If the line starts with "(space)*### (space)*" then it is a todo line
If the line starts with "(space)* one or two hashes" then line must go to output..





gsub("^[[:blank:]]*##?[[:blank:]]*(.*)", "\\1", lll)
    


Rmarkup("script/Puromycin-3.R", destdir="./report")
  
sss <- "wkhtmltopdf ./report/Puromycin-3.html ./report/Puromycin-3.pdf"
cat(sss)
system(sss)


sapply(flist,source)
Rmarkup("script/SweaveEx.R", destdir="./report", details=0, encoding="latin1", cssfile="R2HTML.css")



sapply(flist,source)
Rmarkup("scripts/SweaveEx2.R", destdir="./reports", details=0, encoding="latin1", cssfile="R2HTML.css")



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
  
lll<-  "## @@"
  get.token(lll, key)
lapply(key, grepl, lll)
  
  get.token("## <<>>=", key)











anslines   <- readLines("reports/SweaveEx2.html")

  for (ii in 1:length(anslines)){
    ss <- anslines[ii]
    if (length(grep("<!--[^<!--]*!-->",ss))>0){
      ss <- gsub("[ |\\]*","",ss)
      anslines[ii] <- ss
    }
  }
  
  
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

cbind(tokvec, inchvec, inresvec, anslines)

  
  for (ii in 1:length(anslines)){
    if (inchvec[ii]==1){
      lll <- anslines[ii]
      lll <- gsub("<[^>]*>","",lll)
      lll <- gsub("[[:blank:]]*,",",",lll)
      lll <- sprintf("<PRE>%s</PRE>", lll)
      anslines[ii] <- lll
    }
  }
  
anslines
write(anslines,"fff.html")
	
	
incodevec <- 
	
c("txt", state)[(tokvec+1)]

	
ss<- c(  "<!-- \\end{Sinput}  !-->")

grep("<!--[^<!--]*!-->",ss)

gsub("[ |\\]*","",ss)




library(tools)
Sweave("file.snw",driver=RweaveHTML)

lll <- "<br><li>Kruskal-Wallis chi-squared =<b> 29.2666 </b> , df =<b> 4 </b> , p-value =<font class='pvalue'> 6.901e-06 </font>"

lll2 <- gsub("<[^>]*>","",lll)

gsub("[[:blank:]]*,",",",lll2)

sed -e �s/<[^>]*>//g� foo.html




  key <- c(
           "^$",				## empty line
           "^#",    				## text line
           "^ *##? *@@|^ *##? *<<",	    ## begin code chunk
           "^ *##? *@ *$" 			## end code chunk
           )
  
  state <- c("RCODE", "emptyLine", "textLine", "beginCode", "endCode")


  
  get.token <- function(lll, key){
    ans <- which(c(lapply(key, grepl, lll),recursive=T))
    if (length(ans)==0)
      ans <- 0
    ans
  }


lll <- "## @@@"  
lll <- "## <<"  
lll <- "## @"  
get.token(lll, key)  






sapply(flist,source)
Rmarkup("HTML/PuromycinAnalysis10.R", destdir="./HTML", details=12, cssfile="R2HTML.css")


sapply(flist,source)
Rmarkup("HTML/PuromycinAnalysis10.R", destdir=".\\HTML")

sapply(flist,source)
Rmarkup("HTML/PuromycinAnalysis10.R", destdir="./HTML")

sapply(flist,source)
Rmarkup("HTML/PuromycinAnalysis10.R", destdir="HTML")

sapply(flist,source)
Rmarkup("HTML/PuromycinAnalysis10.R", destdir="./HTML2")

sapply(flist,source)
Rmarkup("HTML/PuromycinAnalysis10.R", destdir=".\\HTML2")



sapply(flist,source)
�5y55ZRmarkup("HTML/Puro10.R", cleanup=T)

sapply(flist,source)
Rmarkup("scripts/Puro10.R", destdir="reports")

sapply(flist,source)
Rmarkup("HTML/Puro20.R", cleanup=T)


sapply(flist,source)
Rmarkup("HTML/PuromycinAnalysis1.R", destdir="./HTML", cssfile="R2HTML.css")

Rmarkup("HTML/PuromycinAnalysis2.R", destdir="./HTML")

Rmarkup("HTML/PuromycinAnalysis2.R", destdir="./HTML", cssfile="R2HTML.css")B    

Rmarkup("HTML/PuromycinAnalysis3.R", destdir="./HTML")

Rmarkup("HTML/PuromycinAnalysis3.R", destdir="./HTML", cssfile="R2HTML.css")

Rmarkup("HTML/PuromycinAnalysis3.R", destdir="./HTML", cssfile="Pastel.css")


library(R2HTML)
flist <- list.files("doBy/R",pattern=glob2rx("*.R"),full.names=TRUE);
sapply(flist,source)

sapply(flist,source)
Rmarkup("HTML/PuromycinAnalysis1.R", destdir="./HTML")

sapply(flist,source)
Rmarkup("HTML/PuromycinAnalysis1.R", destdir="./HTML", cssfile="R2HTML.css")

sapply(flist,source)
Rmarkup("HTML/PuromycinAnalysis1.R", destdir="./HTML", cssfile="Pastel.css")



###################################################
####
#### SANDBOX 
####
###################################################

lll <- "<p><xmp class=command>> plot(rate ~ conc, data = Puromycin, col = as.numeric(state))</xmp></p>"

key <- c(
"^$",						## empty line
"^##?$",    				## text line
"^###+",    				## silent line
"^ *##? *@@|^ *##? *<<",	## begin code chunk
"^ *##? *@ *$"				## end code chunk
)

state <- c("emptyLine", "textLine", "silentLine", "beginCode", "endCode")

get.token <- function(lll, key){
	which(c(lapply(key, grepl, lll),recursive=T))
}

lll <- ""
state[get.token(lll, key)]
lll <- "##"
state[get.token(lll, key)]
lll <- "#"
state[get.token(lll, key)]
lll <- "###"
state[get.token(lll, key)]
lll <- "##@@fig"
state[get.token(lll, key)]
lll <- "## <<"
state[get.token(lll, key)]
lll <- "## @"
state[get.token(lll, key)]


















