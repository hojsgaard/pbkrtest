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

sed -e ‘s/<[^>]*>//g’ foo.html




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
€5y55ZRmarkup("HTML/Puro10.R", cleanup=T)

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


















