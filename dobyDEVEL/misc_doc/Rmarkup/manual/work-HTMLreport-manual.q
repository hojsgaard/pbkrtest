## = HTMLreport() =
library(doBy)
flist <- list.files("../../doBy/R",pattern=glob2rx("*.R"),full.names=TRUE);
sapply(flist,source)
weaveOne("HTMLreport-manual")

sapply(flist,source)
Rscript2HTML("Example1-Puromycin.R")

##a2ps --portrait -1 -B --borders=0 PuromycinAnalysis-report.R

fname <- "Ex1Puro"

sss<-sprintf("a2ps --portrait -1 -B --borders=0 %s.R", fname)
system(sss)

sss<-sprintf("ps2eps -f %s.ps %s.eps", fname, fname)
system(sss)

sss<-sprintf("epstool --copy --bbox %s.eps __tmp__.eps", fname)
system(sss)

sss<-sprintf("epstopdf --outfile=%s.pdf  __tmp__.eps ", fname)
system(sss)

sss<-sprintf("htmldoc -f %s-REPORT.pdf %s-REPORT.html", fname, fname)
system(sss)



system("a2ps --portrait -1 -B --borders=0 Example1-Puromycin.R")
system("ps2eps -f Example1-Puromycin.ps Example1-Puromycin.eps")
system("epstool --copy --bbox Example1-Puromycin.eps __tmp__.eps")
system("epstopdf --outfile=Example1-Puromycin.pdf  __tmp__.eps ")
system("htmldoc -f Example1-Puromycin-REPORT.pdf Example1-Puromycin-REPORT.html")



## Correct bounding box in .eps file
epstool --copy --bbox input.eps output.eps

## Convert .html to .pdf
htmldoc -f Example1-Puromycin-REPORT.pdf Example1-Puromycin-REPORT.html

## Convert plain text file to .ps
a2ps --portrait -1 -B --borders=0 Example1-Puromycin.R

## Convert .eps to pdf
epstopdf input.eps output.pdf

## Convert .ps to .eps
ps2eps -f input.ps output.eps
