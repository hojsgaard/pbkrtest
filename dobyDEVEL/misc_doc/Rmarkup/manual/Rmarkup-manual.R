### R code from vignette source 'Rmarkup-manual.Rnw'

###################################################
### code chunk number 1: Rmarkup-manual.Rnw:34-37
###################################################
oopt <- options()
options("digits"=4, "width"=80, "prompt"=" ", "continue"="  ")
library(doBy)


###################################################
### code chunk number 2: Rmarkup-manual.Rnw:75-85
###################################################
fname <- "Ex1Puro"

(sss<-sprintf("a2ps --portrait -1 -B --borders=0 %s.R", fname))
system(sss)

(sss<-sprintf("del %s.pdf", fname))
system(sss)

(sss<-sprintf("ps2pdf %s.ps", fname))
system(sss)


###################################################
### code chunk number 3: Rmarkup-manual.Rnw:98-100
###################################################
library(doBy)
Rmarkup("Ex1Puro.R")


###################################################
### code chunk number 4: Rmarkup-manual.Rnw:107-109
###################################################
(sss<-sprintf("htmldoc --webpage -f %s-REPORT.pdf %s-REPORT.html", fname, fname))
system(sss)


###################################################
### code chunk number 5: Rmarkup-manual.Rnw:260-261
###################################################
Rmarkup("Ex1Puro.R",cssfile="R2HTML.css", postfix="withCSS")


