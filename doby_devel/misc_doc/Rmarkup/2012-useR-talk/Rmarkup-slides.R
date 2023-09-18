### R code from vignette source 'Rmarkup-slides.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: src-Rmarkup.Rnw:4-5
###################################################
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)


###################################################
### code chunk number 2: src-Rmarkup.Rnw:87-90
###################################################
Rmarkup("SweaveEx1.R", 
    encoding = "latin1",     # because of the 'ø's
    cssfile  = "R2HTML.css") # optional css file


###################################################
### code chunk number 3: src-Rmarkup.Rnw:106-109
###################################################
sss <- "wkhtmltopdf SweaveEx1.html SweaveEx1.pdf"
cat(sss)
system(sss)


###################################################
### code chunk number 4: src-Rmarkup.Rnw:262-268
###################################################
Rmarkup("script/SweaveEx2.R", 
        destdir  = "./report", 
        encoding = "latin1",
        cssfile  = "R2HTML.css",
        parms = list(height = 200, width = 200)
        )


###################################################
### code chunk number 5: src-Rmarkup.Rnw:274-276
###################################################
sss <- "wkhtmltopdf ./report/SweaveEx2.html ./report/SweaveEx2.pdf"
system(sss)


