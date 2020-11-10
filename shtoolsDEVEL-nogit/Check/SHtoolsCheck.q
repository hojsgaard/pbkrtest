f <- list.files("../SHtools/R", pattern="\\.R",full.names=T)
f <- f[(1:length(f))[-grep("~",f)]]
sapply(f,source)

mergeweaveall("Main.Rnw")

extractSource("Main.tex")


str<-scan("Main.tex",what="character",sep="\n")
begin<-min(grep("%%% Input from file",str))
end <-max(grep("%%% End of input from file",str))
range <- begin:end

str <- cutChunk("Main.R")

str <- c("###","bPlot","ePlot","bwrapPlot","ewrapPlot")
    
    <<<<<<<f
weaveall("Main.Rnw")

mergefiles("Main.Rnw")
