xsavePlot <- function (filename = "Rfigure", path = ".", type = "epspdf", caption = NULL, 
    label = NULL, includearg = "width=0.9\\textwidth") 
{
    cl <- match.call()
    full.path <- paste(path, filename, sep = "\\")
    print(full.path)
    if (type == "epspdf") {
      savePlot(paste(full.path,"eps",sep="."), type = "eps")
      savePlot(paste(full.path,"pdf",sep="."), type = "pdf")
    }
    else {
      savePlot(paste(full.path,type,sep="."), type = type)
    }
    if (is.null(label)) 
        label <- rev(unlist(strsplit(full.path, "/")))[1]
    cat("%% Automatically generated environment\n")
    cat("%%", paste(as.expression(cl)), "\n")
    cat("\\begin{figure}[ht]\n \\centering\n")
    cat(paste(" \\includegraphics[", includearg, "]{", full.path, 
        "}\n", sep = ""))
    cat(paste(" \\caption{fig:", label, " ", caption, "}\n", 
        sep = ""))
    cat(paste(" \\label{fig:", label, "}\n", sep = ""))
    cat("\\end{figure}\n")
}


# xsavePlot <-
# function (filename = "Rfigure", path = "", type = "epspdfjpg", caption = NULL,
#     label = NULL, includearg="width=0.9\\textwidth")
# {
#   cl <- match.call()
#   full.path <- paste(path, filename, sep = "")
#     if (type == "epspdfjpg") {
#         savePlot(full.path, type = "eps")
#         savePlot(full.path, type = "pdf")
#         savePlot(full.path, type = "jpg")
#     }
#     else {
#         savePlot(full.path, type = type)
#     }
#     if (is.null(label))
#         label <- rev(unlist(strsplit(full.path, "/")))[1]
#     cat("%% Automatically generated environment\n")
#     cat("%%",paste(as.expression(cl)),"\n")
#     cat("\\begin{figure}[ht]\n \\centering\n")
#     cat(paste(" \\includegraphics[",includearg,"]{", full.path,
#         "}\n", sep = ""))
#     cat(paste(" \\caption{fig:", label, " ", caption, "}\n", sep = ""))
#     cat(paste(" \\label{fig:", label, "}\n", sep = ""))
#     cat("\\end{figure}\n")
# }





# xsavePlot <- function (filename = "Rfigure", path = "",
#                        type = "epspdf",caption=NULL,label=NULL)
# {
#   full.path <<- paste(path, filename, sep = "")
#   if (type == "epspdf") {
#     savePlot(full.path, type = 'eps')
#     savePlot(full.path, type = 'pdf')
#   }
#   else {
#     savePlot(full.path, type = type)
#   }
  
#   if (is.null(label))
#     label <- rev(unlist(strsplit(full.path,"/")))[1]
  
#   cat("% Automatically generated environment\n")
#   cat("\\begin{figure}[ht]\n \\centering\n")
#   cat(paste(" \\includegraphics\[width=0.9\\textwidth\]\{", full.path, "\}\n",sep=''))
#   cat(paste(" \\caption\{", caption, "\}\n",sep=''))
#   cat(paste(" \\label\{fig:", label, "\}\n",sep=''))
#   cat("\\end{figure}\n")
# }




# shsavePlot  <- function(filename="Rfigure", path=".\\",type="epspdf"){
#   full.path   <- paste(path,filename,sep="")
#   if (type=="epspdf"){
#     R.type1 <- "ps"
#     R.type2 <- "pdf"
#     savePlot(full.path,type=R.type1)
#     savePlot(full.path,type=R.type2)
#   }
#   else{
#     if (type=="eps" || type=="ps")
#       R.type  <- "ps"
#     else
#       R.type  <- type;
#     savePlot(full.path,type=R.type)
#   }
#   if (type=="eps" || type=="epspdf"){
#     print(paste(full.path,".ps",sep=''))
#     str <- paste("move ", paste(full.path,".ps",sep=''), paste(full.path,".eps",sep=''))
#     print (str)
#     shell(str)
#   }
# }
