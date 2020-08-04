ymc <- function() {
  require(tcltk)
  require(Ryacas)
  txtfont <- tkfont.create(family="courier", size=10)
  rcmdtitle <- "Yacas Mini Commander"
  wfile <- ".txt"
  tt <<- tktoplevel()
  tkwm.title(tt, rcmdtitle)

  ## Input window
  scr <- tkscrollbar(tt, repeatinterval=5,
                     command=function(...)tkyview(txt,...))
  txt <- tktext(tt, height=20, width=50,font=txtfont,
                yscrollcommand=function(...)tkset(scr,...)
                )
  #tkgrid(txt,scr)
  #tkgrid.configure(scr,sticky="ns")

  ## Output window
  scro <- tkscrollbar(tt, repeatinterval=5,
                     command=function(...)tkyview(txto,...))
  txto <- tktext(tt, height=20, width=50,font=txtfont,
                yscrollcommand=function(...)tkset(scro,...)
                )
  #tkgrid.configure(scro,sticky="ns")
  tkgrid(txt,scr,txto,scro)

  #txto <- tktext(tt, height=20, width=50,font=txtfont)

  #tkpack(txt,scr,side="left")
  #tkpack(txto,scro,side="right")

  tkfocus(txt)
  
  ##Yscroll <- tkscrollbar(tt, repeatinterval=5,
  ##      command=function(...) tkyview(tt, ...))
  ##tkconfigure(tt, yscrollcommand=function(...) tkset(Yscroll, ...))
  ##tkgrid(tt, Yscroll)

  #tkpack(txt)
  save <- function() {
    file <- tclvalue(tkgetSaveFile(
                                   initialfile=tclvalue(tkfile.tail(wfile)),
                                   initialdir=tclvalue(tkfile.dir(wfile))))
    if (!length(file)) return()
    tkwm.title(tt, paste(rcmdtitle,file))
    chn <- tkopen(file, "w")
    tkputs(chn, tclvalue(tkget(txt,"0.0","end")))
    tkclose(chn)
    wfile <<- file
  }
  load <- function() {
    file <- tclvalue(tkgetOpenFile())
    if (!length(file)) return()
    chn <- tkopen(file, "r")
    tkinsert(txt, "0.0", tclvalue(tkread(chn)))
    tkclose(chn)
    wfile <<- file
    tkwm.title(tt, paste(rcmdtitle,file))
  }
  run <- function() {
    selection <- strsplit(tclvalue(tktag.ranges(txt, "sel")), " ")[[1]]
    if (length(selection)==0){
      str <- tclvalue(tkget(txt,"0.0","end"))
    } else {
      str <- tclvalue(tkget(txt,selection[1],selection[2]))
    }
    write(str,"yac.txt")
    yy <- readLines("yac.txt", n=-1)
    yy <- yy[nchar(yy)>0]
    ##print(yy)
    yy <- gsub ("Out>","#",yy)
    yy <- gsub ("In>", "",yy)
    com<- grep("#",yy)
    if (length(com)>0)
      yy<- yy[-com]
    lapply(yy, function(arg){
      #so <- yacyac(arg)
      so <- yacyac(arg)
      print(so)
      so <- dropSemi(so)
#      print(so)
      tkinsert(txto, "end", paste("yac>", arg,"\n"))
      lapply(so, function(s){
 #            print(s)
             tkinsert(txto, "end", paste("   ",s,"\n"))})
      tksee(txto, "end")
    })
    }

  close <- function(){
    response <- tclvalue(tkmessageBox(message="Exit?",
                                      icon="question", type="okcancel", default="cancel"))
    if (response=="cancel") return()
    response2 <- tkmessageBox(message="Save file?",
                              icon="question", type="yesno", default="yes")
    print(response2)
    if ("yes" == tclvalue(response2)) 
      save()
    tkdestroy(tt)
  }
  examples <- function(){
    examples <- c("## Examples of Yacas code",
                  "Sin(Pi)","Exp(0)","N(Exp(4))",
                  "## end of examples")
    lapply(examples, function(ex) tkinsert(txt,"end", paste(ex,"\n")))
  }
  
  about <- function()
    {
      abouttxt <-
        paste(
              "Yacas Mini Commander (ymc)\n",
              "Usage:\n",
              " - To submit the entire content of the editor, click on Run or hit Alt-r or Ctrl-y\n",
              " - To submit some lines only, highlight those lines, click on Run or hit Alt-R or Ctrl-y\n",
              "Note:\n",
              " - There is no scroll bar, but page-up/page-down and the arrows can be used for scrolling\n",
              " - Drag-and-drop of text is not implemented\n",
              "\n",
              "Author: Søren Højsgaard, sorenh@agrsci.dk \n"
              )
      tkmessageBox(message=abouttxt, icon="info", type="ok")
  }
  
  topMenu <- tkmenu(tt)
  tkconfigure(tt, menu=topMenu)
  fileMenu <- tkmenu(topMenu, tearoff=FALSE)
  tkadd(fileMenu, "command", label="Open",       command=load)
  tkadd(fileMenu, "command", label="Save",       command=save)
  tkadd(fileMenu, "command", label="Quit",       command=close)
  tkadd(topMenu, "cascade", label="File",        menu=fileMenu)
  
  tkadd(topMenu, "command", label="Run",         command=run)
  tkadd(topMenu, "command", label="Yacas examples",         command=examples)
  tkadd(topMenu, "command", label="About/Help",  command=about)
  
  tkbind(tt, "<Control-y>", run)
  tkbind(tt, "<Alt-p>", run)
}
                                        #tkscript()


yacyac <- function(x){

  addSemi <- function(x){
    nc <- nchar(x)
    if (substr(x,nc,nc)!=';')
      x <- paste(x,";",collapse='',sep='')
    return(x)
  }

  if (is.expression(x))
    x <- paste(x)
#  x <- R2Yacas(x)
  x<-addSemi(x)
  writeLines(x, .yacCon)

  yac.out <- readLines(.yacCon)

  while (length(yac.out)==0)
    yac.out <- readLines(.yacCon)
  flush(.yacCon)
  #print(yac.out)

  if (yac.out[1]=="]")
    val2 <- yac.out[2]
  else{
    val2 <- yac.out[1:(length(yac.out)-3)]
  }
  #print(val2)

                                        #  val2 <- Yacas2R(yac.out[2])
  return(val2)
}

dropSemi <- function(x){
  ncc <- nchar(x[1])
  
  if (substr(x[1],ncc,ncc)==';')
    x[1]<- substr(x[1],1,ncc-1)
  return(x)
}
