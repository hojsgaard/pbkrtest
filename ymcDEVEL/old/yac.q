tkdestroy(tt)
source("ymc/R/ymc.R")
ymc()





In> 0+x;
Out> x;
In> x+1*y;
Out> x+y;
In> Sin(ArcSin(alpha))+Tan(ArcTan(beta));
Out> alpha+beta;
In> (x+y)^3-(x-y)^3
Out> (x+y)^3-(x-y)^3;
In> Simplify(%)
Out> 6*x^2*y+2*y^3;


In> Taylor(x,0,3) Exp(x)
Out> 1+x+(1/2)*x^2+(1/6)*x^3;
In> PrettyForm(%);


Sin(Pi)
Exp(0)
N(Exp(4))


0+x;
x+1*y;
Sin(ArcSin(alpha))+Tan(ArcTan(beta));
(x+y)^3-(x-y)^3
Simplify(%)

a <- c("asd","uu","","")

tkdestroy(tt)
tt  <- tktoplevel()
txt <- tktext(tt, height=20,width=50, font=txtfont, fg='blue')
tkpack(txto,side="left")
tkpack(txt)

examples <- c(" ## Examples of Yacas code", "Sin(Pi)","Exp(0)","N(Exp(4))"," # end of examples")
lapply(examples, function(ex) tkinsert(txt,"end", paste(ex,"\n")))

examples <- c(" ## Examples of Yacas code", "In> Sin(Pi)","Exp(0)","N(Exp(4))"," # end of examples")







scr <- tkscrollbar(tt, repeatinterval=5,
                       command=function(...)tkyview(txt,...))
txt <- tktext(tt,height=5,width=50,bg="white",font="courier",
    yscrollcommand=function(...)tkset(scr,...))

scro <- tkscrollbar(tt, repeatinterval=5,
                       command=function(...)tkyview(txto,...))
txto <- tktext(tt, height=5,width=50, font=txtfont,
    yscrollcommand=function(...)tkset(scro,...))

#tkgrid.configure(scr,sticky="w")
tkfocus(txt)

tkmove(txt)


tkgrid(txt,scr)






#for (i in (1:100)) tkinsert(txt,"end",paste(i,"^ 2 =",i*i,"\n"))


tkconfigure(txt, state="normal")

tt <- tktoplevel()
tkgrid(tklabel(tt,text="Here is a centered string of text."))

tkgrid(tklabel(tt,text="Left"),sticky="w")
tkgrid(tklabel(tt,text="Right"),sticky="e")


txto <- tktext(tt, height=20,width=50, font=txtfont)


  tkpack(txt,side="left")
  tkpack(txto,side="right")


  txtfont <- tkfont.create(family="courier", size=10)
  rcmdtitle <- "Yacas Mini Commander"
  wfile <- ".txt"
  tt <- tktoplevel()
  tkwm.title(tt, rcmdtitle)
  txt <- tktext(tt, height=20,width=50,font=txtfont)
  txto <- tktext(tt, height=20,width=50, font=txtfont)

  tkpack(txt,side="left")
  tkpack(txto,side="right")

  tkinsert(txto, "0.0", 'asdasd')



  addSemi <- function(x){
    nc <- nchar(x)
    if (substr(x,nc,nc)!=';')
      x <- paste(x,";",collapse='',sep='')
    return(x)
  }

  dropSemi <- function(x){
    nc <- nchar(x)

    if (substr(x,nc,nc)==';')
      x<- substr(x,1,nc-1)
    return(x)
    }
  
x <- 'asas;'





tkdestroy(tt)
tt <- tktoplevel()
edge <- c("top","right","bottom","left")
but <- lapply(1:4, function(i) tkbutton(tt,text=edge[i]))
for (i in 1:4)
tkpack(but[[i]], side=edge[i],fill='both')

tkdestroy(tt)
tt <- tktoplevel()
edge <- c("top","right","bottom","left")
but <- lapply(1:4, function(i) tktext(tt))
for (i in 1:4)
tkpack(but[[i]], side=edge[i],fill='both')










                                        #Yscroll <- tkscrollbar(tt, repeatinterval=5,
  #      command=function(...) tkyview(tt, ...))
  #tkconfigure(tt, yscrollcommand=function(...) tkset(Yscroll, ...))
  #tkgrid(tt, Yscroll)



#########Binomial
plot.binom <- function(n,p){
p.k <- dbinom(0:n,n,p)
barplot(p.k,names.arg=0:n)
}


require(tcltk) || stop("tcltk support is absent")
########################################################
## R-Teil

local({
    p <- tclVar(0)
    p.sav <- 1
    n <- 15
    p.k <- NULL
    schiefe <- NULL

    replot <- function(...) {
        if (is.null(p.k)) return() # too early...
        p.sav <<- my.p <- as.numeric(tclvalue(p))
        p.k <- dbinom(0:n,n,my.p)
        schiefe <- (1-2*my.p)/(sqrt(n*my.p*(1-my.p)))
        eval(substitute(barplot(p.k,names.arg=0:n,ylab="f(x)")))
        eval(substitute(text(x=n,y=(max(p.k)+max(p.k)*0.1),
            labels=paste("Schiefe= ",round(schiefe,2)," Wahrscheinlichkeit p= ",my.p),xpd=TRUE)))
    }

replot.maybe <- function(...)
    {
        if (as.numeric(tclvalue(p)) != p.sav) replot()
    }

regen <- function(...) {                                            # Daten regenerieren
        p.k <<- dbinom(0:n,n,as.numeric(tclvalue(p)))
        replot()
    }

#########################################################
###Tcl/Tk-Teil

    base <- tktoplevel()
    tkwm.title(base, "Schiefe")

    spec.frm <- tkframe(base,borderwidth=2)
    left.frm <- tkframe(spec.frm)
    right.frm <- tkframe(spec.frm)

    frame1 <-tkframe(left.frm, relief="groove", borderwidth=2)
    tkpack(tklabel (frame1, text="Wahrscheinlichkeit p"))
    # Slider für p
    tkpack(tkscale(frame1, command=replot.maybe, from=0, to=1,
                   showvalue=T, variable=p,
                   resolution=0.05, orient="horiz"))

    tkpack(frame1,side="left", anchor="n")

    frame3 <- tkframe(right.frm, relief="groove", borderwidth=2)
    # tkpack(tklabel(frame3, text=e))

    tkpack(frame1, fill="x")
    # tkpack(frame3)
    tkpack(left.frm, right.frm,side="left", anchor="n")

    ## `Bottom frame' (on base):
    q.but <- tkbutton(base,text="Beenden",
                      command=function(){tkdestroy(base);dev.off()})

    tkpack(spec.frm, q.but)

    regen()
    # replot.maybe()
})

