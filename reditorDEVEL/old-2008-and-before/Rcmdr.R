.packageName <- "Rcmdr"
# The R Commander and command logger

# last modified 31 Jan 04 by J. Fox

Commander <- function(){
    etc <- file.path(.path.package(package="Rcmdr")[1], "etc")
    onCopy <- function(){
        selection <- strsplit(tclvalue(tktag.ranges(.log, "sel")), " ")[[1]]
        if (is.na(selection[1])) return()
        text <- tclvalue(tkget(.log, selection[1], selection[2]))
        tkclipboard.clear()
        tkclipboard.append(text)
        }    
    onDelete <- function(){
        selection <- strsplit(tclvalue(tktag.ranges(.log, "sel")), " ")[[1]]
        if (is.na(selection[1])) return()
        tkdelete(.log, selection[1], selection[2])
        }        
    onCut <- function(){
        onCopy()
        onDelete()
        }        
    onPaste <- function(){
        text <- tclvalue(.Tcl("selection get -selection CLIPBOARD"))    
        if (length(text) == 0) return()
        tkinsert(.log, "insert", text)
        }       
    onFind <- function(){
        top <- tktoplevel()
        tkwm.title(top, "Find")
        textFrame <- tkframe(top)
        textVar <- tclVar("")
        textEntry <- tkentry(textFrame, width="20", textvariable=textVar)
        optionsFrame <- tkframe(top)
        regexprVar <- tclVar("0")
        regexprCheckBox <- tkcheckbutton(optionsFrame, variable=regexprVar)
        caseVar <- tclVar("1")
        caseCheckBox <- tkcheckbutton(optionsFrame, variable=caseVar)
        directionVar <- tclVar("-forward")
        forwardButton <- tkradiobutton(optionsFrame, variable=directionVar, value="-forward")
        backwardButton <- tkradiobutton(optionsFrame, variable=directionVar, value="-backward")
        onOK <- function(){
            text <- tclvalue(textVar)
            if (text == ""){
                tkmessageBox(message="No search text specified.", 
                    icon="error", type="ok")
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                onFind()
                return()
                }
            type <- if (tclvalue(regexprVar) == 1) "-regexp" else "-exact"
            case <- tclvalue(caseVar) == 1
            direction <- tclvalue(directionVar)
            stop <- if (direction == "-forward") "end" else "1.0"
            where <- if (case) tksearch(.log, type, direction, "--", text, "insert", stop)
                        else tksearch(.log, type, direction, "-nocase", "--", text, "insert", stop)
            where <- tclvalue(where)
            if (where == "") {
                tkmessageBox(message="Text not found.",
                    icon="info", type="ok")
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                tkfocus(.commander)
                return()
                }
            if (.grab.focus) tkgrab.release(top)
            tkfocus(.log)
            tkmark.set(.log, "insert", where)  
            tksee(.log, where)
            tkdestroy(top)  
            }
        onCancel <- function() {
            if (.grab.focus) tkgrab.release(top)
            tkfocus(.commander)
            tkdestroy(top)  
            }
        buttonsFrame <- tkframe(top)
        OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
        cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)  
        tkgrid(tklabel(textFrame, text="Search for:"), textEntry, sticky="w") 
        tkgrid(textFrame, sticky="w") 
        tkgrid(tklabel(optionsFrame, text="Regular-expression search"), regexprCheckBox, sticky="w")
        tkgrid(tklabel(optionsFrame, text="Case sensitive"), caseCheckBox, sticky="w")
        tkgrid(tklabel(optionsFrame, text="Search Direction", fg="blue"), sticky="w")
        tkgrid(tklabel(optionsFrame, text="Forward"), forwardButton, sticky="w")
        tkgrid(tklabel(optionsFrame, text="Backward"), backwardButton, sticky="w")
        tkgrid(optionsFrame, sticky="w")
        tkgrid(OKbutton, tklabel(buttonsFrame, text="        "), cancelButton, sticky="w")
        tkgrid(buttonsFrame, sticky="w")
        for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
        for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
        .Tcl("update idletasks")
        tkwm.resizable(top, 0, 0)
        tkbind(top, "<Return>", onOK)
        if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
        tkwm.deiconify(top)
        if (.grab.focus) tkgrab.set(top)
        tkfocus(textEntry)
        tkwait.window(top)
        }    
    onSelectAll <- function() {
        tktag.add(.log, "sel", "1.0", "end")
        tkfocus(.log)
        }
    assign(".activeDataSet", NULL, envir=.GlobalEnv)
    assign(".activeModel", NULL, envir=.GlobalEnv)
    assign(".logFileName", NULL, envir=.GlobalEnv)
    assign(".modelNumber", 0, envir=.GlobalEnv)
    assign(".rgl", FALSE, envir=.GlobalEnv)
    log.font.size <- options("Rcmdr")[[1]]$log.font.size
    log.font.size <- if (is.null(log.font.size)) 10 else log.font.size
    assign(".logFont", tkfont.create(family="courier", size=log.font.size), envir=.GlobalEnv)
    assign(".operatorFont", tkfont.create(family="courier", size=log.font.size), 
        envir=.GlobalEnv)
    scale.factor <- options("Rcmdr")[[1]]$scale.factor
    if (!is.null(scale.factor)) .Tcl(paste("tk scaling ", scale.factor, sep=""))
    contrasts <- options("Rcmdr")[[1]]$contrasts
    contrasts <- if (is.null(contrasts)) c("contr.Treatment", "contr.poly") else contrasts
    assign(".saveOptions", options(warn=1, contrasts=contrasts, 
        na.action="na.exclude", graphics.record=TRUE), envir=.GlobalEnv)
    log.height <- options("Rcmdr")[[1]]$log.height
    log.height <- if (is.null(log.height)) "15" else as.character(log.height)
    log.width <- options("Rcmdr")[[1]]$log.width
    log.width <- if (is.null(log.width)) "70" else as.character(log.width)    
    double.click <- options("Rcmdr")[[1]]$double.click
    assign(".double.click", if (is.null(double.click)) FALSE else double.click, envir=.GlobalEnv)
    sort.names <- options("Rcmdr")[[1]]$sort.names
    assign(".sort.names", if (is.null(sort.names)) TRUE else sort.names, envir=.GlobalEnv)
    grab.focus <- options("Rcmdr")[[1]]$grab.focus
    assign(".grab.focus", if (is.null(grab.focus)) TRUE else grab.focus, envir=.GlobalEnv)
    if (.Platform$OS.type != "windows") {
        assign(".oldPager", options(pager=RcmdrPager), envir=.GlobalEnv)
        default.font.size <- options("Rcmdr")[[1]]$default.font.size
        default.font.size <- if (is.null(default.font.size)) "10" else as.character(default.font.size)
        default.font <- options("Rcmdr")[[1]]$default.font
        default.font <- if (is.null(default.font)) paste("*helvetica-medium-r-normal-*-",
            default.font.size, "*", sep="") else default.font
        .Tcl(paste("option add *font ", default.font, sep=""))
        } 
    assign(".commander", tktoplevel(), envir=.GlobalEnv)
    placement <- options("Rcmdr")[[1]]$placement
    placement <- if (is.null(placement)) "-40+40" else placement
    tkwm.geometry(.commander, placement)
    tkwm.title(.commander, "R Commander")
    tkwm.protocol(.commander, "WM_DELETE_WINDOW", closeCommander)
    topMenu <- tkmenu(.commander)
    tkconfigure(.commander, menu=topMenu)
    .commander.done <<- tclVar("0") # to address problem in Debian Linux
    source.files <- list.files(etc, pattern="*.R$")
    for (file in source.files) {
        source(file.path(etc, file))
        cat(paste("Sourced:", file, "\n"))
        }
    Menus <- read.table(file.path(etc, "Rcmdr-menus.txt"), as.is=TRUE)
    for (m in 1:nrow(Menus)){
        if (Menus[m, 1] == "menu") assign(Menus[m, 2], tkmenu(eval(parse(text=Menus[m, 3])), tearoff=FALSE)) 
        else if (Menus[m, 1] == "item") {
            if (Menus[m, 3] == "command")
                tkadd(eval(parse(text=Menus[m, 2])),"command", label=Menus[m, 4], command=eval(parse(text=Menus[m, 5])))
            else if (Menus[m, 3] == "cascade")
                tkadd(eval(parse(text=Menus[m, 2])),"cascade", label=Menus[m, 4], menu=eval(parse(text=Menus[m, 5])))
            else stop(paste("menu defintion error:", Menus[m, ], collapse=" "))
            }
        else stop(paste("menu defintion error:", Menus[m, ], collapse=" "))
        }
    exceptions <- scan(file.path(etc, "log-exceptions.txt"), what="", quiet=TRUE, comment.char="#")
    onEdit <- function(){
        command <- paste("fix(", .activeDataSet, ")", sep="")
        logger(command)
        justDoIt(command)
        activeDataSet(.activeDataSet)
        tkwm.deiconify(.commander)
        tkfocus(.commander)
        }
    onView <- function(){
        command <- paste("showData(", .activeDataSet, ", placement='-20+20')", sep="")
        logger(command)
        justDoIt(command)
        tkwm.deiconify(.commander)
        tkfocus(.commander)
        }
    onSubmit <- function(){
        selection <- strsplit(tclvalue(tktag.ranges(.log, "sel")), " ")[[1]]
        if (is.na(selection[1])) {
            tktag.add(.log, "currentLine", "insert linestart", "insert lineend")
            selection <- strsplit(tclvalue(tktag.ranges(.log, "currentLine")), " ")[[1]]
            tktag.delete(.log, "currentLine")
            if (is.na(selection[1])) {
                tkmessageBox(message=paste("Nothing is selected."),
                    icon="error", type="ok")
                tkfocus(.commander)
                return()
                }
            }
        lines <- tclvalue(tkget(.log, selection[1], selection[2]))
        lines <- strsplit(lines, "\n")[[1]]
        for (line in lines){
            cat(paste("\nR-cmdr>", line, "\n"))
            if (length(grep("<-", line)) > 0){
                var.value <- strsplit(line, "<-")[[1]]
                var <- gsub(" ", "", var.value[1])
                value <- var.value[2]
                if ( (length(grep("\\$", var)) > 0) || (length(grep("\\[", var)) > 0) 
                    || length(grep("\\(", var) > 0))
#                    || length(grep("row.names\\(", var) > 0) 
#                    || length(grep("rownames\\(", var) > 0)
#                    || length(grep("colnames\\(", var) > 0))
#                    justDoIt(paste(var, "<<-", value))
                    justDoIt(paste(var, "<-", value))
                else assign(var, justDoIt(value), envir=.GlobalEnv)
                }
            else if (length(grep("^remove\\(", line)) > 0){
                line <- sub(")", ", envir=.GlobalEnv)", line)
                eval(parse(text=line), envir=.GlobalEnv)
                }
            else if (length(grep("^hist\\(", line)) > 0){ 
                eval(parse(text=paste("plot(", line, ")", sep="")),envir=.GlobalEnv)
                }
            else if (any(sapply(exceptions, 
                    function(.x) length(grep(paste("^", .x, "\\(", sep=""), line)) > 0))){ 
                justDoIt(line)
                }
            else {
                result <- eval(parse(text=line), envir=.GlobalEnv)
                if (!is.null(result)) print(result)
                }
            }
        }
    contextMenu <- function(){
        contextMenu <- tkmenu(tkmenu(.log), tearoff=FALSE)
        tkadd(contextMenu, "command", label="Submit", command=onSubmit)
        tkadd(contextMenu, "command", label="Cut", command=onCut)
        tkadd(contextMenu, "command", label="Copy", command=onCopy)
        tkadd(contextMenu, "command", label="Paste", command=onPaste)
        tkadd(contextMenu, "command", label="Detele", command=onDelete)
        tkadd(contextMenu, "command", label="Find...", command=onFind)
        tkadd(contextMenu, "command", label="Select all", command=onSelectAll)
        tkpopup(contextMenu, tkwinfo("pointerx", .log), tkwinfo("pointery", .log))
        }    
    controlsFrame <- tkframe(.commander)
    editButton <- tkbutton(controlsFrame, text="Edit data set", command=onEdit)
    viewButton <- tkbutton(controlsFrame, text="View data set", command=onView)
    submitButton <- tkbutton(.commander, bitmap=paste("@", file.path(etc, "submit.xbm"), sep=""), 
        borderwidth="2", command=onSubmit)
    assign(".logCommands", tclVar("1"), envir=.GlobalEnv)
    logCheckBox <- tkcheckbutton(controlsFrame, variable=.logCommands)
    assign(".attachDataSet", tclVar("1"), envir=.GlobalEnv)
    attachCheckBox <- tkcheckbutton(controlsFrame, variable=.attachDataSet)
    assign(".dataSetName", tclVar("<No active dataset>"), envir=.GlobalEnv)
    assign(".dataSetLabel", tkbutton(controlsFrame, textvariable=.dataSetName, fg="red",
        relief="groove", command=selectActiveDataSet),
        envir=.GlobalEnv)
    logFrame <- tkframe(.commander)
    assign(".log", tktext(logFrame, bg="white", font=.logFont, 
        height=log.height, width=log.width, wrap="none"),  envir=.GlobalEnv)
    logXscroll <- tkscrollbar(logFrame, repeatinterval=5, orient="horizontal",
        command=function(...) tkxview(.log, ...))
    logYscroll <- tkscrollbar(logFrame, repeatinterval=5,
        command=function(...) tkyview(.log, ...))
    tkconfigure(.log, xscrollcommand=function(...) tkset(logXscroll, ...))
    tkconfigure(.log, yscrollcommand=function(...) tkset(logYscroll, ...))
    assign(".modelName", tclVar("<No active model>"), envir=.GlobalEnv)
    bottomLeftFrame <- tkframe(.commander)
    assign(".modelLabel", tkbutton(bottomLeftFrame, textvariable=.modelName, fg="red",
        relief="groove", command=selectActiveModel), 
        envir=.GlobalEnv)
    show.edit.button <- options("Rcmdr")[[1]]$show.edit.button
    show.edit.button <- if (is.null(show.edit.button)) TRUE else show.edit.button
    tkgrid(tklabel(controlsFrame, bitmap=paste("@", file.path(etc, "Rcmdr.xbm"), sep=""), fg="red"), 
        tklabel(controlsFrame, text="Data set:"), .dataSetLabel, 
        tklabel(controlsFrame, text="  "), if(show.edit.button) editButton, viewButton, 
        tklabel(controlsFrame, text="  Log commands:"), logCheckBox, 
        tklabel(controlsFrame, text="  Attach active data set:"), attachCheckBox, sticky="w")
    tkgrid(controlsFrame, sticky="w", columnspan="2")
    tkgrid(.log, logYscroll)
    tkgrid(logXscroll)
    tkgrid(logFrame, columnspan="2")
    tkgrid(tklabel(bottomLeftFrame, text="Model: "), .modelLabel)
    tkgrid(bottomLeftFrame, submitButton)
    tkgrid.configure(logYscroll, sticky="ns")
    tkgrid.configure(logXscroll, sticky="ew")
    tkgrid.configure(submitButton, sticky="e")
    tkgrid.configure(bottomLeftFrame, sticky="w")
    for (row in 0:4) tkgrid.rowconfigure(.commander, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(.commander, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(.commander, 0, 0)
    tkbind(.commander, "<Control-r>", onSubmit)
    tkbind(.commander, "<Control-R>", onSubmit)
    tkbind(.commander, "<Control-f>", onFind)
    tkbind(.commander, "<Control-F>", onFind)
    tkbind(.log, "<ButtonPress-3>", contextMenu)
    tkwm.deiconify(.commander)
    tkfocus(.commander)
    tkwait <- options("Rcmdr")[[1]]$tkwait  # to address problem in Debian Linux
    if ((!is.null(tkwait)) && tkwait) tkwait.variable(.commander.done)
    }

logger <- function(command){
    if (tclvalue(.logCommands) == "1") {
        tkinsert(.log, "end", paste(command,"\n", sep=""))
        tkyview.moveto(.log, 1)
        }
    lines <- strsplit(command, "\n")[[1]]
    for (line in lines) cat(paste("\nR-cmdr>", line, "\n"))
    command
    }

doItAndPrint <- function(command) {
    result <- try(eval(parse(text=logger(command)), envir=.GlobalEnv), 
        silent=TRUE)
    if (class(result)[1] ==  "try-error"){
        tkmessageBox(message=paste("Error:",
            strsplit(result, ":")[[1]][2]), icon="error")
        tkfocus(.commander)
        return()
        }
    if (!is.null(result)) print(result)
    }

justDoIt <- function(command) {
    result <- try(eval(parse(text=command), envir=.GlobalEnv), silent=TRUE)
    if (class(result)[1] ==  "try-error"){
        tkmessageBox(message=paste("Error:",
            strsplit(result, ":")[[1]][2]), icon="error")
        tkfocus(.commander)
        return()
        }
    result
    }
    
# last modified 20 Mar 2004 by J. Fox

# Data menu dialogs

newDataSet <- function() {
    checkReplace <- function(name){
        tkmessageBox(message=paste("Data set", name, "already exists.\nOverwrite data set?"),
            icon="warning", type="yesno", default="no")
        }
    top <- tktoplevel()
    tkwm.title(top, "New Data Set")
    dsname <- tclVar("Dataset")
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
    onOK <- function(){
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == ""){
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkmessageBox(message=paste("You must enter the name of a data set."),
                icon="error", type="ok", default="ok")
                newDataSet()
                return()
                }     
        if (!is.valid.name(dsnameValue)){
            tkmessageBox(message=paste('"', dsnameValue, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            newDataSet()
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                newDataSet()
                return()
                }
            }
        command <- "edit(as.data.frame(NULL))"
        assign(dsnameValue, justDoIt(command), envir=.GlobalEnv)
        logger(paste(dsnameValue, " <- ", command, sep=""))
        activeDataSet(dsnameValue)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12" ,command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(edit.data.frame)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="e")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
    tkgrid.configure(entryDsname, sticky="w")
    tkgrid.configure(helpButton, sticky="e")    
    for (row in 0:1) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(entryDsname, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(entryDsname)
    tkwait.window(top)
    }

selectActiveDataSet <- function(){
    dataSets <- listDataSets()
    if (length(dataSets) == 0){
        tkmessageBox(message="There are no data sets from which to choose.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Select Data Set")
    dataSetsFrame <- tkframe(top)
    dataSetsBox <- tklistbox(dataSetsFrame, height=min(4, length(dataSets)),
        selectmode="single", background="white")
    dataSetsScroll <- tkscrollbar(dataSetsFrame, repeatinterval=5, 
        command=function(...) tkyview(dataSetsBox, ...))
    tkconfigure(dataSetsBox, yscrollcommand=function(...) tkset(dataSetsScroll, ...))
    for (ds in dataSets) tkinsert(dataSetsBox, "end", ds)
    tkselection.set(dataSetsBox, if (is.null(.activeDataSet)) 0 else which(.activeDataSet == dataSets) - 1)
    onOK <- function(){
        activeDataSet(dataSets[as.numeric(tkcurselection(dataSetsBox)) + 1])
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }  
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(attach)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Data Sets (pick one)"), sticky="w")
    tkgrid(dataSetsBox, dataSetsScroll, sticky="nw")
    tkgrid(dataSetsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, tklabel(top, text="    "), helpButton, sticky="w")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkgrid.configure(dataSetsScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkbind(dataSetsBox, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
    
listDataSetsInPackages <- function() doItAndPrint("data()")

Recode <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
    processRecode <- function(recode){
        parts <- strsplit(recode, "=")[[1]]
        if (length(grep(",", parts[1])) > 0) paste("c(", parts[1], ") = ", parts[2], sep="")
            else paste(parts, collapse="=")
        }
    dataSet <- activeDataSet()
    if (dataSet == FALSE) {
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Recode Variable")
    variablesFrame <- tkframe(top)
    variablesListFrame <- tkframe(variablesFrame)
    variablesBox <- tklistbox(variablesListFrame, height=min(4, length(.variables)),
        selectmode="single", background="white", exportselection="FALSE")
    variablesScroll <- tkscrollbar(variablesListFrame, repeatinterval=5, 
        command=function(...) tkyview(variablesBox, ...))
    tkconfigure(variablesBox, yscrollcommand=function(...) tkset(variablesScroll, ...))
    for (variable in .variables) tkinsert(variablesBox, "end", variable)
    tkselection.set(variablesBox, 0)
    newVariableName <- tclVar("variable")
    newVariable <- tkentry(variablesFrame, width="20", textvariable=newVariableName)
    recodesFrame <- tkframe(top)
    recodes <- tktext(recodesFrame, bg="white", font=tkfont.create(family="courier", size=10), 
        height="5", width="40", wrap="none")
    recodesXscroll <- tkscrollbar(recodesFrame, repeatinterval=5, orient="horizontal",
        command=function(...) tkxview(recodes, ...))
    recodesYscroll <- tkscrollbar(recodesFrame, repeatinterval=5,
        command=function(...) tkyview(recodes, ...))
    tkconfigure(recodes, xscrollcommand=function(...) tkset(recodesXscroll, ...))
    tkconfigure(recodes, yscrollcommand=function(...) tkset(recodesYscroll, ...))
    asFactorFrame <- tkframe(top)
    asFactorVariable <- tclVar("1")
    asFactorCheckBox <- tkcheckbutton(asFactorFrame, variable=asFactorVariable)
    onOK <- function(){
        variable <- as.character(tkget(variablesBox, "active"))
        newVar <- trim.blanks(tclvalue(newVariableName))
        if (!is.valid.name(newVar)){
            tkmessageBox(message=paste('"', newVar, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            Recode()
            return()
            }
        asFactor <- tclvalue(asFactorVariable) == "1"
        recode.directives <- gsub("\n", "; ", tclvalue(tkget(recodes, "1.0", "end")))
        check.empty <- gsub(";", "", gsub(" ", "", recode.directives))
        if ("" == check.empty) {
            tkmessageBox(message="No recode directives specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            Recode()
            return()
            }
        if (0 != length(grep("'", recode.directives))) {
            tkmessageBox(message='Use only double-quotes (" ") in recode directives', 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            Recode()
            return()
            }
        if (is.element(newVar, .variables)) {
            if ("no" == tclvalue(checkReplace(newVar))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                Recode()
                return()
                }
            }
        recode.directives <- strsplit(recode.directives, ";")[[1]]
        recode.directives <- paste(sapply(recode.directives, processRecode), collapse=";") 
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        cmd <- paste("recode(", dataSet,"$",variable, ", '", recode.directives, 
            "', as.factor.result=", asFactor, ")", sep="")
        logger(paste(dataSet,"$",newVar, " <- ", cmd, sep=""))
#        justDoIt(paste(dataSet,"$",newVar, " <<- ", cmd, sep=""))
        justDoIt(paste(dataSet,"$",newVar, " <- ", cmd, sep=""))
        activeDataSet(dataSet)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    OKCancelFrame <- tkframe(top)
    OKbutton <- tkbutton(OKCancelFrame, text="OK", fg="darkgreen", width="12",command=onOK)
    cancelButton <- tkbutton(OKCancelFrame, text="Cancel", fg="red", width="12",command=onCancel)    
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(Recode)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)       
    tkgrid(tklabel(top, text="Variable to recode (pick one)"), 
        tklabel(top, text="Recode directives"), sticky="w")
    tkgrid(variablesBox, variablesScroll, sticky="nw")
    tkgrid(variablesListFrame, sticky="nw")
    tkgrid(tklabel(variablesFrame, text="New variable name"), sticky="w")
    tkgrid(newVariable, sticky="w")
    tkgrid(recodes, recodesYscroll, sticky="nw")
    tkgrid(recodesXscroll)
    tkgrid(variablesFrame, recodesFrame, sticky="nw")
    tkgrid(tklabel(asFactorFrame, text="Make new variable a factor"), asFactorCheckBox, 
        sticky="w")
    tkgrid(asFactorFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w") 
    tkgrid(OKCancelFrame, helpButton, sticky="w")
    tkgrid.configure(variablesScroll, sticky="ns")
    tkgrid.configure(recodesXscroll, sticky="ew")
    tkgrid.configure(recodesYscroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(recodes)
    tkwait.window(top)             
    }

Compute <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
    dataSet <- activeDataSet()
    if (dataSet == FALSE) {
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Compute New Variable")
    variablesFrame <- tkframe(top)
    variablesListFrame <- tkframe(variablesFrame)
    variablesBox <- tklistbox(variablesListFrame, height=min(4, length(.variables)),
        selectmode="browse", background="white")
    variablesScroll <- tkscrollbar(variablesListFrame, repeatinterval=5, 
        command=function(...) tkyview(variablesBox, ...))
    tkconfigure(variablesBox, yscrollcommand=function(...) tkset(variablesScroll, ...))
    for (variable in .variables) tkinsert(variablesBox, "end", variable)
    newVariableName <- tclVar("variable")
    newVariable <- tkentry(variablesFrame, width="20", textvariable=newVariableName)
    computeFrame <- tkframe(top)
    computeVar <- tclVar("")
    compute <- tkentry(computeFrame, font=.logFont, width="30", textvariable=computeVar)
    computeXscroll <- tkscrollbar(computeFrame, repeatinterval=10,
        orient="horizontal", command=function(...) tkxview(compute, ...))
    tkconfigure(compute, xscrollcommand=function(...) tkset(computeXscroll, ...))
    onOK <- function(){
        newVar <- trim.blanks(tclvalue(newVariableName))
        if (!is.valid.name(newVar)){
            tkmessageBox(message=paste('"', newVar, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            Compute()
            return()
            }
        express <- tclvalue(computeVar)
        check.empty <- gsub(";", "", gsub(" ", "", express))
        if ("" == check.empty) {
            tkmessageBox(message="No expression specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            Compute()
            return()
            }
        if (is.element(newVar, .variables)) {
            if ("no" == tclvalue(checkReplace(newVar))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                Compute()
                return()
                }
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        logger(paste(dataSet,"$",newVar, " <- ", express, sep=""))
#        justDoIt(paste(dataSet,"$",newVar, " <<- with(", .activeDataSet,
        justDoIt(paste(dataSet,"$",newVar, " <- with(", .activeDataSet,
            " ,", express, ")"))
        activeDataSet(dataSet)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    OKCancelFrame <- tkframe(top)
    OKbutton <- tkbutton(OKCancelFrame, text="OK", fg="darkgreen", width="12", command=onOK)
    cancelButton <- tkbutton(OKCancelFrame, text="Cancel", fg="red", width="12",command=onCancel)  
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help("Compute")
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)         
    tkgrid(tklabel(variablesFrame, text="Current variables (list only)"), sticky="w")
    tkgrid(variablesBox, variablesScroll, sticky="w")
    tkgrid(variablesListFrame, sticky="w")
    tkgrid(tklabel(variablesFrame, text="New variable name"), sticky="w")
    tkgrid(newVariable, sticky="w")
    tkgrid(tklabel(computeFrame, text="Expression to compute"), sticky="w")
    tkgrid(compute, sticky="w")
    tkgrid(computeXscroll, sticky="ew")
    tkgrid(variablesFrame, computeFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, sticky="w") 
    tkgrid(OKCancelFrame, helpButton, sticky="w")
    tkgrid.configure(variablesScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e") 
    for (row in 0:1) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)   
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(compute)
    tkwait.window(top)
    }

deleteVariable <- function(){
    dataSet <- activeDataSet()
    if (dataSet == FALSE) {
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Delete Variables")
    variablesListFrame <- tkframe(top)
    variablesBox <- tklistbox(variablesListFrame, height=min(4, length(.variables)),
        selectmode="multiple", background="white")
    variablesScroll <- tkscrollbar(variablesListFrame, repeatinterval=5, 
        command=function(...) tkyview(variablesBox, ...))
    tkconfigure(variablesBox, yscrollcommand=function(...) tkset(variablesScroll, ...))
    for (variable in .variables) tkinsert(variablesBox, "end", variable)
    onOK <- function(){
        variables <- .variables[as.numeric(tkcurselection(variablesBox)) + 1]
        if (length(variables) == 1){
            response <- tclvalue(tkmessageBox(message=paste("Delete ", variables,
                "?\nPlease confirm.", sep=""), icon="warning", type="okcancel", default="cancel"))
            if (response == "cancel") {
                onCancel()
                return()
                }
            }
        else{
            response <- tclvalue(tkmessageBox(message=paste("Delete ", length(variables),
                " variables?\nPlease confirm.", sep=""), 
                icon="warning", type="okcancel", default="cancel"))
            if (response == "cancel") {
                onCancel()
                return()
                }
            }  
        for (variable in variables){              
#            eval(parse(text=paste(dataSet, "$", variable, "<<- NULL", sep="")), envir=.GlobalEnv)
            eval(parse(text=paste(dataSet, "$", variable, "<- NULL", sep="")), envir=.GlobalEnv)
            logger(paste(dataSet, "$", variable, " <- NULL", sep=""))
            }
        activeDataSet(dataSet)
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12",command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)    
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help("NULL")
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)       
    tkgrid(tklabel(top, text="Variable(s) to delete (pick one or more)"), sticky="w")
    tkgrid(variablesBox, variablesScroll, sticky="nw")
    tkgrid(variablesListFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(variablesScroll, sticky="ns")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)   
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

readDataSet <- function() {
    checkReplace <- function(name){
        tkmessageBox(message=paste("Data set", name, "already exists.\nOverwrite data set?"),
            icon="warning", type="yesno", default="no")
        }
    top <- tktoplevel()
    tkwm.title(top, "Read Data From Text File")
    optionsFrame <- tkframe(top)
    dsname <- tclVar("Dataset")
    entryDsname <- tkentry(optionsFrame, width="20", textvariable=dsname)
    headerVariable <- tclVar("1")
    headerCheckBox <- tkcheckbutton(optionsFrame, variable=headerVariable)
    delimiterFrame <- tkframe(optionsFrame)
    delimiterVariable <- tclVar("whitespace")
    whitespaceButton <- tkradiobutton(delimiterFrame, variable=delimiterVariable, value="whitespace")
    commasButton <- tkradiobutton(delimiterFrame, variable=delimiterVariable, value="commas")
    otherButton <- tkradiobutton(delimiterFrame, variable=delimiterVariable, value="other")
    otherVariable <- tclVar("")
    otherEntry <- tkentry(delimiterFrame, width="4", textvariable=otherVariable) 
    decimalFrame <- tkframe(optionsFrame)
    decimalVariable <- tclVar("period")
    periodButton <- tkradiobutton(decimalFrame, variable=decimalVariable, value="period")
    commaButton <- tkradiobutton(decimalFrame, variable=decimalVariable, value="comma")  
    missingVariable <- tclVar("NA")
    missingEntry <- tkentry(optionsFrame, width="8", textvariable=missingVariable)    
    onOK <- function(){
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == ""){
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkmessageBox(message=paste("You must enter a name for the data set."),
                icon="error", type="ok", default="ok")
                readDataSet()
                return()
                }
        if (!is.valid.name(dsnameValue)){
            tkmessageBox(message=paste('"', dsnameValue, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            readDataSet()
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                readDataSet()
                return()
                }
            }
        file <- tclvalue(tkgetOpenFile(filetypes='{"Text Files" {".txt" ".TXT" ".dat" ".DAT"}} {"All Files" {"*"}}'))
        if (file == "") {
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            return()
            }
        head <- tclvalue(headerVariable) == "1"
        delimiter <- tclvalue(delimiterVariable)
        del <- if (delimiter == "whitespace") ""
            else if (delimiter == "commas") ","
            else tclvalue(otherVariable)
        miss <- tclvalue(missingVariable)
        dec <- if (tclvalue(decimalVariable) == "period") "." else ","
        command <- paste('read.table("', file,'", header=', head, 
            ', sep="', del, '", na.strings="', miss, '", dec="', dec, '", strip.white=TRUE)', sep="")
        logger(paste(dsnameValue, " <- ", command, sep=""))
        assign(dsnameValue, justDoIt(command), envir=.GlobalEnv)
        activeDataSet(dsnameValue)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }    
    buttonsFrame <- tkframe(top)  
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", default="active", command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(read.table)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(optionsFrame, text="Enter name for data set:"), entryDsname, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Variable names in file:"), headerCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Missing data indicator:"), missingEntry, sticky="w")
    tkgrid(tklabel(delimiterFrame, text="Field Separator", fg="blue"), sticky="w")
    tkgrid(tklabel(delimiterFrame, text="White space"), whitespaceButton, sticky="w")
    tkgrid(tklabel(delimiterFrame, text="Commas"), commasButton, sticky="w")
    tkgrid(tklabel(delimiterFrame, text="Other"), otherButton, 
        tklabel(delimiterFrame, text="  Specify:"), otherEntry, sticky="w")
    tkgrid(delimiterFrame, sticky="w", columnspan=2)
    tkgrid(tklabel(decimalFrame, text="Decimal-Point Character", fg="blue"), sticky="w", columnspan=2)
    tkgrid(tklabel(decimalFrame, text="Period [.]"), periodButton, sticky="w")
    tkgrid(tklabel(decimalFrame, text="Comma [,]"), commaButton, sticky="w")
    tkgrid(decimalFrame, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(helpButton, sticky="e")   
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK) 
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(entryDsname)
    tkwait.window(top)
    }
    
readDataFromPackage <- function() {
    checkReplace <- function(name){
        tkmessageBox(message=paste("Data set", name, "already exists.\nOverwrite data set?"),
            icon="warning", type="yesno", default="no")
        }
    top <- tktoplevel()
    tkwm.title(top, "Read Data From Package")
    dsname <- tclVar("")
    enterFrame <- tkframe(top)
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
    packages <- .packages()
    packagesFrame <- tkframe(top)
    packagesBox <- tklistbox(packagesFrame, height=min(4, length(packages)),
        selectmode="single", background="white", exportselection="FALSE")
    packagesScroll <- tkscrollbar(packagesFrame, repeatinterval=5, 
        command=function(...) tkyview(packagesBox, ...))
    tkconfigure(packagesBox, yscrollcommand=function(...) tkset(packagesScroll, ...))
    for (pack in packages) tkinsert(packagesBox, "end", pack)
    onOK <- function(){
        dsnameValue <- tclvalue(dsname)
        if (dsnameValue != ""){
            if (is.element(dsnameValue, listDataSets())) {
                if ("no" == tclvalue(checkReplace(dsnameValue))){
                    if (.grab.focus) tkgrab.release(top)
                    tkdestroy(top)
                    readDataFromPackage()
                    return()
                    }
                }
            save.options <- options(warn=2)
            check <- try(eval(parse(text=logger(paste("data(", dsnameValue, ")", sep=""))),
                envir=.GlobalEnv), silent=TRUE)
            options(save.options)
            if (class(check) == "try-error"){
                tkmessageBox(message=paste("Data set", dsnameValue, "does not exist."),
                    icon="error", type="ok")
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                readDataFromPackage()
                return()
                }
            activeDataSet(dsnameValue)
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkfocus(.commander)
            }
        else{
            packageName <- as.character(tkget(packagesBox, "active")) 
            save.options <- options(warn=-1)
            dataSets <- data(package=parse(text=packageName))$results[,3] 
            options(save.options)
            if (length(dataSets) == 0){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                tkmessageBox(message=paste("There are no data sets in package", packageName),
                    icon="error", type="ok", default="ok")
                    readDataFromPackage()
                    return()
                }
            subdialog <- tktoplevel()
            tkwm.title(subdialog, "Select Data Set")
            dsFrame <- tkframe(subdialog)
            dsBox <- tklistbox(dsFrame, height=min(4, length(dataSets)),
                selectmode="single", background="white", exportselection="FALSE")
            dsScroll <- tkscrollbar(dsFrame, repeatinterval=5, 
                command=function(...) tkyview(dsBox, ...))
            tkconfigure(dsBox, yscrollcommand=function(...) tkset(dsScroll, ...))
            for (ds in dataSets) tkinsert(dsBox, "end", ds)
            onOKsub <- function() {
                dsnameValue <- as.character(tkget(dsBox, "active"))
                if (is.element(dsnameValue, listDataSets())) {
                    if ("no" == tclvalue(checkReplace(dsnameValue))){
                        if (.grab.focus) tkgrab.release(subdialog)
                        tkdestroy(subdialog)
                        tkdestroy(top)
                        readDataFromPackage()
                        return()
                        }
                    }
                command <- paste("data(", dsnameValue, ', package="', packageName, '")', sep="")
                justDoIt(command)
                logger(command)
                activeDataSet(dsnameValue)                
                if (.grab.focus) tkgrab.release(subdialog)
                tkfocus(.commander)
                tkdestroy(subdialog)
                }
            onCancelSub <- function() {
                if (.grab.focus) tkgrab.release(subdialog)  
                tkfocus(.commander)
                tkdestroy(subdialog)
                }
            subButtonFrame <- tkframe(subdialog)
            OKSubButton <- tkbutton(subButtonFrame, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
            cancelSubButton <- tkbutton(subButtonFrame, text="Cancel", fg="red", width="12",command=onCancelSub)
            labelFrame <- tkframe(subdialog)
            tkgrid(dsBox, dsScroll, sticky="nw")
            tkgrid(tklabel(labelFrame, text="Package: "), tklabel(labelFrame, text=packageName, fg="red"),
                sticky="w")
            tkgrid(labelFrame, sticky="w")
            tkgrid(tklabel(subdialog, text="Select data set"), sticky="w")
            tkgrid(dsFrame, sticky="w")
            tkgrid(OKSubButton, cancelSubButton, sticky="w")
            tkgrid(subButtonFrame, sticky="w")
            tkgrid.configure(dsScroll, sticky="ns")
            for (row in 0:3) tkgrid.rowconfigure(subdialog, row, weight=0)
            for (col in 0:1) tkgrid.columnconfigure(subdialog, col, weight=0)
            .Tcl("update idletasks")
            tkwm.resizable(subdialog, 0, 0)
            tkbind(subdialog, "<Return>", onOKsub)
            if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOKsub)
            tkbind(dsBox, "<Double-ButtonPress-1>", onOKsub)
            tkselection.set(dsBox, 0)
            if (.grab.focus) tkgrab.release(top)
            tkwm.deiconify(subdialog)
            if (.grab.focus) tkgrab.set(subdialog)
            tkfocus(subdialog)
            tkwait.window(subdialog)
            tkdestroy(top)           
            }
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        } 
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", default="active", command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(data)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(packagesBox, packagesScroll, sticky="nw")
    tkgrid(tklabel(top, text="Enter name of data set:"), entryDsname, sticky="w")
    tkgrid(tklabel(top, text="OR", fg="red"), sticky="w")
    tkgrid(tklabel(top, text="Select package:"), packagesFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
    tkgrid.configure(packagesScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(entryDsname, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkbind(packagesBox, "<Double-ButtonPress-1>", onOK)
    tkselection.set(packagesBox, 0)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(entryDsname)
    tkwait.window(top)
    }

importSPSS <- function() {
    checkReplace <- function(name){
        tkmessageBox(message=paste("Data set", name, "already exists.\nOverwrite data set?"),
            icon="warning", type="yesno", default="no")
        }
    top <- tktoplevel()
    tkwm.title(top, "Import SPSS Data Set")
    dsname <- tclVar("Dataset")
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
    asFactor <- tclVar("1")
    asFactorCheckBox <- tkcheckbutton(top, variable=asFactor)
    maxLevels <- tclVar("Inf")
    entryMaxLevels <- tkentry(top, width="5", textvariable=maxLevels)
    onOK <- function(){
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == ""){
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkmessageBox(message=paste("You must enter the name of a data set."),
                icon="error", type="ok", default="ok")
                importSPSS()
                return()
                }
        if (!is.valid.name(dsnameValue)){
            tkmessageBox(message=paste('"', dsnameValue, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            importSPSS()
            return()
            }
                     
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                importSPSS()
                return()
                }
            }
        file <- tclvalue(tkgetOpenFile(
            filetypes='{"SPSS save files" {".sav" ".SAV"}} {"SPSS portable files" {".por" ".POR"}} {"All Files" {"*"}}'))
        if (file == "") {
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            return()
            }
        factor <- tclvalue(asFactor) == "1"
        levels <- as.numeric(tclvalue(maxLevels))
        command <- paste('read.spss("', file,'", use.value.labels=', factor,
            ", max.value.labels=", levels, ", to.data.frame=TRUE)", sep="")
        logger(paste(dsnameValue, " <- ", command, sep=""))
        assign(dsnameValue, justDoIt(command), envir=.GlobalEnv)
        activeDataSet(dsnameValue)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", default="active", command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(read.spss)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="w")
    tkgrid(tklabel(top, text="Convert value labels\nto factor levels", justify="left"), 
        asFactorCheckBox, sticky="w")
    tkgrid(tklabel(top, text="Maximum number\nof value labels\nfor factor conversion", justify="left"), 
        entryMaxLevels, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(entryDsname, sticky="w")
    tkgrid.configure(asFactorCheckBox, sticky="w")
    tkgrid.configure(entryMaxLevels, sticky="w")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(entryDsname, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(entryDsname)
    tkwait.window(top)
    }

importMinitab <- function() {
    checkReplace <- function(name){
        tkmessageBox(message=paste("Data set", name, "already exists.\nOverwrite data set?"),
            icon="warning", type="yesno", default="no")
        }
    top <- tktoplevel()
    tkwm.title(top, "Import Minitab Data Set")
    dsname <- tclVar("Dataset")
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
    onOK <- function(){
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == ""){
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkmessageBox(message=paste("You must enter the name of a data set."),
                icon="error", type="ok", default="ok")
                importMinitab()
                return()
                }     
        if (!is.valid.name(dsnameValue)){
            tkmessageBox(message=paste('"', dsnameValue, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            importMinitab()
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                importMinitab()
                return()
                }
            }
        file <- tclvalue(tkgetOpenFile(
            filetypes='{"Minitab portable files" {".mtp" ".MTP"}} {"All Files" {"*"}}'))
        if (file == "") {
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            return()
            }
        command <- paste('read.mtp("', file,'")', sep="")
        datalist <- justDoIt(command)
        lengths <- sapply(datalist, length)
        datalist <- datalist[lengths != 0]
        lengths <- lengths[lengths != 0]
        if (!all(lengths == length(datalist[[1]]))){
            tkmessageBox(message=
                paste("Minitab data set contains elements of unequal length.\nData set cannot be converted."),
                icon="error", type="ok")
            tkdestroy(top)
            tkfocus(.commander)
            return()
            }
        assign(dsnameValue, as.data.frame(datalist), envir=.GlobalEnv)
        logger(paste(dsnameValue, " <- as.data.frame(", command, ")", sep=""))
        activeDataSet(dsnameValue)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12" ,command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(read.mtp)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="e")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
    tkgrid.configure(entryDsname, sticky="w")
    tkgrid.configure(helpButton, sticky="e")    
    for (row in 0:1) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(entryDsname, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(entryDsname)
    tkwait.window(top)
    }

numericToFactor <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Convert Numeric Variable to Factor")
    variableFrame <- tkframe(top)
    variableBox <- tklistbox(variableFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    variableScroll <- tkscrollbar(variableFrame, repeatinterval=5, 
        command=function(...) tkyview(variableBox, ...))
    tkconfigure(variableBox, yscrollcommand=function(...) tkset(variableScroll, ...))
    for (var in .numeric) tkinsert(variableBox, "end", var)
    levelsFrame <- tkframe(top)
    levelsVariable <- tclVar("names")
    numbersButton <- tkradiobutton(levelsFrame, variable=levelsVariable, value="numbers")
    namesButton <- tkradiobutton(levelsFrame, variable=levelsVariable, value="names")
    factorName <- tclVar("<same as variable>")
    factorNameField <- tkentry(top, width="20", textvariable=factorName)
    onOK <- function(){
        variable <- as.character(tkget(variableBox, "active"))
        name <- trim.blanks(tclvalue(factorName))
        if (name == "<same as variable>") name <- variable
        if (!is.valid.name(name)){
            tkmessageBox(message=paste('"', name, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            numericToFactor()
            return()
            }
        if (is.element(name, .variables)) {
            if ("no" == tclvalue(checkReplace(name))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                numericToFactor()
                return()
                }
            }
        levelsType <- tclvalue(levelsVariable)
        if (levelsType == "names"){
            values <- sort(unique(eval(parse(text=paste(.activeDataSet, "$", variable, sep="")),
                envir=.GlobalEnv)))
            nvalues <- length(values)
            if (nvalues > 30) {
                tkmessageBox(message=paste("Number of levels (", nvalues, ") too large.", sep=""),
                    icon="error", type="ok")
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                numericToFactor()
                return()
                }
            subdialog <- tktoplevel()
            tkwm.title(subdialog, "Level Names")
            names <- rep("", nvalues)
            onOKsub <- function() {
                for (i in 1:nvalues){
                    names[i] <- eval(parse(text=paste("tclvalue(levelName", i, ")", sep="")))
                    }
                if (length(unique(names)) != nvalues){
                    tkmessageBox(message="Levels names are not unique.",
                        icon="error", type="ok")
                    if (.grab.focus) tkgrab.release(subdialog)
                    tkdestroy(subdialog)
                    numericToFactor()
                    return()
                    }
                if (any(names == "")){
                    tkmessageBox(message="A level name is empty.",
                        icon="error", type="ok")
                    if (.grab.focus) tkgrab.release(subdialog)
                    tkdestroy(subdialog)
                    numericToFactor()
                    return()
                    }
                command <- paste("factor(", .activeDataSet, "$", variable,
                    ", labels=c(", paste(paste("'", names, "'", sep=""), collapse=","), "))", sep="")
#                justDoIt(paste(.activeDataSet, "$", name, " <<- ", command, sep=""))
                justDoIt(paste(.activeDataSet, "$", name, " <- ", command, sep=""))
                logger(paste(.activeDataSet,"$", name," <- ", command, sep=""))
                activeDataSet(.activeDataSet)
                if (.grab.focus) tkgrab.release(subdialog)
                tkfocus(.commander)
                tkdestroy(subdialog)
                }
            onCancelSub <- function() {
                if (.grab.focus) tkgrab.release(subdialog)  
                tkfocus(.commander)
                tkdestroy(subdialog)
                }
            OKSubButton <- tkbutton(subdialog, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
            cancelSubButton <- tkbutton(subdialog, text="Cancel", fg="red", width="12",command=onCancelSub)
            tkgrid(tklabel(subdialog, text="Numeric value"), tklabel(subdialog, text="Level name"), sticky="w")
            for (i in 1:nvalues){
                valVar <- paste("levelName", i, sep="")
                assign(valVar, tclVar(""))
                assign(paste("entry", i, sep=""), tkentry(subdialog, width="20", 
                    textvariable=eval(parse(text=valVar))))
                tkgrid(tklabel(subdialog, text=values[i]), eval(parse(text=paste("entry", i, sep=""))), sticky="w")
                }
            tkgrid(OKSubButton, cancelSubButton, sticky="w")
            for (row in 0:(nvalues + 1)) tkgrid.rowconfigure(subdialog, row, weight=0)
            for (col in 0:1) tkgrid.columnconfigure(subdialog, col, weight=0)
            .Tcl("update idletasks")
            tkwm.resizable(subdialog, 0, 0)
            tkbind(subdialog, "<Return>", onOKsub)
            if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOKsub)
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)           
            tkwm.deiconify(subdialog)
            if (.grab.focus) tkgrab.set(subdialog)
            tkfocus(entry1)
            tkwait.window(subdialog)
            }
        else{
            command <- paste("as.factor(", .activeDataSet, "$", variable, ")", sep="")
#            justDoIt(paste(.activeDataSet, "$", name, " <<- ", command, sep=""))
            justDoIt(paste(.activeDataSet, "$", name, " <- ", command, sep=""))
            logger(paste(.activeDataSet, "$", name," <- ", command, sep=""))
            activeDataSet(.activeDataSet)
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkfocus(.commander)
            }
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(factor)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable (pick one)"), 
        tklabel(top, text="Factor Levels", fg="blue"), sticky="w")
    tkgrid(variableBox, variableScroll, sticky="nw")
    tkgrid(tklabel(levelsFrame, text="Supply level names"), namesButton, sticky="w")
    tkgrid(tklabel(levelsFrame, text="Use numbers"), numbersButton, sticky="w")
    tkgrid(variableFrame, levelsFrame, sticky="nw")
    tkgrid(tklabel(top, text="Name for factor"), sticky="w")
    tkgrid(factorNameField, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(variableScroll, sticky="ns")
    tkgrid.configure(numbersButton, sticky="w")
    tkgrid.configure(namesButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(variableBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
#    if (.grab.focus) tkgrab.set(top)   # causes problems ?
    tkfocus(top)
    tkwait.window(top)
    }

reorderFactor <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.factors) == 0){
        tkmessageBox(message="There no factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Reorder Factor Levels")
    variableFrame <- tkframe(top)
    variableBox <- tklistbox(variableFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    variableScroll <- tkscrollbar(variableFrame, repeatinterval=5, 
        command=function(...) tkyview(variableBox, ...))
    tkconfigure(variableBox, yscrollcommand=function(...) tkset(variableScroll, ...))
    for (var in .factors) tkinsert(variableBox, "end", var)
    orderedFrame <- tkframe(top)
    orderedVariable <- tclVar("0")
    orderedCheckBox <- tkcheckbutton(orderedFrame, variable=orderedVariable)
    factorName <- tclVar("<same as original>")
    factorNameField <- tkentry(top, width="20", textvariable=factorName)
    onOK <- function(){
        variable <- as.character(tkget(variableBox, "active"))
        name <- trim.blanks(tclvalue(factorName))
        if (name == "<same as original>") name <- variable
        if (!is.valid.name(name)){
            tkmessageBox(message=paste('"', name, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            reorderFactor()
            return()
            }
        if (is.element(name, .variables)) {
            if ("no" == tclvalue(checkReplace(name))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                reorderFactor()
                return()
                }
            }
        old.levels <- eval(parse(text=paste("levels(", .activeDataSet, "$", variable, ")", 
            sep="")), envir=.GlobalEnv)
        nvalues <- length(old.levels)
        ordered <- tclvalue(orderedVariable)
        if (nvalues > 30) {
            tkmessageBox(message=paste("Number of levels (", nvalues, ") too large.", sep=""),
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            reorderFactor()
            return()
            }
        subdialog <- tktoplevel()
        tkwm.title(subdialog, "Reorder Levels")
        order <- 1:nvalues
        onOKsub <- function() {
            for (i in 1:nvalues){
                order[i] <- as.numeric(eval(parse(text=paste("tclvalue(levelOrder", i, ")", sep=""))))
                }
            if (any(sort(order) != 1:nvalues)){
                tkmessageBox(message=paste("Order of levels must include all integers from 1 to ",
                    nvalues, sep=""), icon="error", type="ok")
                if (.grab.focus) tkgrab.release(subdialog)
                tkdestroy(subdialog)
                reorderFactor()
                return()
                }
            levels <- old.levels[order(order)]
            ordered <- if (ordered == "1") ", ordered=TRUE" else ""
            command <- paste("factor(", .activeDataSet, "$", variable,
                ", levels=c(", paste(paste("'", levels, "'", sep=""), collapse=","), ")",
                ordered, ")", sep="")
#            justDoIt(paste(.activeDataSet, "$", name, " <<- ", command, sep=""))
            justDoIt(paste(.activeDataSet, "$", name, " <- ", command, sep=""))
            logger(paste(.activeDataSet,"$", name," <- ", command, sep=""))
            activeDataSet(.activeDataSet)
            if (.grab.focus) tkgrab.release(subdialog)
            tkdestroy(subdialog)
            }
        onCancelSub <- function() {
            if (.grab.focus) tkgrab.release(subdialog)  
            tkfocus(.commander)
            tkdestroy(subdialog)
            }
        OKSubButton <- tkbutton(subdialog, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
        cancelSubButton <- tkbutton(subdialog, text="Cancel", fg="red", width="12",command=onCancelSub)
        tkgrid(tklabel(subdialog, text="Old Levels", fg="blue"), 
            tklabel(subdialog, text="New order", fg="blue"), sticky="w")
        for (i in 1:nvalues){
            valVar <- paste("levelOrder", i, sep="")
            assign(valVar, tclVar(i))
            assign(paste("entry", i, sep=""), tkentry(subdialog, width="2", 
                textvariable=eval(parse(text=valVar))))
            tkgrid(tklabel(subdialog, text=old.levels[i]), eval(parse(text=paste("entry", i, sep=""))), sticky="w")
            }
        tkgrid(OKSubButton, cancelSubButton, sticky="w")
        for (row in 0:(nvalues + 1)) tkgrid.rowconfigure(subdialog, row, weight=0)
        for (col in 0:1) tkgrid.columnconfigure(subdialog, col, weight=0)
        .Tcl("update idletasks")
        tkwm.resizable(subdialog, 0, 0)
        tkbind(subdialog, "<Return>", onOKsub)
        if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOKsub)
        tkdestroy(top)           
        tkwm.deiconify(subdialog)
        if (.grab.focus) tkgrab.set(subdialog)
        tkfocus(subdialog)
        tkwait.window(subdialog)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(factor)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Factor (pick one)"), sticky="w")
    tkgrid(variableBox, variableScroll, sticky="nw")
    tkgrid(variableFrame, sticky="nw")
    tkgrid(tklabel(top, text="Name for factor"), sticky="w")
    tkgrid(factorNameField, sticky="w")
    tkgrid(tklabel(orderedFrame, text="Make ordered factor"), orderedCheckBox, sticky="w")
    tkgrid(orderedFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(variableScroll, sticky="ns")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(variableBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
#    if (.grab.focus) tkgrab.set(top)  # causes problems ?
    tkfocus(top)
    tkwait.window(top)
    }

standardize <- function(X){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Standardize Variables")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        xx <- paste('"', x, '"', sep="")
        command <- paste("scale(", .activeDataSet, "[,c(", paste(xx, collapse=","),
            ")])", sep="")
        assign(".Z", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".Z <- ", command, sep=""))
        for (i in 1:length(x)){
            Z <- paste("Z.", x[i], sep="")
            if (is.element(Z, .variables)) {
                if ("no" == tclvalue(checkReplace(Z))){
                    if (.grab.focus) tkgrab.release(top)
                    tkdestroy(top)
                    next
                    }
                }
#            justDoIt(paste(.activeDataSet, "$", Z, " <<- .Z[,", i, "]", sep=""))
            justDoIt(paste(.activeDataSet, "$", Z, " <- .Z[,", i, "]", sep=""))
            logger(paste(.activeDataSet, "$", Z, " <- .Z[,", i, "]", sep=""))
            }
        remove(.Z, envir=.GlobalEnv)   
        logger("remove(.Z)")
        activeDataSet(.activeDataSet)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(scale)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variables (pick one or more)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(top, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

helpDataSet <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    justDoIt(paste("help(", .activeDataSet, ")", sep=""))
    logger(paste("help(", .activeDataSet, ")", sep=""))
    }
    
variablesDataSet <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    doItAndPrint(paste("names(", .activeDataSet, ")", sep=""))
    }

exportDataSet <- function() {
    dsname <- activeDataSet()
    if (dsname == FALSE) {
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Export Active Data Set")
    optionsFrame <- tkframe(top)
    colnames <- tclVar("1")
    colnamesCheckBox <- tkcheckbutton(optionsFrame, variable=colnames)
    rownames <- tclVar("1")
    rownamesCheckBox <- tkcheckbutton(optionsFrame, variable=rownames)
    quotes <- tclVar("1")
    quotesCheckBox <- tkcheckbutton(optionsFrame, variable=quotes)
    delimiter <- tclVar("spaces")
    spacesButton <- tkradiobutton(optionsFrame, variable=delimiter, value="spaces")
    tabsButton <- tkradiobutton(optionsFrame, variable=delimiter, value="tabs")
    commasButton <- tkradiobutton(optionsFrame, variable=delimiter, value="commas")
    missingVariable <- tclVar("NA")
    missingEntry <- tkentry(optionsFrame, width="8", textvariable=missingVariable)
    onOK <- function(){
        col <- tclvalue(colnames) == 1
        row <- tclvalue(rownames) == 1
        quote <- tclvalue(quotes) == 1
        delim <- tclvalue(delimiter)
        missing <- tclvalue(missingVariable)
        sep <- if (delim == "tabs") "\\t"
            else if (delim == "spaces") " "
            else ","
        saveFile <- tclvalue(tkgetSaveFile(filetypes='{"Text Files" {".txt" ".TXT" ".dat" ".DAT"}} {"All Files" {"*"}}',
            defaultextension="txt", initialfile=paste(dsname, ".txt", sep="")))
        if (saveFile == "") {
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkfocus(.commander)
            return()
            }
        command <- paste("write.table(", dsname, ', "', saveFile, '", sep="', sep, 
            '", col.names=', col, ", row.names=", row, ", quote=", quote,
            ', na="', missing, '")', sep="")           
        justDoIt(command)
        logger(command)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }    
    buttonsFrame <- tkframe(top)  
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", default="active", command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(write.table)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(optionsFrame, text="Write variable names:"), colnamesCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Write row names:"), rownamesCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Quotes around character values:"), quotesCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Missing values:"), missingEntry, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Field Separator", fg="blue"), sticky="w", columnspan=2)
    tkgrid(tklabel(optionsFrame, text="Spaces"), spacesButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Tabs"), tabsButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Commas"), commasButton, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(helpButton, sticky="e") 
    for (row in 0:1) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)  
    tkbind(top, "<Return>", onOK) 
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

filterNA <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Data set", name, "already exists.\nOverwrite data set?"),
            icon="warning", type="yesno", default="no")
        }
    dataSet <- activeDataSet()
    if (dataSet == FALSE) {
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Remove Missing Data")
    allVariablesFrame <- tkframe(top)
    allVariables <- tclVar("1")
    allVariablesCheckBox <- tkcheckbutton(allVariablesFrame, variable=allVariables)
    variablesFrame <- tkframe(top)
    variablesBox <- tklistbox(variablesFrame, height=min(4, length(.variables)),
        selectmode="multiple", background="white", exportselection="FALSE")
    variablesScroll <- tkscrollbar(variablesFrame, repeatinterval=5, command=function(...) tkyview(variablesBox, ...))
    tkconfigure(variablesBox, yscrollcommand=function(...) tkset(variablesScroll, ...))
    for (var in .variables) tkinsert(variablesBox, "end", var)
    newDataSetName <- tclVar("<same as active data set>")
    dataSetNameFrame <- tkframe(top)
    dataSetNameEntry <- tkentry(dataSetNameFrame, width="25", textvariable=newDataSetName)
    onOK <- function(){
        newName <- trim.blanks(tclvalue(newDataSetName))
        if (newName == "<same as active data set>") newName <- .activeDataSet
        if (!is.valid.name(newName)){
            tkmessageBox(message=paste('"', newName, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            filterNA()
            return()
            }
        if (is.element(newName, listDataSets())) {
            if ("no" == tclvalue(checkReplace(newName))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                filterNA()
                return()
                }
            }
        if (tclvalue(allVariables) == "1"){
            command <- paste(newName, " <- na.omit(", .activeDataSet, ")", sep="")
            logger(command)
            justDoIt(command)
            activeDataSet(newName)
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)  
            tkfocus(.commander)
            }
        else {
            x <- .variables[as.numeric(tkcurselection(variablesBox)) + 1]
            if (0 > length(x)) {
                tkmessageBox(message="No variables were selected.", 
                    icon="error", type="ok")
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                filterNA()
                return()
                }
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            x <- paste('"', x, '"', sep="")
            command <- paste(newName, " <- na.omit(", .activeDataSet, "[,c(", paste(x, collapse=","),
                ')])', sep="")
            logger(command)
            justDoIt(command)
            activeDataSet(newName)
            tkfocus(.commander)
            }
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)  
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help("na.omit")
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)    
    tkgrid(tklabel(allVariablesFrame, text="Include all variables"), 
        allVariablesCheckBox, sticky="w")
    tkgrid(allVariablesFrame, sticky="w")
    tkgrid(tklabel(top, text="   OR", fg="red"), sticky="w")
    tkgrid(tklabel(variablesFrame, text="Variables (select one or more)"), sticky="w")
    tkgrid(variablesBox, variablesScroll, sticky="nw")
    tkgrid(variablesFrame, sticky="nw")
    tkgrid(tklabel(dataSetNameFrame, text="Name for new data set"), sticky="w")
    tkgrid(dataSetNameEntry, sticky="w")
    tkgrid(dataSetNameFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(variablesScroll, sticky="ns")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
#    if (.grab.focus) tkgrab.set(top)  # causes problems ?
    tkfocus(top)
    tkwait.window(top)
    }

subsetDataSet <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Data set", name, "already exists.\nOverwrite data set?"),
            icon="warning", type="yesno", default="no")
        }
    dataSet <- activeDataSet()
    if (dataSet == FALSE) {
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Subset Data Set")
    allVariablesFrame <- tkframe(top)
    allVariables <- tclVar("1")
    allVariablesCheckBox <- tkcheckbutton(allVariablesFrame, variable=allVariables)
    variablesFrame <- tkframe(top)
    variablesBox <- tklistbox(variablesFrame, height=min(4, length(.variables)),
        selectmode="multiple", background="white", exportselection="FALSE")
    variablesScroll <- tkscrollbar(variablesFrame, repeatinterval=5, command=function(...) tkyview(variablesBox, ...))
    tkconfigure(variablesBox, yscrollcommand=function(...) tkset(variablesScroll, ...))
    for (var in .variables) tkinsert(variablesBox, "end", var)
    subsetVariable <- tclVar("<all cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    newDataSetName <- tclVar("<same as active data set>")
    dataSetNameFrame <- tkframe(top)
    dataSetNameEntry <- tkentry(dataSetNameFrame, width="25", textvariable=newDataSetName)
    onOK <- function(){
        newName <- trim.blanks(tclvalue(newDataSetName))
        if (newName == "<same as active data set>") newName <- .activeDataSet
        if (!is.valid.name(newName)){
            tkmessageBox(message=paste('"', newName, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            subsetDataSet()
            return()
            }
        if (is.element(newName, listDataSets())) {
            if ("no" == tclvalue(checkReplace(newName))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                subsetDataSet()
                return()
                }
            }
        selectVars <- if (tclvalue(allVariables) == "1") ""
            else {
                x <- .variables[as.numeric(tkcurselection(variablesBox)) + 1]
                if (0 > length(x)) {
                    tkmessageBox(message="No variables were selected.", 
                        icon="error", type="ok")
                    if (.grab.focus) tkgrab.release(top)
                    tkdestroy(top)
                    subsetDataSet()
                    return()
                    }
                paste(", select=c(", paste(x, collapse=","), ")", sep="")
                }
        cases <- tclvalue(subsetVariable)
        selectCases <- if (cases == "<all cases>") ""
            else paste(", subset=", cases, sep="")
        if (selectVars == "" && selectCases ==""){
            tkmessageBox(message="New data set same as active data set.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            subsetDataSet()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste(newName, " <- subset(", .activeDataSet, selectCases, selectVars, ")",
            sep="")
        logger(command)
        justDoIt(command)
        activeDataSet(newName)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)  
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help("subset")
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)    
    tkgrid(tklabel(allVariablesFrame, text="Include all variables"), 
        allVariablesCheckBox, sticky="w")
    tkgrid(allVariablesFrame, sticky="w")
    tkgrid(tklabel(top, text="   OR", fg="red"), sticky="w")
    tkgrid(tklabel(variablesFrame, text="Variables (select one or more)"), sticky="w")
    tkgrid(variablesBox, variablesScroll, sticky="nw")
    tkgrid(variablesFrame, sticky="nw")
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(tklabel(dataSetNameFrame, text="Name for new data set"), sticky="w")
    tkgrid(dataSetNameEntry, sticky="w")
    tkgrid(dataSetNameFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(variablesScroll, sticky="ns")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
#    if (.grab.focus) tkgrab.set(top)  # causes problems ?
    tkfocus(top)
    tkwait.window(top)
    }

setCaseNames <- function(){
    dataSet <- activeDataSet()
    if (dataSet == FALSE) {
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Set Case Names")
    variablesListFrame <- tkframe(top)
    variablesBox <- tklistbox(variablesListFrame, height=min(4, length(.variables)),
        selectmode="single", background="white")
    variablesScroll <- tkscrollbar(variablesListFrame, repeatinterval=5, 
        command=function(...) tkyview(variablesBox, ...))
    tkconfigure(variablesBox, yscrollcommand=function(...) tkset(variablesScroll, ...))
    for (variable in .variables) tkinsert(variablesBox, "end", variable)
    onOK <- function(){
        variable <- as.character(tkget(variablesBox, "active"))
        var <- eval(parse(text=paste(dataSet, "$", variable, sep="")), envir=.GlobalEnv)
        if (length(var) != length(unique(var))){
            tkmessageBox(message="Case names must be unique.", icon="error", type="ok", default="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            setCaseNames()
            return()
            }
        command <- paste("row.names(", dataSet, ") <- as.character(", dataSet, "$", variable, ")", sep="")
        justDoIt(command)
        logger(command)
#        eval(parse(text=paste(dataSet, "$", variable, "<<- NULL", sep="")), envir=.GlobalEnv)
        eval(parse(text=paste(dataSet, "$", variable, "<- NULL", sep="")), envir=.GlobalEnv)
        logger(paste(dataSet, "$", variable, " <- NULL", sep=""))
        activeDataSet(dataSet)
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12",command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)    
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(row.names)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)       
    tkgrid(tklabel(top, text="Select variable containing row names"), sticky="w")
    tkgrid(variablesBox, variablesScroll, sticky="nw")
    tkgrid(variablesListFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(variablesScroll, sticky="ns")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)   
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkbind(variablesBox, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
    
renameVariables <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Rename Variables")
    variableFrame <- tkframe(top)
    variableBox <- tklistbox(variableFrame, height=min(4, length(.variables)),
        selectmode="multiple", background="white", exportselection="FALSE")
    variableScroll <- tkscrollbar(variableFrame, repeatinterval=5, 
        command=function(...) tkyview(variableBox, ...))
    tkconfigure(variableBox, yscrollcommand=function(...) tkset(variableScroll, ...))
    for (var in .variables) tkinsert(variableBox, "end", var)
    onOK <- function(){
        which.variables <- as.numeric(tkcurselection(variableBox)) + 1
        variables <- .variables[which.variables]
        nvariables <- length(variables)
        if (nvariables < 1) {
            tkmessageBox(message="No variables selected.", icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top) 
            renameVariables()
            }
        subdialog <- tktoplevel()
        tkwm.title(subdialog, "Variable Names")
        newnames <- rep("", nvariables)
        onOKsub <- function() {
            for (i in 1:nvariables){
                newnames[i] <- eval(parse(text=paste("tclvalue(newName", i, ")", sep="")))
                }
            if (any(newnames == "")){
                tkmessageBox(message="A variable name is empty.",
                    icon="error", type="ok")
                if (.grab.focus) tkgrab.release(subdialog)
                tkdestroy(subdialog)
                renameVariables()
                return()
                }
            test.names <- newnames == make.names(newnames)
            if (!all(test.names)){
                tkmessageBox(message=paste("The following variable names are not legal:\n",
                    paste(newnames[!test.names], collapse=", ")), icon="error", type="ok")
                if (.grab.focus) tkgrab.release(subdialog)
                tkdestroy(subdialog)
                renameVariables()
                return()
                }                
            all.names <- eval(parse(text=paste("names(", .activeDataSet, ")")))
            all.names[which.variables] <- newnames
            if (length(unique(all.names)) != length(all.names)){
                tkmessageBox(message="Variable names are not unique",
                    icon="error", type="ok")
                if (.grab.focus) tkgrab.release(subdialog)
                tkdestroy(subdialog)
                renameVariables()
                return()
                }
            command <- paste("names(", .activeDataSet, ")[c(", paste(which.variables, collapse=","),
                ")] <- c(", paste('"', newnames, '"', collapse=",", sep=""), ")", sep="")
            justDoIt(command)
            logger(command)
            activeDataSet(.activeDataSet)
            if (.grab.focus) tkgrab.release(subdialog)
            tkfocus(.commander)
            tkdestroy(subdialog)
            }
        onCancelSub <- function() {
            if (.grab.focus) tkgrab.release(subdialog)  
            tkfocus(.commander)
            tkdestroy(subdialog)
            }
        OKSubButton <- tkbutton(subdialog, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
        cancelSubButton <- tkbutton(subdialog, text="Cancel", fg="red", width="12",command=onCancelSub)
        tkgrid(tklabel(subdialog, text="Old Name", fg="blue"), 
            tklabel(subdialog, text="New name", fg="blue"), sticky="w")
        for (i in 1:nvariables){
            valVar <- paste("newName", i, sep="")
            assign(valVar, tclVar(""))
            assign(paste("entry", i, sep=""), tkentry(subdialog, width="20", 
                textvariable=eval(parse(text=valVar))))
            tkgrid(tklabel(subdialog, text=variables[i]), eval(parse(text=paste("entry", i, sep=""))), sticky="w")
            }
        tkgrid(OKSubButton, cancelSubButton, sticky="w")
        for (row in 0:(nvariables + 1)) tkgrid.rowconfigure(subdialog, row, weight=0)
        for (col in 0:1) tkgrid.columnconfigure(subdialog, col, weight=0)
        .Tcl("update idletasks")
        tkwm.resizable(subdialog, 0, 0)
        tkbind(subdialog, "<Return>", onOKsub)
        if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOKsub)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)           
        tkwm.deiconify(subdialog)
        if (.grab.focus) tkgrab.set(subdialog)
        tkfocus(entry1)
        tkwait.window(subdialog)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(names)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variables (pick one or more)"), sticky="w")
    tkgrid(variableBox, variableScroll, sticky="nw")
    tkgrid(variableFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="      "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(variableScroll, sticky="ns")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
#    if (.grab.focus) tkgrab.set(top)   # causes problems ?
    tkfocus(top)
    tkwait.window(top)
    }

setContrasts <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.factors) == 0){
        tkmessageBox(message="There no factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Set Contrasts for Factor")
    variableFrame <- tkframe(top)
    variableBox <- tklistbox(variableFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    variableScroll <- tkscrollbar(variableFrame, repeatinterval=5, 
        command=function(...) tkyview(variableBox, ...))
    tkconfigure(variableBox, yscrollcommand=function(...) tkset(variableScroll, ...))
    for (var in .factors) tkinsert(variableBox, "end", var)
    contrastsFrame <- tkframe(top)
    contrastsVariable <- tclVar(getOption("contrasts")[1])
    treatmentButton <- tkradiobutton(contrastsFrame, variable=contrastsVariable, value="contr.Treatment")
    sumButton <- tkradiobutton(contrastsFrame, variable=contrastsVariable, value="contr.Sum")
    helmertButton <- tkradiobutton(contrastsFrame, variable=contrastsVariable, value="contr.helmert")
    polyButton <- tkradiobutton(contrastsFrame, variable=contrastsVariable, value="contr.poly")
    specifyButton <- tkradiobutton(contrastsFrame, variable=contrastsVariable, value="specify")
    onOK <- function(){
        variable <- as.character(tkget(variableBox, "active"))
        contrasts <- tclvalue(contrastsVariable)
        if (contrasts != "specify"){
            command <- paste("contrasts(", .activeDataSet, "$", variable, ') <- "', contrasts, '"', sep="")
            justDoIt(command)
            logger(command)
            activeDataSet(.activeDataSet)          
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkfocus(.commander)
            }
        else{
            subdialog <- tktoplevel()
            tkwm.title(subdialog, "Specify Contrasts")
            tkgrid(tklabel(subdialog, text="Enter Contrast Coefficients", fg="blue"), sticky="w")
            env <- environment()
            tableFrame <- tkframe(subdialog)
            row.names <- eval(parse(text=paste("levels(", .activeDataSet, "$", variable, ")")))
            row.names <- substring(paste(abbreviate(row.names, 12), "            "), 1, 12)
            nrows <- length(row.names)
            ncols <- nrows - 1
            make.col.names <- "tklabel(tableFrame, text='Contrast Name:')"
            for (j in 1:ncols) {
                varname <- paste(".col.", j, sep="")
                assign(varname, tclVar(paste(".", j, sep="")), envir=env)
                make.col.names <- paste(make.col.names, ", ", 
                    "tkentry(tableFrame, width='12', textvariable=", varname, ")", sep="")
                }
            eval(parse(text=paste("tkgrid(", make.col.names, ", sticky='w')", sep="")), envir=env)
            for (i in 1:nrows){   
                make.row <- paste("tklabel(tableFrame, text='", row.names[i], "')")
                for (j in 1:ncols){
                    varname <- paste(".tab.", i, ".", j, sep="")
                    assign(varname, tclVar("0"), envir=env)
                    make.row <- paste(make.row, ", ", "tkentry(tableFrame, width='5', textvariable=", 
                        varname, ")", sep="")
                    }
                eval(parse(text=paste("tkgrid(", make.row, ", sticky='w')", sep="")), envir=env)
                }
            tkgrid(tableFrame, sticky="w")
            onOKsub <- function(){
                tkdestroy(top)
                cell <- 0
                values <- rep(NA, nrows*ncols)
                for (j in 1:ncols){
                    for (i in 1:nrows){
                        cell <- cell + 1
                        varname <- paste(".tab.", i, ".", j, sep="")
                        values[cell] <- as.numeric(eval(parse(text=paste("tclvalue(", varname,")", sep=""))))
                        }
                    }
                values <- na.omit(values)
                if (length(values) != nrows*ncols){
                    tkmessageBox(message=paste("Number of valid entries in contrast matrix(", length(values), ")\n",
                        "not equal to number of levels (", nrows,") * number of contrasts (", ncols,").", 
                        sep=""), icon="error", type="ok")
                    if (.grab.focus) tkgrab.release(subdialog)
                    tkdestroy(subdialog)
                    setContrasts()
                    return()
                    }
                if (qr(matrix(values, nrows, ncols))$rank < ncols) {
                    tkmessageBox(message="Contrast matrix is not of full column rank", 
                        icon="error", type="ok")
                    if (.grab.focus) tkgrab.release(subdialog)
                    tkdestroy(subdialog)
                    setContrasts()
                    return()
                    }  
                contrast.names <- rep("", ncols)
                for (j in 1:ncols){
                    varname <- paste(".col.", j, sep="")
                    contrast.names[j] <- eval(parse(text=paste("tclvalue(", varname,")", sep="")))
                    }
                if (length(unique(contrast.names)) < ncols) {
                    tkmessageBox(message="Contrast names must be unique", 
                        icon="error", type="ok")
                    if (.grab.focus) tkgrab.release(subdialog)
                    tkdestroy(subdialog)
                    setContrasts()
                    return()
                    }                    
                if (.grab.focus) tkgrab.release(subdialog)
                tkdestroy(subdialog)
                command <- paste("matrix(c(", paste(values, collapse=","), "), ", nrows, ", ", ncols,
                    ")", sep="")
                assign(".Contrasts", justDoIt(command), envir=.GlobalEnv)
                logger(paste(".Contrasts <- ", command, sep=""))
                command <- paste("colnames(.Contrasts) <- c(", 
                    paste("'", contrast.names, "'", sep="", collapse=", "), ")", sep="")
                justDoIt(command)
                logger(command)
                command <- paste("contrasts(", .activeDataSet, "$", variable, ") <- .Contrasts", sep="")
                justDoIt(command)
                logger(command)
                justDoIt("remove(.Contrasts, envir=.GlobalEnv)")   
                logger("remove(.Contrasts)") 
                activeDataSet(.activeDataSet)                                      
                tkfocus(.commander)
                }
            subButtonsFrame <- tkframe(subdialog)
            OKSubButton <- tkbutton(subButtonsFrame, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
            onCancelSub <- function() {
                if (.grab.focus) tkgrab.release(subdialog)
                tkfocus(.commander)
                tkdestroy(subdialog) 
                tkdestroy(top) 
                }
            cancelSubButton <- tkbutton(subButtonsFrame, text="Cancel", fg="red", width="12",command=onCancelSub)
            onHelpSub <- function() {
                if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(subdialog)
                help("contrasts")
                }
            helpSubButton <- tkbutton(subButtonsFrame, text="Help", width="12", command=onHelpSub)
            tkgrid(tableFrame, sticky="w")
            tkgrid(tklabel(subdialog, text=""))
            tkgrid(OKSubButton, cancelSubButton, tklabel(subButtonsFrame, text="        "), helpSubButton, sticky="w")
            tkgrid(subButtonsFrame, sticky="w")
            for (row in 0:4) tkgrid.rowconfigure(subdialog, row, weight=0)
            for (col in 0:0) tkgrid.columnconfigure(subdialog, col, weight=0)
            .Tcl("update idletasks")
            tkwm.resizable(subdialog, 0, 0)
            tkbind(subdialog, "<Return>", onOK)
            if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOK)
            tkwm.deiconify(subdialog)
            if (.grab.focus) tkgrab.set(subdialog)
            tkfocus(subdialog)
            tkwait.window(subdialog)        
            }
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(contrasts)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Factor (pick one)"), sticky="w")
    tkgrid(variableBox, variableScroll, sticky="nw")
    tkgrid(variableFrame, sticky="nw")
    tkgrid(tklabel(top, text="Contrasts", fg="blue"), sticky="w")
    tkgrid(tklabel(contrastsFrame, text="Treatment (dummy) contrasts"), treatmentButton, sticky="w")
    tkgrid(tklabel(contrastsFrame, text="Sum (deviation) contrasts"), sumButton, sticky="w")
    tkgrid(tklabel(contrastsFrame, text="Helmert contrasts"), helmertButton, sticky="w")
    tkgrid(tklabel(contrastsFrame, text="Polynomial contrasts"), polyButton, sticky="w")
    tkgrid(tklabel(contrastsFrame, text="Other (specify)"), specifyButton, sticky="w")
    tkgrid(contrastsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(variableScroll, sticky="ns")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(variableBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top) 
    tkfocus(top)
    tkwait.window(top)
    }
# Distributions menu dialogs

# last modified 27 Jab 04 by J. Fox

normalQuantiles <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Normal Quantiles")
    quantilesVar <- tclVar("")
    quantilesEntry <- tkentry(top, width="30", textvariable=quantilesVar)
    muVar <- tclVar("0")
    muEntry <- tkentry(top, width="6", textvariable=muVar)
    sigmaVar <- tclVar("1")
    sigmaEntry <- tkentry(top, width="6", textvariable=sigmaVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            tkmessageBox(message="No probabilities specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            normalQuantiles()
            return()
            }
        mu <- as.numeric(tclvalue(muVar))
        sigma <- as.numeric(tclvalue(sigmaVar))
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("qnorm(c(", quantiles, "), mean=", mu, 
            ", sd=", sigma, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        } 
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(qnorm)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Probabilities"), quantilesEntry, sticky="e")
    tkgrid(tklabel(top, text="mu (mean)"), muEntry, sticky="e")
    tkgrid(tklabel(top, text="sigma (standard deviation)"), sigmaEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(muEntry, sticky="w")
    tkgrid.configure(sigmaEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(quantilesEntry)
    tkwait.window(top)
    }

normalProbabilities <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Normal Probabilities")
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- tkentry(top, width="30", textvariable=probabilitiesVar)
    muVar <- tclVar("0")
    muEntry <- tkentry(top, width="6", textvariable=muVar)
    sigmaVar <- tclVar("1")
    sigmaEntry <- tkentry(top, width="6", textvariable=sigmaVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            tkmessageBox(message="No values specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            normalProbabilities()
            return()
            }
        mu <- as.numeric(tclvalue(muVar))
        sigma <- as.numeric(tclvalue(sigmaVar))
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("pnorm(c(", probabilities, "), mean=", mu, 
            ", sd=", sigma, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(pnorm)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable value(s)"), probabilitiesEntry, sticky="e")
    tkgrid(tklabel(top, text="mu (mean)"), muEntry, sticky="e")
    tkgrid(tklabel(top, text="sigma (standard deviation)"), sigmaEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(muEntry, sticky="w")
    tkgrid.configure(sigmaEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(probabilitiesEntry)
    tkwait.window(top)
    }

tQuantiles <- function(){
    top <- tktoplevel()
    tkwm.title(top, "t Quantiles")
    quantilesVar <- tclVar("")
    quantilesEntry <- tkentry(top, width="30", textvariable=quantilesVar)
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            tkmessageBox(message="No probabilities specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tQuantiles()
            return()
            }
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            tkmessageBox(message="Degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tQuantiles()
            return()
            }
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("qt(c(", quantiles, "), df=", df, 
            ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(qt)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Probabilities"), quantilesEntry, sticky="e")
    tkgrid(tklabel(top, text="Degrees of freedom"), dfEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(quantilesEntry)
    tkwait.window(top)
    }
    
tProbabilities <- function(){
    top <- tktoplevel()
    tkwm.title(top, "t Probabilities")
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- tkentry(top, width="30", textvariable=probabilitiesVar)
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        df <- as.numeric(tclvalue(dfVar))
        if ("" == probabilities) {
            tkmessageBox(message="No values specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tProbabilities()
            return()
            }
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            tkmessageBox(message="Degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tProbabilities()
            return()
            }
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("pt(c(", probabilities, "), df=", df, 
            ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(pt)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable value(s)"), probabilitiesEntry, sticky="e")
    tkgrid(tklabel(top, text="Degrees of freedom"), dfEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(probabilitiesEntry)
    tkwait.window(top)
    }

chisqQuantiles <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Chi-Squared Quantiles")
    quantilesVar <- tclVar("")
    quantilesEntry <- tkentry(top, width="30", textvariable=quantilesVar)
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            tkmessageBox(message="No probabilities specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            chisqQuantiles()
            return()
            }
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            tkmessageBox(message="Degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            chisqQuantiles()
            return()
            }
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("qchisq(c(", quantiles, "), df=", df, 
            ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(qchisq)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Probabilities"), quantilesEntry, sticky="e")
    tkgrid(tklabel(top, text="Degrees of freedom"), dfEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(quantilesEntry)
    tkwait.window(top)
    }
    
chisqProbabilities <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Chi-Squared Probabilities")
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- tkentry(top, width="30", textvariable=probabilitiesVar)
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            tkmessageBox(message="No values specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            chisqProbabilities()
            return()
            }
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            tkmessageBox(message="Degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            chisqProbabilities()
            return()
            }
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("pchisq(c(", probabilities, "), df=", df, 
            ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(pchisq)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable value(s)"), probabilitiesEntry, sticky="e")
    tkgrid(tklabel(top, text="Degrees of freedom"), dfEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(probabilitiesEntry)
    tkwait.window(top)
    }

FQuantiles <- function(){
    top <- tktoplevel()
    tkwm.title(top, "F Quantiles")
    quantilesVar <- tclVar("")
    quantilesEntry <- tkentry(top, width="30", textvariable=quantilesVar)
    df1Var <- tclVar("")
    df1Entry <- tkentry(top, width="6", textvariable=df1Var)
    df2Var <- tclVar("")
    df2Entry <- tkentry(top, width="6", textvariable=df2Var)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            tkmessageBox(message="Probabilities not specified", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            FQuantiles()
            return()
            }
        df1 <- as.numeric(tclvalue(df1Var))
        df2 <- as.numeric(tclvalue(df2Var))
        if (is.na(df1) || is.na(df2)) {
            tkmessageBox(message="Degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            FQuantiles()
            return()
            }
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("qf(c(", quantiles, "), df1=", df1, 
            ", df2=", df2, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(qf)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Probabilities"), quantilesEntry, sticky="e")
    tkgrid(tklabel(top, text="Numerator degrees of freedom"), df1Entry, sticky="e")
    tkgrid(tklabel(top, text="Denominator degrees of freedom"), df2Entry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(df1Entry, sticky="w")
    tkgrid.configure(df2Entry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(quantilesEntry)
    tkwait.window(top)
    }
    
FProbabilities <- function(){
    top <- tktoplevel()
    tkwm.title(top, "F Probabilities")
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- tkentry(top, width="30", textvariable=probabilitiesVar)
    df1Var <- tclVar("")
    df1Entry <- tkentry(top, width="6", textvariable=df1Var)
    df2Var <- tclVar("")
    df2Entry <- tkentry(top, width="6", textvariable=df2Var)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            tkmessageBox(message="Values not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            FProbabilities()
            return()
            }
        df1 <- as.numeric(tclvalue(df1Var))
        df2 <- as.numeric(tclvalue(df2Var))
        if (is.na(df1) || is.na(df2)) {
            tkmessageBox(message="Degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            FProbabilities()
            return()
            }
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("pf(c(", probabilities, "), df1=", df1, 
            ", df2=", df2, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(pf)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable value(s)"), probabilitiesEntry, sticky="e")
    tkgrid(tklabel(top, text="Numerator degrees of freedom"), df1Entry, sticky="e")
    tkgrid(tklabel(top, text="Denominator degrees of freedom"), df2Entry, sticky="e")    
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(df1Entry, sticky="w")
    tkgrid.configure(df2Entry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(probabilitiesEntry)
    tkwait.window(top)
    }

binomialQuantiles <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Binomial Quantiles")
    quantilesVar <- tclVar("")
    quantilesEntry <- tkentry(top, width="30", textvariable=quantilesVar)
    trialsVar <- tclVar("")
    trialsEntry <- tkentry(top, width="6", textvariable=trialsVar)
    probVar <- tclVar(".5")
    probEntry <- tkentry(top, width="6", textvariable=probVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        trials <- as.numeric(tclvalue(trialsVar))
        prob <- as.numeric(tclvalue(probVar))
        if ("" == quantiles) {
            tkmessageBox(message="Probabilities not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialQuantiles()
            return()
            }
        if (is.na(trials)) {
            tkmessageBox(message="Binomial trials not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialQuantiles()
            return()
            }
        if (is.na(prob)) {
            tkmessageBox(message="Probability of success not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialQuantiles()
            return()
            }
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("qbinom(c(", quantiles, "), size=", trials, 
            ", prob=", prob, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        } 
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(qbinom)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Probabilities"), quantilesEntry, sticky="e")
    tkgrid(tklabel(top, text="Binomial trials"), trialsEntry, sticky="e")
    tkgrid(tklabel(top, text="Probability of success"), probEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(quantilesEntry)
    tkwait.window(top)
    }
    
binomialProbabilities <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Cumulative Binomial Probabilities")
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- tkentry(top, width="30", textvariable=probabilitiesVar)
    trialsVar <- tclVar("")
    trialsEntry <- tkentry(top, width="6", textvariable=trialsVar)
    probVar <- tclVar(".5")
    probEntry <- tkentry(top, width="6", textvariable=probVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        trials <- as.numeric(tclvalue(trialsVar))
        prob <- as.numeric(tclvalue(probVar))
        if ("" == probabilities) {
            tkmessageBox(message="Values not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialProbabilities()
            return()
            }
        if (is.na(trials)) {
            tkmessageBox(message="Binomial trials not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialProbabilities()
            return()
            }
        if (is.na(prob)) {
            tkmessageBox(message="Probability of success not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialProbabilities()
            return()
            }
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("pbinom(c(", probabilities, "), size=", trials, 
            ", prob=", prob, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(pbinom)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable value(s)"), probabilitiesEntry, sticky="e")
    tkgrid(tklabel(top, text="Binomial trials"), trialsEntry, sticky="e")
    tkgrid(tklabel(top, text="Probability of success"), probEntry, sticky="e")    
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(probabilitiesEntry)
    tkwait.window(top)
    }

binomialMass <- function(){
    checkTrials <- function(trials){
        tkmessageBox(message=paste("Number of trials", trials, "is large.\nCreate long output?"),
            icon="warning", type="yesno", default="no")
        }
    top <- tktoplevel()
    tkwm.title(top, "Binomial Probabilities")
    trialsVar <- tclVar("")
    trialsEntry <- tkentry(top, width="6", textvariable=trialsVar)
    probVar <- tclVar(".5")
    probEntry <- tkentry(top, width="6", textvariable=probVar)
    onOK <- function(){
        trials <- as.numeric(tclvalue(trialsVar))
        if (is.na(trials)) {
            tkmessageBox(message="Binomial trials not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialMass()
            return()
            }
        if (trials > 50){
            if ("no" == tclvalue(checkTrials(trials))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                binomialMass()
                return()
                }
            }
        prob <- as.numeric(tclvalue(probVar))
        if (is.na(prob)) {
            tkmessageBox(message="Probability of success not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialMass()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("data.frame(Pr=dbinom(0:", trials, ", size=", trials, 
            ", prob=", prob, "))", sep="")
        logger(paste(".Table <- ", command, sep=""))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        logger(paste("rownames(.Table) <- 0:", trials, sep=""))
#        justDoIt(paste("rownames(.Table) <<- 0:", trials, sep=""))
        justDoIt(paste("rownames(.Table) <- 0:", trials, sep=""))
        justDoIt(paste("print(", logger(".Table"), ")", sep=""))
        logger("remove(.Table)") 
        remove(.Table, envir=.GlobalEnv)       
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(dbinom)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Binomial trials"), trialsEntry, sticky="e")
    tkgrid(tklabel(top, text="Probability of success"), probEntry, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, tklabel(top, text="    "), helpButton, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(trialsEntry)
    tkwait.window(top)
    }

PoissonMass <- function(){
    checkRange <- function(range){
        tkmessageBox(message=paste("Range of values over which to plot,", range, ", is large.\nCreate long output?"),
            icon="warning", type="yesno", default="no")
        }
    top <- tktoplevel()
    tkwm.title(top, "Poisson Probabilities")
    meanVar <- tclVar("")
    meanEntry <- tkentry(top, width="6", textvariable=meanVar)
    onOK <- function(){
        mean <- as.numeric(tclvalue(meanVar))
        if (is.na(mean)) {
            tkmessageBox(message="Poisson mean not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            PoissonMass()
            return()
            }
        min <- qpois(.00005, lambda=mean)
        max <- qpois(.99995, lambda=mean)
        range <- max - min
        if (range > 50){
            if ("no" == tclvalue(checkRange(range))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                PoissonMass()
                return()
                }
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("data.frame(Pr=round(dpois(", min, ":", max, ", lambda=", mean, "), 4))", sep="")
        logger(paste(".Table <- ", command, sep=""))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        logger(paste("rownames(.Table) <- ", min, ":", max, sep=""))
#        justDoIt(paste("rownames(.Table) <<- ", min, ":", max, sep=""))
        justDoIt(paste("rownames(.Table) <- ", min, ":", max, sep=""))
        justDoIt(paste("print(", logger(".Table"), ")", sep=""))
        logger("remove(.Table)") 
        remove(.Table, envir=.GlobalEnv)       
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(dpois)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Mean"), meanEntry, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, tklabel(top, text="    "), helpButton, sticky="w")
    tkgrid.configure(meanEntry, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:1) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(meanEntry)
    tkwait.window(top)
    }
# Distributions  -> Plot Distributions menu dialogs

# last modified 27 Jan 04 by J. Fox

normalDistributionPlot <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Normal Distribution")
    muVar <- tclVar("0")
    muEntry <- tkentry(top, width="6", textvariable=muVar)
    sigmaVar <- tclVar("1")
    sigmaEntry <- tkentry(top, width="6", textvariable=sigmaVar)
    functionVar <- tclVar("Density")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        mu <- as.numeric(tclvalue(muVar))
        sigma <- as.numeric(tclvalue(sigmaVar))
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dnorm" else "pnorm"
        min <- round(qnorm(.0005, mean=mu, sd=sigma), 3)
        max <- round(qnorm(.9995, mean=mu, sd=sigma), 3)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("seq(", min, ", ", max, ", length=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, mean=", mu, 
            ", sd=", sigma, '), xlab="x", ylab="', fun, 
            '", main=expression(paste("Normal Distribution: ", mu, " = ',
            mu, ', ", sigma, " = ', sigma, '")), type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(dnorm)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="mu (mean)"), muEntry, sticky="e")
    tkgrid(tklabel(top, text="sigma (standard deviation)"), sigmaEntry, sticky="e")
    tkgrid(tklabel(top, text="Plot density function"), densityButton, sticky="e")
    tkgrid(tklabel(top, text="Plot distribution function"), distributionButton, sticky="e")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(muEntry, sticky="w")
    tkgrid.configure(sigmaEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(muEntry)
    tkwait.window(top)
    }

tDistributionPlot <- function(){
    top <- tktoplevel()
    tkwm.title(top, "t Distribution")
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    functionVar <- tclVar("Density")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            tkmessageBox(message="Degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tDistributionPlot()
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dt" else "pt"
        min <- round(qt(.0005, df=df), 3)
        max <- round(qt(.9995, df=df), 3)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("seq(", min, ", ", max, ", length=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, df=", df, 
            '), xlab="t", ylab="', fun, 
            '", main="t Distribution: df = ', df, '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(dt)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Degrees of freedom"), dfEntry, sticky="e")
    tkgrid(tklabel(top, text="Plot density function"), densityButton, sticky="e")
    tkgrid(tklabel(top, text="Plot distribution function"), distributionButton, sticky="e")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(dfEntry)
    tkwait.window(top)
    }

chisquareDistributionPlot <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Chi-squared Distribution")
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    functionVar <- tclVar("Density")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            tkmessageBox(message="Degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            chisquareDistributionPlot()
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dchisq" else "pchisq"
        min <- round(qchisq(.0005, df=df), 3)
        max <- round(qchisq(.9995, df=df), 3)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("seq(", min, ", ", max, ", length=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, df=", df, 
            '), xlab=expression(chi^2), ylab="', fun, 
            '", main="Chi-Squared Distribution: df = ', df, '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(dchisq)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Degrees of freedom"), dfEntry, sticky="e")
    tkgrid(tklabel(top, text="Plot density function"), densityButton, sticky="e")
    tkgrid(tklabel(top, text="Plot distribution function"), distributionButton, sticky="e")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(dfEntry)
    tkwait.window(top)
    }

FDistributionPlot <- function(){
    top <- tktoplevel()
    tkwm.title(top, "F Distribution")
    df1Var <- tclVar("")
    df2Var <- tclVar("")
    df1Entry <- tkentry(top, width="6", textvariable=df1Var)
    df2Entry <- tkentry(top, width="6", textvariable=df2Var)
    functionVar <- tclVar("Density")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        df1 <- as.numeric(tclvalue(df1Var))
        df2 <- as.numeric(tclvalue(df2Var))
        if (is.na(df1)) {
            tkmessageBox(message="Numerator degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            FDistributionPlot()
            return()
            }
        if (is.na(df2)) {
            tkmessageBox(message="Denominator degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            FDistributionPlot()
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "df" else "pf"
        min <- round(qf(.0005, df1=df1, df2=df2), 3)
        max <- round(qf(.9995, df1=df1, df2=df2), 3)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("seq(", min, ", ", max, ", length=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, df1=", df1, ", df2=", df2,
            '), xlab="f", ylab="', fun, 
            '", main="F Distribution: Numerator df = ', df1, ', Denominator df = ', df2, 
            '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(df)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Numerator degrees of freedom"), df1Entry, sticky="e")
    tkgrid(tklabel(top, text="Denominator degrees of freedom"), df2Entry, sticky="e")
    tkgrid(tklabel(top, text="Plot density function"), densityButton, sticky="e")
    tkgrid(tklabel(top, text="Plot distribution function"), distributionButton, sticky="e")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(df1Entry, sticky="w")
    tkgrid.configure(df2Entry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(df1Entry)
    tkwait.window(top)
    }

binomialDistributionPlot <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Binomial Distribution")
    trialsVar <- tclVar("")
    trialsEntry <- tkentry(top, width="6", textvariable=trialsVar)
    probVar <- tclVar(".5")
    probEntry <- tkentry(top, width="6", textvariable=probVar)
    functionVar <- tclVar("Probability")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Probability")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        trials <- as.numeric(tclvalue(trialsVar))
        if (is.na(trials)) {
            tkmessageBox(message="Binomial trials not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialDistributionPlot()
            return()
            }
        prob <- as.numeric(tclvalue(probVar))
        if (is.na(prob)) {
            tkmessageBox(message="Probability of success not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialDistributionPlot()
            return()
            }
        fun <- tclvalue(functionVar)
        min <- qbinom(.0005, size=trials, prob=prob)
        max <- qbinom(.9995, size=trials, prob=prob)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste(min, ":", max, sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        if (fun == "Probability"){
            doItAndPrint(paste("plot(.x, dbinom(.x, size=", trials, ", prob=", prob,
                '), xlab="Number of Successes", ylab="Probability Mass", main="Binomial Distribution: Trials = ', 
                trials, ', Probability of success = ', prob, '", type="h")', sep=""))
            doItAndPrint(paste("points(.x, dbinom(.x, size=", trials, ", prob=", prob,
                '), pch=16)', sep=""))
            }
        else {
            command <- "rep(.x, rep(2, length(.x)))"
            logger(paste(".x <- ", command, sep=""))
            assign(".x", justDoIt(command), envir=.GlobalEnv)
            doItAndPrint(paste("plot(.x[-1], pbinom(.x, size=", trials, ", prob=", prob,
                ')[-length(.x)], xlab="Number of Successes", ylab="Cumulative Probability", main="Binomial Distribution: Trials = ', 
                trials, ', Probability of success = ', prob, '", type="l")', sep=""))
            }
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(dbinom)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Binomial trials"), trialsEntry, sticky="e")
    tkgrid(tklabel(top, text="Probability of success"), probEntry, sticky="e")
    tkgrid(tklabel(top, text="Plot probability mass function"), densityButton, sticky="e")
    tkgrid(tklabel(top, text="Plot distribution function"), distributionButton, sticky="e")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(trialsEntry)
    tkwait.window(top)
    }

PoissonDistributionPlot <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Poisson Distribution")
    meanVar <- tclVar("")
    meanEntry <- tkentry(top, width="6", textvariable=meanVar)
    functionVar <- tclVar("Probability")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Probability")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        mean <- as.numeric(tclvalue(meanVar))
        if (is.na(mean)) {
            tkmessageBox(message="Mean not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            PoissonDistributionPlot()
            return()
            }
        fun <- tclvalue(functionVar)
        min <- qpois(.0005, lambda=mean)
        max <- qpois(.9995, lambda=mean)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste(min, ":", max, sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        if (fun == "Probability"){
            doItAndPrint(paste("plot(.x, dpois(.x, lambda=", mean,
                '), xlab="x", ylab="Probability Mass", main="Poisson Distribution: Mean = ', 
                mean, '", type="h")', sep=""))
            doItAndPrint(paste("points(.x, dpois(.x, lambda=", mean,
                '), pch=16)', sep=""))
            }
        else {
            command <- "rep(.x, rep(2, length(.x)))"
            logger(paste(".x <- ", command, sep=""))
            assign(".x", justDoIt(command), envir=.GlobalEnv)
            doItAndPrint(paste("plot(.x[-1], ppois(.x, lambda=", mean,
                ')[-length(.x)], xlab="x", ylab="Probability Mass", main="Poisson Distribution: Mean = ', 
                mean, '", type="l")', sep=""))
            }
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(dpois)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="mean"), meanEntry, sticky="e")
    tkgrid(tklabel(top, text="Plot probability mass function"), densityButton, sticky="e")
    tkgrid(tklabel(top, text="Plot distribution function"), distributionButton, sticky="e")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(meanEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(meanEntry)
    tkwait.window(top)
    }
# last modified 4 Feb 2004 by J. Fox

# File menu dialogs

loadLog <- function(){
    logFile <- tclvalue(tkgetOpenFile(filetypes='{"Log Files" {".log"}} {"All Files" {"*"}}',
        defaultextension="log"))
    if (logFile == "") return()
    fileCon <- file(logFile, "r")
    contents <- readLines(fileCon)
    close(fileCon)
    assign(".logFileName", logFile, envir=.GlobalEnv)
    if (tclvalue(tkget(.log, "1.0", "end")) != "\n"){
        response2 <- tkmessageBox(message="Save current log file?",
                icon="question", type="yesno", default="yes")
        if ("yes" == tclvalue(response2)) saveLog()
        }
    tkdelete(.log, "1.0", "end")
    tkinsert(.log, "end", paste(contents, collapse="\n"))
    }
    
saveLog <- function() {
    if (is.null(.logFileName)) {
        saveLogAs()
        return()
        }
    log <- tclvalue(tkget(.log, "1.0", "end"))
    fileCon <- file(.logFileName, "w")
    cat(log, file = fileCon)
    close(fileCon)
    }

saveLogAs <- function() {
    logFile <- tclvalue(tkgetSaveFile(filetypes='{"Log Files" {".log"}} {"All Files" {"*"}}',
        defaultextension="log", initialfile="RCommander.log"))
    log <- tclvalue(tkget(.log, "1.0", "end"))
    fileCon <- file(logFile, "w")
    cat(log, file = fileCon)
    close(fileCon)
    assign(".logFileName", logFile, envir=.GlobalEnv)
    }


closeCommander <- function(){
    globals <- c(".activeDataSet", ".activeModel", ".attachDataSet", ".commander", 
        ".dataSetLabel", ".dataSetName", ".double.click", ".factors",
        ".log", ".logCommands", ".logFileName", ".logFont", 
        ".grab.focus", ".modelLabel", ".modelName", ".modelNumber", ".modelWithSubset", 
        ".numeric", "oldPager", ".operatorFont", ".rgl", ".saveOptions", ".sort.names",
        ".twoLevelFactors", ".variables")
    response <- tclvalue(tkmessageBox(message="Exit?",
        icon="question", type="okcancel", default="cancel"))
    if (response == "cancel") return(invisible(response))
    if (.rgl) rgl.quit()
    if (!is.null(.activeDataSet) && (tclvalue(.attachDataSet) == "1")) 
        justDoIt(logger(paste("detach(", .activeDataSet, ")", sep="")))
    assign(".activeDataSet", NULL, envir=.GlobalEnv)
    assign(".activeModel", NULL, envir=.GlobalEnv)
    if (tclvalue(tkget(.log, "1.0", "end")) != "\n"){
        response2 <- tkmessageBox(message="Save log file?",
                icon="question", type="yesno", default="yes")
        if ("yes" == tclvalue(response2)) saveLog()
        }
    if (.Platform$OS.type != "windows") options(.oldPager)
    options(.saveOptions)
    tkdestroy(.commander)
    tkwait <- options("Rcmdr")[[1]]$tkwait  # to address problem in Debian Linux
    if ((!is.null(tkwait)) && tkwait) tclvalue(.commander.done) <<- "1"   
    which.globals <- sapply(globals, exists, envir=.GlobalEnv)
    remove(list=globals[which.globals], envir=.GlobalEnv)
    return(invisible(response))
    }
    
closeCommanderAndR <- function(){
    response <- closeCommander()
    if (response == "cancel") return()
    quit(save="no")
    }

Options <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Commander Options")
    current <- options("Rcmdr")[[1]]
    log.font.size <- if (is.null(current$log.font.size)) 10 else current$log.font.size
    log.width <- if (is.null(current$log.width)) 70 else current$log.width
    log.height <- if (is.null(current$log.height)) 15 else current$log.height
    contrasts <- if (is.null(current$contrasts)) c("contr.Treatment", "contr.poly") else current$contrasts
    grab.focus <- if (is.null(current$grab.focus)) TRUE else current$grab.focus
    double.click <- if (is.null(current$double.click)) FALSE else current$double.click
    sort.names <- if (is.null(current$sort.names)) TRUE else current$sort.names
    show.edit.button <- if (is.null(current$show.edit.button)) TRUE else current$show.edit.button
    scale.factor <- current$scale.factor
    default.font.size <- if (is.null(current$default.font.size)) 10 else current$default.font.size
    default.font <- if(is.null(current$default.font)) paste("*helvetica-medium-r-normal-*-",
            default.font.size, "*", sep="") else current$default.font
    logFontSizeVar <- tclVar(log.font.size)
    logFontSizeSlider <- tkscale(top, from=6, to=20, showvalue=TRUE, variable=logFontSizeVar,
        resolution=1, orient="horizontal")
    logWidthVar <- tclVar(log.width)
    logWidthSlider <- tkscale(top, from=30, to=120, showvalue=TRUE, variable=logWidthVar,
        resolution=5, orient="horizontal")    
    logHeightVar <- tclVar(log.height)
    logHeightSlider <- tkscale(top, from=5, to=35, showvalue=TRUE, variable=logHeightVar,
        resolution=5, orient="horizontal")   
    contrasts1 <- tclVar(contrasts[1])
    contrasts2 <- tclVar(contrasts[2])
    contrastsFrame <- tkframe(top)
    contrasts1Entry <- tkentry(contrastsFrame, width="15", textvariable=contrasts1)  
    contrasts2Entry <- tkentry(contrastsFrame, width="15", textvariable=contrasts2) 
    grabFocusVar <- tclVar(as.numeric(grab.focus))
    grabFocusCheckBox <- tkcheckbutton(top, variable=grabFocusVar)
    doubleClickVar <- tclVar(as.numeric(double.click))
    doubleClickCheckBox <- tkcheckbutton(top, variable=doubleClickVar)
    sortNamesVar <- tclVar(as.numeric(sort.names))
    sortNamesCheckBox <- tkcheckbutton(top, variable=sortNamesVar)
    showEditButtonVar <- tclVar(as.numeric(show.edit.button))
    showEditButtonCheckBox <- tkcheckbutton(top, variable=showEditButtonVar)
    scaleFactorVar <- tclVar(if (is.null(scale.factor)) 1.0 else scale.factor)
    scaleFactorSlider <- tkscale(top, from=0.2, to=3.0, showvalue=TRUE, variable=scaleFactorVar,
        resolution=0.2, orient="horizontal")        
    defaultFont <- tclVar(default.font)
    defaultFontEntry <- tkentry(top, width="30", textvariable=scaleFactorVar)                  
    onOK <- function(){
        log.font.size <- round(as.numeric(tclvalue(logFontSizeVar)))
        log.width <- round(as.numeric(tclvalue(logWidthVar)))
        log.height <- as.numeric(tclvalue(logHeightVar))
        contrasts <- c(tclvalue(contrasts1), tclvalue(contrasts2))
        grab.focus <- tclvalue(grabFocusVar) == 1
        double.click <- tclvalue(doubleClickVar) == 1
        sort.names <- tclvalue(sortNamesVar) == 1
        show.edit.button <- tclvalue(showEditButtonVar) == 1
        scale.factor <- round(as.numeric(tclvalue(scaleFactorVar)), 1)
        if (scale.factor == 1) scale.factor <- NULL
        default.font <- tclvalue(defaultFont)
        options <- list(
            log.font.size=log.font.size,
            log.width=log.width,
            log.height=log.height,
            contrasts=contrasts,
            grab.focus=grab.focus,
            double.click=double.click,
            sort.names=sort.names,
            show.edit.button=show.edit.button
            )
        if (.Platform$OS.type == "windows") options$scale.factor <- scale.factor
        else options$default.font <- default.font
        options(Rcmdr=options)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)  
        closeCommander()
        Commander()
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }    
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="Restart Commander", fg="darkgreen", command=onOK, 
        default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(Commander)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Log-font size (points)"), logFontSizeSlider, sticky="se")
    tkgrid.configure(logFontSizeSlider, sticky="w")
    tkgrid(tklabel(top, text="Log width (characters)"), logWidthSlider, sticky="se")
    tkgrid.configure(logWidthSlider, sticky="w")
    tkgrid(tklabel(top, text="Log height (lines)"), logHeightSlider, sticky="se")
    tkgrid.configure(logHeightSlider, sticky="w")
    tkgrid(tklabel(contrastsFrame, text="Unordered factors"), tklabel(contrastsFrame, text="   "),
        tklabel(contrastsFrame, text="Ordered factors"), sticky="w")
    tkgrid(contrasts1Entry, tklabel(contrastsFrame, text="   "), contrasts2Entry, sticky="w")
    tkgrid(tklabel(top, text="Contrasts"), contrastsFrame, sticky="se")
    tkgrid.configure(contrastsFrame, sticky="sw")
    tkgrid(tklabel(top, text="Active window grabs focus"), grabFocusCheckBox, sticky="e")
    tkgrid.configure(grabFocusCheckBox, sticky="w")
    tkgrid(tklabel(top, text="Double-click presses OK button"), doubleClickCheckBox, sticky="e")
    tkgrid.configure(doubleClickCheckBox, sticky="w")
    tkgrid(tklabel(top, text="Sort variable names alphabetically"), sortNamesCheckBox, sticky="e")
    tkgrid.configure(sortNamesCheckBox, sticky="w")
    tkgrid(tklabel(top, text="Show edit button"), showEditButtonCheckBox, sticky="e")
    tkgrid.configure(showEditButtonCheckBox, sticky="w")
    if (.Platform$OS.type == "windows"){
        tkgrid(tklabel(top, text="Scale factor for Tk elements"), scaleFactorSlider, sticky="se")
        tkgrid.configure(scaleFactorSlider, sticky="w")
        }
    else {
        tkgrid(tklabel(top, text="Default font"), defaultFontEntry, sticky="e")
        tkgrid.configure(defaultFontEntry, sticky="w")
        }
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:8) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
# Graphs menu dialogs

# last modified 20 Mar 04 by J. Fox

indexPlot <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Index Plot")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        type <- if (tclvalue(typeVariable) == "spikes") "h" else "p"
        identify <- tclvalue(identifyVariable) == "1"
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("plot(", .activeDataSet, "$", x, ', type="', type, '")', sep="")
        doItAndPrint(command)
        if (par("usr")[3] <= 0) doItAndPrint('abline(h=0, col="gray")')
        if (identify) {
            command <- paste("identify(", .activeDataSet, "$", x, 
                ", labels=rownames(", .activeDataSet, "))", sep="")
            doItAndPrint(command)
            }        
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(plot)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    optionsFrame <- tkframe(top)
    typeVariable <- tclVar("spikes")
    spikesButton <- tkradiobutton(optionsFrame, variable=typeVariable, value="spikes")
    pointsButton <- tkradiobutton(optionsFrame, variable=typeVariable, value="points")
    identifyVariable <- tclVar("0")
    identifyCheckBox <- tkcheckbutton(optionsFrame, variable=identifyVariable)
    tkgrid(tklabel(top, text="Variable"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="nw")    
    tkgrid(tklabel(optionsFrame, text="Identify observations\nwith mouse", justify="left"), 
        identifyCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Spikes"), spikesButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Points"), pointsButton, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame)
    tkgrid.configure(xScroll, sticky="ns")
    for (row in 0:1) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

Histogram <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Histogram")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        bins <- tclvalue(binsVariable)
        bins <- if (bins == "<auto>") '"Sturges"' else as.numeric(bins)
        scale <- tclvalue(scaleVariable)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("Hist(", .activeDataSet, "$", x, ', scale="',
            scale, '", breaks=', bins, ', col="darkgray")', sep="")
        doItAndPrint(command)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(Hist)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    scaleFrame <- tkframe(top)
    scaleVariable <- tclVar("frequency")
    frequenciesButton <- tkradiobutton(scaleFrame, variable=scaleVariable, value="frequency")
    percentsButton <- tkradiobutton(scaleFrame, variable=scaleVariable, value="percent")
    densitiesButton <- tkradiobutton(scaleFrame, variable=scaleVariable, value="density")
    binsFrame <- tkframe(top)
    binsVariable <- tclVar("<auto>")
    binsField <- tkentry(binsFrame, width="6", textvariable=binsVariable)
    tkgrid(tklabel(top, text="Variable"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="nw")    
    tkgrid(tklabel(scaleFrame, text="Axis Scaling", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(scaleFrame, text="Frequency counts"), frequenciesButton, sticky="w")
    tkgrid(tklabel(scaleFrame, text="Percentages"), percentsButton, sticky="w")
    tkgrid(tklabel(scaleFrame, text="Densities"), densitiesButton, sticky="w")
    tkgrid(tklabel(binsFrame, text="Number of bins: "), binsField, sticky="w")
    tkgrid(binsFrame, sticky="w")
    tkgrid(scaleFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame)
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(binsField, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

stemAndLeaf <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Stem and Leaf Display")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    displayDigits <- tclVar("1")
    onDigits <- function(...){
        tclvalue(displayDigits) <- formatC(10^as.numeric(tclvalue(leafsDigitValue)), 
            format="fg", big.mark=",")
        tclvalue(leafsAutoVariable) <- "0"
        }
    optionsFrame <- tkframe(top)
    partsFrame <- tkframe(top)
    leafsFrame <- tkframe(top)
    styleFrame <- tkframe(top)
    leafsAutoVariable <- tclVar("1")
    leafsDigitCheckBox <- tkcheckbutton(leafsFrame, variable=leafsAutoVariable)
    partsVariable <- tclVar("auto")
    partsAutoButton <- tkradiobutton(partsFrame, variable=partsVariable, value="auto")
    parts1Button <- tkradiobutton(partsFrame, variable=partsVariable, value="1")
    parts2Button <- tkradiobutton(partsFrame, variable=partsVariable, value="2")
    parts5Button <- tkradiobutton(partsFrame, variable=partsVariable, value="5")
    styleVariable <- tclVar("Tukey")
    TukeyButton <- tkradiobutton(styleFrame, variable=styleVariable, value="Tukey")
    bareButton <- tkradiobutton(styleFrame, variable=styleVariable, value="bare")
    trimOutliersVariable <- tclVar("1")
    trimOutliersCheckBox <- tkcheckbutton(optionsFrame, variable=trimOutliersVariable)    
    showDepthsVariable <- tclVar("1")
    showDepthsCheckBox <- tkcheckbutton(optionsFrame, variable=showDepthsVariable)
    reverseNegativeVariable <- tclVar("1")
    reverseNegativeCheckBox <- tkcheckbutton(optionsFrame, variable=reverseNegativeVariable)
    leafsDigitValue <- tclVar("0")
    leafsDigitSlider <- tkscale(leafsFrame, from=-6, to=6, showvalue=FALSE, variable=leafsDigitValue,
        resolution=1, orient="horizontal", command=onDigits)
    leafsDigitShow <- tklabel(leafsFrame, textvariable=displayDigits, width=8, justify="right")
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        unit <- if (tclvalue(leafsAutoVariable) == "1") "" 
            else paste(", unit=", 10^as.numeric(tclvalue(leafsDigitValue)), sep="")
        m <- if (tclvalue(partsVariable) == "auto") ""
            else paste(", m=", tclvalue(partsVariable), sep="")
        trim <- if (tclvalue(trimOutliersVariable) == "1") ""
            else ", trim.outliers=FALSE"
        depths <- if (tclvalue(showDepthsVariable) == "1") ""
            else ", depths=FALSE"
        reverse <- if (tclvalue(reverseNegativeVariable) == "1") ""
            else ", reverse.negative.leaves=FALSE"
        style <- if (tclvalue(styleVariable) == "Tukey") ""
            else ', style="bare"'
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("stem.leaf(", .activeDataSet, "$", x, style, unit, m, trim, 
            depths, reverse, ")", sep="")
        logger(command)
        justDoIt(command)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }    
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(stem.leaf)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="nw")
    tkgrid(tklabel(leafsFrame, text="Leafs Digit:  ", fg="blue"),
        tklabel(leafsFrame, text="Automatic"), leafsDigitCheckBox,
        tklabel(leafsFrame, text="  or set:", fg="red"), leafsDigitShow, leafsDigitSlider, sticky="w")  
    tkgrid(leafsFrame, sticky="w") 
    tkgrid(tklabel(partsFrame, text="Parts Per Stem:  ", fg="blue"),
        tklabel(partsFrame, text="Automatic"), partsAutoButton,
        tklabel(partsFrame, text="  1"), parts1Button, 
        tklabel(partsFrame, text="  2"), parts2Button, 
        tklabel(partsFrame, text="  5"), parts5Button, sticky="w")
    tkgrid(partsFrame, sticky="w")
    tkgrid(tklabel(styleFrame, text="Style of Divided Stems:  ", fg="blue"),
        tklabel(styleFrame, text="Tukey"), TukeyButton,
        tklabel(styleFrame, text="  Repeated stem digits"), bareButton, sticky="w")
    tkgrid(styleFrame, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Trim outliers"), trimOutliersCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Show depths"), showDepthsCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Reverse negative leaves"), reverseNegativeCheckBox, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    tclvalue(leafsAutoVariable) <- "1"
    for (row in 0:6) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

boxPlot <- function(){
    env <- environment()
    .groupsLabel <- tclVar("Plot by groups")
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Boxplot")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    identifyVariable <- tclVar("0")
    identifyFrame <- tkframe(top)
    identifyCheckBox <- tkcheckbutton(identifyFrame, variable=identifyVariable)
    .groups <- FALSE
    onOK <- function(){
        x <- as.character(tkget(xBox, "active"))
        identifyPoints <- "1" == tclvalue(identifyVariable)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        var <- paste(.activeDataSet, "$", x, sep="")
        if (.groups == FALSE) {
            command <- (paste("boxplot(", var, ', ylab="', x, '")', sep=""))
            logger(command)
            justDoIt(command)     
            if (identifyPoints) doItAndPrint(paste("identify(rep(1, length(", var,
                ")), ", var, ", rownames(", .activeDataSet,"))", sep=""))           
            }
        else {
            command <- (paste("boxplot(", x, "~", .groups, ', ylab="', x, 
                '", xlab="', .groups,'"',
                ", data=", .activeDataSet, ")", sep=""))
            logger(command)
            justDoIt(command)
            if (identifyPoints) doItAndPrint(paste("identify(", .activeDataSet, "$", .groups, ", ", var,
                ", rownames(", .activeDataSet,"))", sep=""))
            }
        tkfocus(.commander)
        }
    onGroups <- function(){
        if (length(.factors) == 0){
            tkmessageBox(message="There no factors in the active data set.", 
                    icon="error", type="ok")
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            return()
            }
        subdialog <- tktoplevel()
        tkwm.title(subdialog, "Groups")
        groupsFrame <- tkframe(subdialog)
        groupsBox <- tklistbox(groupsFrame, height=min(4, length(.factors)),
            selectmode="single", background="white", exportselection="FALSE")
        groupsScroll <- tkscrollbar(groupsFrame, repeatinterval=5, command=function(...) tkyview(groupsBox, ...))
        tkconfigure(groupsBox, yscrollcommand=function(...) tkset(groupsScroll, ...))
        for (groups in .factors) tkinsert(groupsBox, "end", groups)
        onOKsub <- function() {
            groups <- as.character(tkget(groupsBox, "active"))
            assign(".groups", groups, envir=env)
            tclvalue(.groupsLabel) <- paste("Plot by:", groups)
            tkconfigure(groupsButton, fg="blue")
            if (.grab.focus) tkgrab.release(subdialog)
            tkdestroy(subdialog)
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        onCancelSub <- function() {
            assign(".groups", FALSE, envir=env)
            tclvalue(.groupsLabel) <- "Plot by groups"
            tkconfigure(groupsButton, fg="black")
            if (.grab.focus) tkgrab.release(subdialog) 
            tkdestroy(subdialog) 
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        subButtonFrame <- tkframe(subdialog)
        OKSubButton <- tkbutton(subButtonFrame, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
        cancelSubButton <- tkbutton(subButtonFrame, text="Cancel", fg="red", width="12",command=onCancelSub)
        tkgrid(tklabel(subdialog, text="Groups variable (pick one)"), sticky="w")
        tkgrid(groupsBox, groupsScroll, sticky="nw")
        tkgrid(groupsFrame, sticky="w")
        tkgrid(OKSubButton, cancelSubButton, sticky="w")
        tkgrid(subButtonFrame, sticky="w")
        tkgrid.configure(groupsScroll, sticky="ns")
        for (row in 0:2) tkgrid.rowconfigure(subdialog, row, weight=0)
        for (col in 0:0) tkgrid.columnconfigure(subdialog, col, weight=0)
        .Tcl("update idletasks")
        tkwm.resizable(subdialog, 0, 0)
        tkbind(subdialog, "<Return>", onOKsub)
        if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOKsub)
        tkbind(groupsBox, "<Double-ButtonPress-1>", onOKsub)
        tkselection.set(groupsBox, 0)
        tkwm.deiconify(subdialog)
        if (.grab.focus) tkgrab.set(subdialog)
        tkfocus(subdialog)
        tkwait.window(subdialog)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(boxplot)
        }
    helpButton <- tkbutton(buttonFrame, text="Help", width="12", command=onHelp)
    groupsButton <- tkbutton(top, textvariable=.groupsLabel, command=onGroups)
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")    
    tkgrid(tklabel(identifyFrame, text="Identify outliers\nwith mouse", justify="left"), 
        identifyCheckBox, sticky="w")
    tkgrid(identifyFrame, stick="w")
    tkgrid(groupsButton, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonFrame, text="    "), helpButton,sticky="w")
    tkgrid(buttonFrame, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

scatterPlot <- function(){
    env <- environment()
    .groupsLabel <- tclVar("Plot by groups")
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) < 2){
        tkmessageBox(message="There are fewer than 2 numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Scatterplot")
    xFrame <- tkframe(top)
    yFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    yBox <- tklistbox(yFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    yScroll <- tkscrollbar(yFrame, repeatinterval=5, command=function(...) tkyview(yBox, ...))    
    tkconfigure(yBox, yscrollcommand=function(...) tkset(yScroll, ...))
    for (y in .numeric) tkinsert(yBox, "end", y)
    optionsFrame <- tkframe(top)
    identifyVariable <- tclVar("0")
    identifyCheckBox <- tkcheckbutton(optionsFrame, variable=identifyVariable)
    jitterXVariable <- tclVar("0")
    jitterXCheckBox <- tkcheckbutton(optionsFrame, variable=jitterXVariable)
    jitterYVariable <- tclVar("0")
    jitterYCheckBox <- tkcheckbutton(optionsFrame, variable=jitterYVariable)
    boxplots <- tclVar("1")
    boxplotsCheckBox <- tkcheckbutton(optionsFrame, variable=boxplots)
    lsLine <- tclVar("1")
    lsLineCheckBox <- tkcheckbutton(optionsFrame, variable=lsLine)
    smoothLine <- tclVar("1")
    smoothCheckBox <- tkcheckbutton(optionsFrame, variable=smoothLine)
    sliderValue <- tclVar("50")
    slider <- tkscale(optionsFrame, from=0, to=100, showvalue=TRUE, variable=sliderValue,
        resolution=5, orient="horizontal")
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    .groups <- FALSE
    .linesByGroup <- FALSE
    onOK <- function(){
        x <- as.character(tkget(xBox, "active"))
        y <- as.character(tkget(yBox, "active"))
        if (x == y) {
            tkmessageBox(message="x and y variables must be different", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            scatterPlot()
            return()
            }
        if ("1" == tclvalue(jitterXVariable)) x <- paste("jitter(", x, ")", sep="")
        if ("1" == tclvalue(jitterYVariable)) y <- paste("jitter(", y, ")", sep="")
        labels <- if("1" == tclvalue(identifyVariable)) 
            paste("rownames(", .activeDataSet, ")", sep="") else "FALSE"
        box <- if ("1" == tclvalue(boxplots)) "'xy'" else "FALSE"
        line <- if("1" == tclvalue(lsLine)) "lm" else "FALSE"
        smooth <- as.character("1" == tclvalue(smoothLine))
        span <- as.numeric(tclvalue(sliderValue))
        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == "<all valid cases>") "" 
            else paste(", subset=", subset, sep="")
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)
        if (.groups == FALSE) {
            doItAndPrint(paste("scatterplot(", y, "~", x,
                ", reg.line=", line, ", smooth=", smooth, ", labels=", labels,
                ", boxplots=", box, ", span =", span/100,
                ", data=", .activeDataSet, subset, ")", sep=""))
            }
        else {
            doItAndPrint(paste("scatterplot(", y, "~", x," | ", .groups,
                ", reg.line=", line, ", smooth=", smooth, ", labels=", labels,
                ", boxplots=", box, ", span=", span/100,
                ", by.groups=", .linesByGroup,
                ", data=", .activeDataSet, subset, ")", sep=""))
            }
        }
    onGroups <- function(){
        if (length(.factors) == 0){
            tkmessageBox(message="There are no factors in the active data set.", 
                    icon="error", type="ok")
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            return()
            }
        subdialog <- tktoplevel()
        tkwm.title(subdialog, "Groups")
        linesByGroupFrame <- tkframe(subdialog)
        linesButtonFrame <- tkframe(subdialog)
        groupsFrame <- tkframe(subdialog)
        groupsBox <- tklistbox(groupsFrame, height=min(4, length(.factors)),
            selectmode="single", background="white", exportselection="FALSE")
        groupsScroll <- tkscrollbar(groupsFrame, repeatinterval=5, command=function(...) tkyview(groupsBox, ...))
        tkconfigure(groupsBox, yscrollcommand=function(...) tkset(groupsScroll, ...))
        for (groups in .factors) tkinsert(groupsBox, "end", groups)
        linesByGroup <- tclVar("1")
        linesCheckBox <- tkcheckbutton(linesByGroupFrame, variable=linesByGroup)
        onOKsub <- function(){
            groups <- as.character(tkget(groupsBox, "active"))
            assign(".groups", groups, envir=env)
            tclvalue(.groupsLabel) <- paste("Plot by:", groups)
            tkconfigure(groupsButton, fg="blue")
            lines <- as.character("1" == tclvalue(linesByGroup))
            assign(".linesByGroup", lines, envir=env)
            if (.grab.focus) tkgrab.release(subdialog)
            tkdestroy(subdialog)
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        onCancelSub <- function() {
            assign(".groups", FALSE, envir=env)
            assign(".linesByGroup", FALSE, envir=env)
            tclvalue(.groupsLabel) <- "Plot by groups"
            tkconfigure(groupsButton, fg="black")
            if (.grab.focus) tkgrab.release(subdialog)  
            tkdestroy(subdialog)
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        OKSubButton <- tkbutton(linesButtonFrame, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
        cancelSubButton <- tkbutton(linesButtonFrame, text="Cancel", fg="red", width="12", command=onCancelSub)
        tkgrid(tklabel(subdialog, text="Groups (pick one)"), sticky="w")
        tkgrid(groupsBox, groupsScroll, sticky="nw")
        tkgrid(groupsFrame, sticky="w")
        tkgrid(tklabel(linesByGroupFrame, text="Plot lines by group"), linesCheckBox, sticky="w")
        tkgrid(linesByGroupFrame, sticky="w")
        tkgrid(OKSubButton, cancelSubButton, sticky="w")
        tkgrid(linesButtonFrame, sticky="w")
        tkgrid(tklabel(subdialog, text="Position legend with mouse click", fg="blue"))
        tkgrid.configure(groupsScroll, sticky="ns")
        for (row in 0:4) tkgrid.rowconfigure(subdialog, row, weight=0)
        for (col in 0:0) tkgrid.columnconfigure(subdialog, col, weight=0)
        .Tcl("update idletasks")
        tkwm.resizable(subdialog, 0, 0)
        tkbind(subdialog, "<Return>", onOKsub)
        if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOKsub)
        tkselection.set(groupsBox, 0)
        tkwm.deiconify(subdialog)
        if (.grab.focus) tkgrab.set(subdialog)
        tkfocus(subdialog)
        tkwait.window(subdialog)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(scatterplot)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    groupsButton <- tkbutton(top, textvariable=.groupsLabel, command=onGroups)
    tkgrid(tklabel(top, text="x-variable (pick one)"), 
        tklabel(top, text="y-variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(yBox, yScroll, sticky="nw")
    tkgrid(xFrame, yFrame, sticky="w")    
    tkgrid(tklabel(optionsFrame, text="Identify points"), identifyCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Jitter x variable"), jitterXCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Jitter y variable"), jitterYCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Marginal boxplots"), boxplotsCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Least-squares line"), lsLineCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Smooth line"), smoothCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Span for smooth"), slider, sticky="w")
    tkgrid(optionsFrame)
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(groupsButton, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(yScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkselection.set(yBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

scatterPlotMatrix <- function(){
    env <- environment()
    .groupsLabel <- tclVar("Plot by groups")
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) < 3){
        tkmessageBox(message="There are fewer than 3 numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Scatterplot Matrix")
    variablesFrame <- tkframe(top)
    variablesBox <- tklistbox(variablesFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE")
    variablesScroll <- tkscrollbar(variablesFrame, 
        repeatinterval=5, command=function(...) tkyview(variablesBox, ...))    
    tkconfigure(variablesBox, yscrollcommand=function(...) tkset(variablesScroll, ...))
    for (variable in .numeric) tkinsert(variablesBox, "end", variable)
    optionsFrame <- tkframe(top)
    lsLine <- tclVar("1")
    lsLineCheckBox <- tkcheckbutton(optionsFrame, variable=lsLine)
    smoothLine <- tclVar("1")
    smoothCheckBox <- tkcheckbutton(optionsFrame, variable=smoothLine)
    sliderValue <- tclVar("50")
    slider <- tkscale(optionsFrame, from=0, to=100, showvalue=TRUE, variable=sliderValue,
        resolution=5, orient="horizontal")
    diagonalFrame <- tkframe(top)
    diagonal <- tclVar("density")
    histogramButton <- tkradiobutton(diagonalFrame, variable=diagonal, value="histogram")
    densityButton <- tkradiobutton(diagonalFrame, variable=diagonal, value="density")
    boxplotButton <- tkradiobutton(diagonalFrame, variable=diagonal, value="boxplot")
    qqplotButton <- tkradiobutton(diagonalFrame, variable=diagonal, value="qqplot")
    noneButton <- tkradiobutton(diagonalFrame, variable=diagonal, value="none")
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    assign(".groups", "FALSE", envir=.GlobalEnv)
    assign(".linesByGroup", "FALSE", envir=.GlobalEnv)
    onOK <- function(){
        variables <- .numeric[as.numeric(tkcurselection(variablesBox)) + 1]
        if (length(variables) < 3) {
            tkmessageBox(message="Fewer than 3 variable selected.", 
                icon="error", type="ok")
            tkdestroy(top)
            scatterPlotMatrix()
            return()
            }
        line <- if("1" == tclvalue(lsLine)) "lm" else "FALSE"
        smooth <- as.character("1" == tclvalue(smoothLine))
        span <- as.numeric(tclvalue(sliderValue))
        diag <- as.character(tclvalue(diagonal))
        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == "<all valid cases>") "" 
            else paste(", subset=", subset, sep="")
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        if (.groups == FALSE) {
           command <- paste("scatterplot.matrix(~", paste(variables, collapse="+"),
                ", reg.line=", line, ", smooth=", smooth,
                ", span=", span/100, ", diagonal = '", diag,
                "', data=", .activeDataSet, subset, ")", sep="")
           logger(command)
           justDoIt(command)
            }
        else {
            command <- paste("scatterplot.matrix(~", paste(variables, collapse="+")," | ", .groups,
                ", reg.line=", line, ", smooth=", smooth,
                ", span=", span/100, ", diagonal= '", diag,
                "', by.groups=", .linesByGroup,
                ", data=", .activeDataSet, subset, ")", sep="")
            logger(command)
            justDoIt(command)
            }
        tkfocus(.commander)
        }
    onGroups <- function(){
        if (length(.factors) == 0){
            tkmessageBox(message="There are no factors in the active data set.", 
                    icon="error", type="ok")
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            return()
            }
        subdialog <- tktoplevel()
        tkwm.title(subdialog, "Groups")
        linesByGroupFrame <- tkframe(subdialog)
        linesButtonFrame <- tkframe(subdialog)
        groupsFrame <- tkframe(subdialog)
        groupsBox <- tklistbox(groupsFrame, height=min(4, length(.factors)),
            selectmode="single", background="white", exportselection="FALSE")
        groupsScroll <- tkscrollbar(groupsFrame, repeatinterval=5, command=function(...) tkyview(groupsBox, ...))
        tkconfigure(groupsBox, yscrollcommand=function(...) tkset(groupsScroll, ...))
        for (groups in .factors) tkinsert(groupsBox, "end", groups)
        linesByGroup <- tclVar("1")
        linesCheckBox <- tkcheckbutton(linesByGroupFrame, variable=linesByGroup)
        onOKsub <- function(){
            groups <- as.character(tkget(groupsBox, "active"))
            assign(".groups", groups, envir=env)
            tclvalue(.groupsLabel) <- paste("Plot by:", groups)
            tkconfigure(groupsButton, fg="blue")
            lines <- as.character("1" == tclvalue(linesByGroup))
            assign(".linesByGroup", lines, envir=env)
            if (.grab.focus) tkgrab.release(subdialog)
            tkdestroy(subdialog)
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        onCancelSub <- function() {
            assign(".groups", FALSE, envir=env)
            tclvalue(.groupsLabel) <- "Plot by groups"
            tkconfigure(groupsButton, fg="black")
            if (.grab.focus) tkgrab.release(subdialog)  
            tkdestroy(subdialog)
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        OKSubButton <- tkbutton(linesButtonFrame, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
        cancelSubButton <- tkbutton(linesButtonFrame, text="Cancel", fg="red", width="12", command=onCancelSub)
        tkselection.set(groupsBox, 0)
        tkgrid(tklabel(subdialog, text="Groups (pick one)"), sticky="w")
        tkgrid(groupsBox, groupsScroll, sticky="nw")
        tkgrid(groupsFrame, sticky="w")
        tkgrid(tklabel(linesByGroupFrame, text="Plot lines by group"), linesCheckBox, sticky="w")
        tkgrid(linesByGroupFrame, sticky="w")
        tkgrid(OKSubButton, cancelSubButton, sticky="w")
        tkgrid(linesButtonFrame, sticky="w")
        tkgrid.configure(groupsScroll, sticky="ns")
        for (row in 0:3) tkgrid.rowconfigure(subdialog, row, weight=0)
        for (col in 0:0) tkgrid.columnconfigure(subdialog, col, weight=0)
        .Tcl("update idletasks")
        tkwm.resizable(subdialog, 0, 0)
        tkbind(subdialog, "<Return>", onOKsub)
        if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOKsub)
        tkwm.deiconify(subdialog)
        if (.grab.focus) tkgrab.set(subdialog)
        tkfocus(subdialog)
        tkwait.window(subdialog)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(scatterplot.matrix)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    groupsButton <- tkbutton(top, textvariable=.groupsLabel, command=onGroups)
    tkgrid(tklabel(top, text="Select variables (three or more)"), sticky="w")
    tkgrid(variablesBox, variablesScroll, sticky="nw")
    tkgrid(variablesFrame, sticky="nw")    
    tkgrid(tklabel(optionsFrame, text="Least-squares line"), lsLineCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Smooth line"), smoothCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Span for smooth"), slider, sticky="w")
    tkgrid(optionsFrame)
    tkgrid(tklabel(diagonalFrame, text="On Diagonal:", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(diagonalFrame, text="Density plots"), densityButton, sticky="w")
    tkgrid(tklabel(diagonalFrame, text="Histograms"), histogramButton, sticky="w")
    tkgrid(tklabel(diagonalFrame, text="Boxplots"), boxplotButton, sticky="w")
    tkgrid(tklabel(diagonalFrame, text="Normal QQ plots"), qqplotButton, sticky="w")
    tkgrid(tklabel(diagonalFrame, text="Nothing (empty)"), noneButton, sticky="w")
    tkgrid(diagonalFrame, sticky="w")
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(groupsButton, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    for (row in 0:6) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkgrid.configure(variablesScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

barGraph <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.factors) == 0){
        tkmessageBox(message="There are no factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Bar Graph")
    variableFrame <- tkframe(top)
    variableBox <- tklistbox(variableFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    variableScroll <- tkscrollbar(variableFrame, repeatinterval=5, 
        command=function(...) tkyview(variableBox, ...))
    tkconfigure(variableBox, yscrollcommand=function(...) tkset(variableScroll, ...))
    for (var in .factors) tkinsert(variableBox, "end", var)
    onOK <- function(){
        variable <- as.character(tkget(variableBox, "active"))
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("barplot(table(", .activeDataSet, "$", variable, '), xlab="',
            variable, '", ylab="Frequency")', sep="")
        logger(command)
        justDoIt(command)
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(barplot)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(variableBox, variableScroll, sticky="nw")
    tkgrid(variableFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(variableScroll, sticky="ns")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(variableBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkbind(variableBox, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

pieChart <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.factors) == 0){
        tkmessageBox(message="There are no factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Pie Chart")
    variableFrame <- tkframe(top)
    variableBox <- tklistbox(variableFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    variableScroll <- tkscrollbar(variableFrame, repeatinterval=5, 
        command=function(...) tkyview(variableBox, ...))
    tkconfigure(variableBox, yscrollcommand=function(...) tkset(variableScroll, ...))
    for (var in .factors) tkinsert(variableBox, "end", var)
    onOK <- function(){
        variable <- as.character(tkget(variableBox, "active"))
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- (paste("pie(table(", .activeDataSet, "$", variable, "), labels=levels(",
            .activeDataSet, "$", variable, '), main="', variable, '", col=rainbow(length(levels(',
            .activeDataSet, "$", variable, "))))", sep=""))
        logger(command)
        justDoIt(command)
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(pie)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(variableBox, variableScroll, sticky="nw")
    tkgrid(variableFrame, sticky="w")
    tkgrid.configure(variableScroll, sticky="ns")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(variableBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkbind(variableBox, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

linePlot <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) < 2){
        tkmessageBox(message="There are fewer than 2 numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Line Plot")
    variablesFrame <- tkframe(top)
    xFrame <- tkframe(variablesFrame)
    yFrame <- tkframe(variablesFrame)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    yBox <- tklistbox(yFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE")
    yScroll <- tkscrollbar(yFrame, repeatinterval=5, command=function(...) tkyview(yBox, ...))    
    tkconfigure(yBox, yscrollcommand=function(...) tkset(yScroll, ...))
    for (y in .numeric) tkinsert(yBox, "end", y)
    axisLabelVariable <- tclVar("<use y-variable names>")
    axisLabelFrame <- tkframe(top)
    axisLabelEntry <- tkentry(axisLabelFrame, width="40", textvariable=axisLabelVariable)
    axisLabelScroll <- tkscrollbar(axisLabelFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(axisLabelEntry, ...))
    tkconfigure(axisLabelEntry, xscrollcommand=function(...) tkset(axisLabelScroll, ...))
    legendFrame <- tkframe(top)
    legendVariable <- tclVar("0")
    legendCheckBox <- tkcheckbutton(legendFrame, variable=legendVariable)
    onOK <- function(){
        y <- .numeric[as.numeric(tkcurselection(yBox)) + 1]
        x <- as.character(tkget(xBox, "active"))
        if (0 == length(y)) {
            tkmessageBox(message="No y variables selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            linePlot()
            return()
            }
        if (is.element(x, y)) {
            tkmessageBox(message="x and y variables must be different.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            linePlot()
            return()
            }
        .x <- na.omit(eval(parse(text=paste(.activeDataSet, "$", x, sep="")), envir=.GlobalEnv))
        if (!identical(order(.x), seq(along=.x))){
            response <- tclvalue(tkmessageBox(message="x-values are not in order.\nContinue?", 
                icon="warning", type="okcancel", default="cancel"))
            if (response == "cancel") {
                onCancel()
                return()
                }
            }
        axisLabel <- tclvalue(axisLabelVariable)
        legend <- tclvalue(legendVariable) == "1"
        if (axisLabel == "<use y-variable names>"){
            axisLabel <- if (legend) ""
                else if(length(y) == 1) y
                else paste(paste("(", 1:length(y), ") ", y, sep=""), collapse=", ")
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        pch <- if (length(y) == 1) ", pch=1" else ""
        command <- paste("matplot(", .activeDataSet, "$", x, ", ", .activeDataSet, "[, ",
            paste("c(", paste(paste('"', y, '"', sep=""), collapse=","), ")", sep=""),
            '], type="b", lty=1, ylab="', axisLabel, '"', pch, ")", sep="")
        logger(command)
        justDoIt(command)
        if (legend && length(y) > 1){
            n <- length(y)
            cols <- rep(1:6, 1 + n %/% 6)[1:n]
            command <- paste("legend(locator(1), legend=", 
                paste("c(", paste(paste('"', y, '"', sep=""), collapse=","), ")", sep=""),
                ", col=c(", paste(cols, collapse=","), "), lty=1, pch=c(",
                paste(paste('"', as.character(1:n), '"', sep=""), collapse=","), "))", sep="")
            logger(command)
            justDoIt(command)
            }
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(matplot)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(variablesFrame, text="x variable (pick one)"), 
        tklabel(variablesFrame, text="    "),
        tklabel(variablesFrame, text="y variables (pick one or more)"), sticky="w")
    tkgrid(yBox, yScroll, sticky="nw")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, tklabel(variablesFrame, text="    "), yFrame, sticky="nw")
    tkgrid(variablesFrame, sticky="w")    
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="            "), 
        helpButton, sticky="w")
    tkgrid(tklabel(axisLabelFrame, text="Label for y-axis"), sticky="w")
    tkgrid(axisLabelEntry, sticky="w")
    tkgrid(axisLabelScroll, sticky="ew")
    tkgrid(axisLabelFrame, sticky="w")
    tkgrid(tklabel(legendFrame, text="Plot legend (position with mouse click)"),
        legendCheckBox, sticky="w")
    tkgrid(legendFrame, sticky="w")
    tkgrid(buttonsFrame, stick="w")
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(yScroll, sticky="ns")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
#    if (.grab.focus) tkgrab.set(top)  # causes problem ?
    tkfocus(top)
    tkwait.window(top)
    }
    
QQPlot <- function()
# this function modified by Martin Maechler
{
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Quantile-Comparison (QQ) Plot")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
                      selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        dist <- tclvalue(distVariable)
        save <- options(warn=-1)
        on.exit(options=save)
        retryMe <- function(msg) {
            tkmessageBox(message= msg, icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            QQPlot()
        }
        switch(dist,
               "norm" = { args <- 'dist= "norm"' },
               "t" =  {
                   df <- tclvalue(tDfVariable)
                   df.num <- as.numeric(df)
                   if (is.na(df.num) || df.num < 1) {
                       retryMe("df for t must be a positive number.")
                       return()
                   }
                   args <- paste('dist="t", df=', df, sep="")
               },
               "chisq" = {
                   df <- tclvalue(chisqDfVariable)
                   df.num <- as.numeric(df)
                   if (is.na(df.num) || df.num < 1) {
                       retryMe("df for chi-square must be a positive number.")
                       return()
                   }
                   args <- paste('dist="chisq", df=', df, sep="")
               },
               "f" = {
                   df1 <- tclvalue(FDf1Variable)
                   df2 <- tclvalue(FDf2Variable)
                   df.num1 <- as.numeric(df1)
                   df.num2 <- as.numeric(df2)
                   if (is.na(df.num1) || df.num1 < 1 ||
                       is.na(df.num2) || df.num2 < 1) {
                       retryMe("numerator and denominator \ndf for F must be positive numbers.")
                       return()
                   }
                   args <- paste('dist="f", df1=', df1, ', df2=', df2, sep="")
               },
               ## else -- other `dist' :
           {
               dist <- tclvalue(otherNameVariable)
               params <- tclvalue(otherParamsVariable)
               args <- paste('dist="', dist,'", ', params, sep="")
           }) # end{switch}
        labels <-
            if ("1" == tclvalue(identifyVariable))
                paste("rownames(", .activeDataSet, ")", sep="")
            else "FALSE"
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("qq.plot", "(", .activeDataSet, "$", x, ", ", args,
                          ", labels=", labels, ")", sep="")
        doItAndPrint(command)
        tkfocus(.commander)
    }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",
                             command=onCancel)
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12",
                           command=function()help(qq.plot))
    distFrame <- tkframe(top)
    distVariable <- tclVar("norm")
    normalButton <- tkradiobutton(distFrame, variable=distVariable, value="norm")
    tButton <- tkradiobutton(distFrame, variable=distVariable, value="t")
    chisqButton <- tkradiobutton(distFrame, variable=distVariable, value="chisq")
    FButton <- tkradiobutton(distFrame, variable=distVariable, value="f")
    otherButton <- tkradiobutton(distFrame, variable=distVariable, value="other")
    tDfFrame <- tkframe(distFrame)
    tDfVariable <- tclVar("")
    tDfField <- tkentry(tDfFrame, width="6", textvariable=tDfVariable)
    chisqDfFrame <- tkframe(distFrame)
    chisqDfVariable <- tclVar("")
    chisqDfField <- tkentry(chisqDfFrame, width="6", textvariable=chisqDfVariable)
    FDfFrame <- tkframe(distFrame)
    FDf1Variable <- tclVar("")
    FDf1Field <- tkentry(FDfFrame, width="6", textvariable=FDf1Variable)
    FDf2Variable <- tclVar("")
    FDf2Field <- tkentry(FDfFrame, width="6", textvariable=FDf2Variable)
    otherParamsFrame <- tkframe(distFrame)
    otherParamsVariable <- tclVar("")
    otherParamsField <- tkentry(otherParamsFrame, width="30", textvariable=otherParamsVariable)
    otherNameVariable <- tclVar("")
    otherNameField <- tkentry(otherParamsFrame, width="10", textvariable=otherNameVariable)
    identifyVariable <- tclVar("0")
    identifyFrame <- tkframe(top)
    identifyCheckBox <- tkcheckbutton(identifyFrame, variable=identifyVariable)
    tkgrid(tklabel(top, text="Variable"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="nw")
    tkgrid(tklabel(identifyFrame, text="Identify observations\nwith mouse", justify="left"),
           identifyCheckBox, sticky="w")
    tkgrid(identifyFrame, sticky="w")
    tkgrid(tklabel(distFrame, text="Distribution", fg="blue"), columnspan=6, sticky="w")
    tkgrid(tklabel(distFrame, text="Normal"), normalButton, sticky="w")
    tkgrid(tklabel(tDfFrame, text="df = "), tDfField, sticky="w")
    tkgrid(tklabel(distFrame, text="t"), tButton, tDfFrame, sticky="w")
    tkgrid(tklabel(chisqDfFrame, text="df = "), chisqDfField, sticky="w")
    tkgrid(tklabel(distFrame, text="Chi-square"), chisqButton,
           chisqDfFrame, sticky="w")
    tkgrid(tklabel(FDfFrame, text="Numerator df = "), FDf1Field,
           tklabel(FDfFrame, text="Denominator df = "), FDf2Field, sticky="w")
    tkgrid(tklabel(distFrame, text="F"), FButton, FDfFrame, sticky="w")
    tkgrid(tklabel(otherParamsFrame, text="Specify: "),
           otherNameField, tklabel(otherParamsFrame, text="Parameters: "),
           otherParamsField, sticky="w")
    tkgrid(tklabel(distFrame, text="Other"), otherButton,
           otherParamsFrame, sticky="w")
    tkgrid(distFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

PlotMeans <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (length(.factors) == 0){
        tkmessageBox(message="There no factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Plot Means")
    groupFrame <- tkframe(top)
    responseFrame <- tkframe(top)
    groupBox <- tklistbox(groupFrame, height=min(4, length(.factors)),
        selectmode="multiple", background="white", exportselection="FALSE")
    groupScroll <- tkscrollbar(groupFrame, repeatinterval=5, 
        command=function(...) tkyview(groupBox, ...))
    tkconfigure(groupBox, yscrollcommand=function(...) tkset(groupScroll, ...))
    for (group in .factors) tkinsert(groupBox, "end", group)
    responseBox <- tklistbox(responseFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    responseScroll <- tkscrollbar(responseFrame, repeatinterval=5, 
        command=function(...) tkyview(responseBox, ...))    
    tkconfigure(responseBox, yscrollcommand=function(...) tkset(responseScroll, ...))
    for (response in .numeric) tkinsert(responseBox, "end", response)
    onOK <- function(){
        groups <- .factors[as.numeric(tkcurselection(groupBox)) + 1]
        response <- as.character(tkget(responseBox, "active"))
        if (0 == length(groups)) {
            tkmessageBox(message="No factors selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            PlotMeans()
            return()
            }
        if (2 < length(groups)) {
            tkmessageBox(message="More than two factors selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            PlotMeans()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        error.bars <- tclvalue(errorBarsVariable)
        level <- if (error.bars == "conf.int") paste(", level=", tclvalue(levelVariable), sep="") else ""
        if (length(groups) == 1) doItAndPrint(paste("plotMeans(", .activeDataSet, "$", response, 
            ", ", .activeDataSet, "$", groups[1], 
            ', error.bars="', error.bars, '"', level, ')', sep=""))
        else {
            if (eval(parse(text=paste("length(levels(", .activeDataSet, "$", groups[1], 
                ")) < length(levels(", .activeDataSet, "$", groups[2], "))", sep=""))))
                groups <- rev(groups)
            doItAndPrint(paste("plotMeans(", .activeDataSet, "$", response, ", ", .activeDataSet, "$", groups[1], 
                ", ", .activeDataSet, "$", groups[2], ', error.bars="', error.bars, '"', level, ')', sep=""))
            }
        tkfocus(.commander)
        }
    optionsFrame <- tkframe(top)
    errorBarsVariable <- tclVar("se")
    seButton <- tkradiobutton(optionsFrame, variable=errorBarsVariable, value="se")
    sdButton <- tkradiobutton(optionsFrame, variable=errorBarsVariable, value="sd")
    confIntButton <- tkradiobutton(optionsFrame, variable=errorBarsVariable, value="conf.int")
    noneButton <- tkradiobutton(optionsFrame, variable=errorBarsVariable, value="none")
    levelVariable <- tclVar("0.95")
    levelEntry <- tkentry(optionsFrame, width="6", textvariable=levelVariable)    
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }  
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(plotMeans)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Factors (pick one or two)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid(groupFrame, responseFrame, sticky="nw")
    tkgrid(tklabel(optionsFrame, text="Error Bars", fg="blue"), sticky="w")
    tkgrid(tklabel(optionsFrame, text="Standard errors"), seButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Standard deviations"), sdButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Confidence intervals"), confIntButton,
        tklabel(optionsFrame, text="   Level of confidence:"), levelEntry, sticky="w")
    tkgrid(tklabel(optionsFrame, text="No error bars"), noneButton, sticky="w")
    tkgrid(optionsFrame, sticky="w", columnspan=2)
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

Scatter3D <- function(){
    env <- environment()
    .groupsLabel <- tclVar("Plot by groups")
    rgl <- require(rgl)
    mgcv <- require(mgcv)
    absent <- !c(rgl, mgcv)
    if (any(absent)) {
        tkmessageBox(message= paste(
            "The following packages required for 3D scatterplots are missing:\n",
            paste(c("rgl", "mgcv")[absent], collapse=", ")), icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) < 3){
        tkmessageBox(message="There are fewer than 3 numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "3D Scatterplot")
    variablesFrame <- tkframe(top)
    xFrame <- tkframe(variablesFrame)
    yFrame <- tkframe(variablesFrame)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    yBox <- tklistbox(yFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    yScroll <- tkscrollbar(yFrame, repeatinterval=5, command=function(...) tkyview(yBox, ...))    
    tkconfigure(yBox, yscrollcommand=function(...) tkset(yScroll, ...))
    for (y in .numeric) tkinsert(yBox, "end", y)
    surfacesFrame <- tkframe(top)
    gridLines <- tclVar("1")
    gridLinesCheckBox <- tkcheckbutton(surfacesFrame, variable=gridLines)
    linearLSSurface <- tclVar("1")
    linearLSCheckBox <- tkcheckbutton(surfacesFrame, variable=linearLSSurface)
    quadLSSurface <- tclVar("0")
    quadLSCheckBox <- tkcheckbutton(surfacesFrame, variable=quadLSSurface)
    nonparSurface <- tclVar("0")
    nonparCheckBox <- tkcheckbutton(surfacesFrame, variable=nonparSurface)
    dfNonparVariable <- tclVar("<auto>")
    dfNonparField <- tkentry(surfacesFrame, width="6", textvariable=dfNonparVariable)
    additiveSurface <- tclVar("0")
    additiveCheckBox <- tkcheckbutton(surfacesFrame, variable=additiveSurface)
    dfAddVariable <- tclVar("<auto>")
    dfAddField <- tkentry(surfacesFrame, width="6", textvariable=dfAddVariable)
    bgFrame <- tkframe(top)
    bgVariable <-tclVar("black")
    whiteButton <- tkradiobutton(bgFrame, variable=bgVariable, value="white")
    blackButton <- tkradiobutton(bgFrame, variable=bgVariable, value="black")
    assign(".groups", FALSE, envir=env)
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        y <- as.character(tkget(yBox, "active"))
        if (2 != length(x)) {
            tkmessageBox(message="You must select 2 explanatory variables.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            Scatter3D()
            return()
            }
        if (is.element(y, x)) {
            tkmessageBox(message="Response and explanatory variables must be different.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            Scatter3D()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        grid <- if (tclvalue(gridLines) == 1) "TRUE" else "FALSE"
        lin <- if(tclvalue(linearLSSurface) == 1) '"linear"'
        quad <- if(tclvalue(quadLSSurface) == 1) '"quadratic"'
        nonpar <- if (tclvalue(nonparSurface) == 1) '"smooth"'
        additive <- if (tclvalue(additiveSurface) == 1) '"additive"'
        surfaces <- c(lin, quad, nonpar, additive)
        nsurfaces <- length(surfaces)
        dfNonpar <- tclvalue(dfNonparVariable)
        dfNonpar <- if (dfNonpar == "<auto>") "" else paste(", df.smooth=", as.numeric(dfNonpar), sep="")
        dfAdd <- tclvalue(dfAddVariable)
        dfAdd <- if (dfAdd == "<auto>") "" else paste(", df.additive=", as.numeric(dfAdd), sep="")
        fit <- if (nsurfaces == 0) ", surface=FALSE"
            else if (nsurfaces == 1) paste(", fit=", surfaces, sep="")
            else paste(", fit=c(", paste(surfaces, collapse=","), ")", sep="")
        bg <- tclvalue(bgVariable)
        if (.groups != FALSE){ 
            groups <- paste(", groups=", .groups, sep="")
            parallel <- paste(", parallel=", .parallelSurfaces, sep="")
            }
        else groups <- parallel <- ""                   
        command <- paste("scatter3d(", x[1], ", ", y, ", ", x[2], fit, dfNonpar, 
            dfAdd, groups, parallel, ', bg="', bg, '", grid=', grid, ')', sep="")
        doItAndPrint(command)
        assign(".rgl", TRUE, envir=.GlobalEnv)
        tkfocus(.commander)
        }
    onGroups <- function(){
        if (length(.factors) == 0){
            tkmessageBox(message="There no factors in the active data set.", 
                    icon="error", type="ok")
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            return()
            }
        subdialog <- tktoplevel()
        tkwm.title(subdialog, "Groups")
        groupsFrame <- tkframe(subdialog)
        groupsBox <- tklistbox(groupsFrame, height=min(4, length(.factors)),
            selectmode="single", background="white", exportselection="FALSE")
        groupsScroll <- tkscrollbar(groupsFrame, repeatinterval=5, command=function(...) tkyview(groupsBox, ...))
        tkconfigure(groupsBox, yscrollcommand=function(...) tkset(groupsScroll, ...))
        for (groups in .factors) tkinsert(groupsBox, "end", groups)
        parallelSurfacesFrame <- tkframe(subdialog)
        parallelSurfacesVariable <- tclVar("1")
        parallelSurfacesCheckBox <- tkcheckbutton(parallelSurfacesFrame, variable=parallelSurfacesVariable)
        onOKsub <- function() {
            groups <- as.character(tkget(groupsBox, "active"))
            assign(".groups", groups, envir=env)
            tclvalue(.groupsLabel) <- paste("Plot by:", groups)
            tkconfigure(groupsButton, fg="blue")
            assign(".parallelSurfaces", tclvalue(parallelSurfacesVariable) == "1", envir=env)
            if (.grab.focus) tkgrab.release(subdialog)
            tkdestroy(subdialog)
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        onCancelSub <- function() {
            assign(".groups", FALSE, envir=env)
            tclvalue(.groupsLabel) <- "Plot by groups"
            tkconfigure(groupsButton, fg="black")
            if (.grab.focus) tkgrab.release(subdialog) 
            tkdestroy(subdialog) 
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        subButtonFrame <- tkframe(subdialog)
        OKSubButton <- tkbutton(subButtonFrame, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
        cancelSubButton <- tkbutton(subButtonFrame, text="Cancel", fg="red", width="12",command=onCancelSub)
        tkgrid(tklabel(subdialog, text="Groups variable (pick one)"), sticky="w")
        tkgrid(groupsBox, groupsScroll, sticky="nw")
        tkgrid(groupsFrame, sticky="w")
        tkgrid(tklabel(parallelSurfacesFrame, text="Parallel regression surfaces"), parallelSurfacesCheckBox, sticky="w")
        tkgrid(parallelSurfacesFrame, sticky="w")
        tkgrid(OKSubButton, cancelSubButton, sticky="w")
        tkgrid(subButtonFrame, sticky="w")
        tkgrid.configure(groupsScroll, sticky="ns")
        for (row in 0:3) tkgrid.rowconfigure(subdialog, row, weight=0)
        for (col in 0:0) tkgrid.columnconfigure(subdialog, col, weight=0)
        .Tcl("update idletasks")
        tkwm.resizable(subdialog, 0, 0)
        tkbind(subdialog, "<Return>", onOKsub)
        if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOKsub)
        tkbind(groupsBox, "<Double-ButtonPress-1>", onOKsub)
        tkselection.set(groupsBox, 0)
        tkwm.deiconify(subdialog)
        if (.grab.focus) tkgrab.set(subdialog)
        tkfocus(subdialog)
        tkwait.window(subdialog)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help("Scatter3DDialog")
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    groupsButton <- tkbutton(top, textvariable=.groupsLabel, command=onGroups)
    tkgrid(tklabel(variablesFrame, text="Response variable (pick one)"), 
        tklabel(variablesFrame, text="    "),
        tklabel(variablesFrame, text="Explanatory variables (pick two)"), sticky="w")
    tkgrid(yBox, yScroll, sticky="nw")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(yFrame, tklabel(variablesFrame, text="    "), xFrame, sticky="nw")
    tkgrid(variablesFrame, sticky="w")   
    tkgrid(tklabel(surfacesFrame, text="Show surface grid lines"), gridLinesCheckBox, sticky="w")
    tkgrid(tklabel(surfacesFrame, text="Surfaces to Fit", fg="blue"), sticky="w")
    tkgrid(tklabel(surfacesFrame, text="Linear least-squares"), linearLSCheckBox, sticky="w")
    tkgrid(tklabel(surfacesFrame, text="Quadratic least-squares"), quadLSCheckBox, sticky="w")
    dfLabel <- tklabel(surfacesFrame, text="df = ")
    tkgrid(tklabel(surfacesFrame, text="Smooth regression"), nonparCheckBox, 
        dfLabel, dfNonparField, sticky="w")
    tkgrid.configure(dfLabel, sticky="e")
    tkgrid(tklabel(surfacesFrame, text="Additive regression"), additiveCheckBox, 
        tklabel(surfacesFrame, text="df(each term) = "), dfAddField, sticky="w")
    tkgrid(surfacesFrame, sticky="w") 
    tkgrid(tklabel(bgFrame, text="Background Color", fg="blue"), sticky="w", columnspan=2)
    tkgrid(tklabel(bgFrame, text="Black"), blackButton, sticky="w")
    tkgrid(tklabel(bgFrame, text="White"), whiteButton, sticky="w")
    tkgrid(bgFrame, sticky="w")
    tkgrid(groupsButton, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="            "), 
        helpButton, sticky="w")
    tkgrid(buttonsFrame, stick="w")
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(yScroll, sticky="ns")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(yBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
# Model menu dialogs

# last modified 29 January 04 by J. Fox

selectActiveModel <- function(){
    models <- union(listLinearModels(), listGeneralizedLinearModels())
    if (length(models) == 0){
        tkmessageBox(message="There are no models from which to choose.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Select Model")
    modelsFrame <- tkframe(top)
    modelsBox <- tklistbox(modelsFrame, height=min(4, length(models)),
        selectmode="single", background="white")
    modelsScroll <- tkscrollbar(modelsFrame, repeatinterval=5, 
        command=function(...) tkyview(modelsBox, ...))
    tkconfigure(modelsBox, yscrollcommand=function(...) tkset(modelsScroll, ...))
    for (mod in models) tkinsert(modelsBox, "end", mod)
    tkselection.set(modelsBox, if (is.null(.activeModel)) 0 else which(.activeModel == models) - 1)
    buttonsFrame <- tkframe(top)
    onOK <- function(){
    model <- models[as.numeric(tkcurselection(modelsBox)) + 1]
    dataSet <- eval(parse(text=paste("as.character(", model, "$call$data)")))
    if (length(dataSet) == 0){
        tkmessageBox(message="There is no dataset associated with this model.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkfocus(.commander)
            return()
            }
        dataSets <- listDataSets()
        if (!is.element(dataSet, dataSets)){
        tkmessageBox(message=paste("The dataset associated with this model, ", 
                dataSet, ", is not in memory.", sep=""),
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkfocus(.commander)
            return()
            }
        if (dataSet != .activeDataSet) activeDataSet(dataSet)
        activeModel(model)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }    
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    nameFrame <- tkframe(top)
    tkgrid(tklabel(nameFrame, fg="blue", text="Current Model: "), 
        tklabel(nameFrame, text=tclvalue(.modelName)), sticky="w")
    tkgrid(nameFrame, sticky="w", columnspan="2")
    tkgrid(tklabel(top, text="Models (pick one)"), sticky="w")
    tkgrid(modelsBox, modelsScroll, sticky="nw")
    tkgrid(modelsFrame, columnspan="2", sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(modelsScroll, sticky="ns")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

summarizeModel <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    doItAndPrint(paste("summary(", .activeModel, ")", sep=""))
    }

plotModel <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    doItAndPrint("par(mfrow=c(2,2))")
    doItAndPrint(paste("plot(", .activeModel, ")", sep=""))
    doItAndPrint("par(mfrow=c(1,1))")
    }

CRPlots <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    doItAndPrint(paste("cr.plots(", .activeModel, ", ask=FALSE)", sep=""))
    }

AVPlots <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    response <- tclvalue(tkmessageBox(message="Identify points with mouse?", 
        icon="question", type="yesno", default="no"))
    doItAndPrint(paste("av.plots(", .activeModel, ", ask=FALSE, identify.points=",
        response=="yes", ")", sep=""))
    }

anovaTable <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    doItAndPrint(paste("Anova(", .activeModel, ")", sep=""))
    }

VIF <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (class(get(.activeModel, envir=.GlobalEnv))[1] != "lm"){
        tkmessageBox(message="Variance-inflation factors available\nonly for linear models.", 
            icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    doItAndPrint(paste("vif(", .activeModel, ")", sep=""))
    }
            
influencePlot <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    response <- tclvalue(tkmessageBox(message="Identify points with mouse?", 
        icon="question", type="yesno", default="no"))
    labels <- if (response == "no") ", labels=FALSE" else ""
    doItAndPrint(paste("influence.plot(", .activeModel, labels, ")", sep=""))
    }  
    
effectPlots <- function(){
          if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    doItAndPrint('trellis.device(bg="white")')
    command <- paste("plot(all.effects(", .activeModel, "), ask=FALSE)", sep="")
    justDoIt(command)
    logger(command)
    }

addObservationStatistics <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
    addVariable <- function(name, values){
        variable <- paste(.activeModel, ".", name, sep="")
        if (is.element(variable, .variables)) {
            ans <- checkReplace(variable)
            if (tclvalue(ans) == "no") return()
            }
        command <- paste(name, "(", .activeModel, ")", sep="")
#        justDoIt(paste(.activeDataSet, "$", variable, " <<- ", command, sep=""))
        justDoIt(paste(.activeDataSet, "$", variable, " <- ", command, sep=""))
        logger(paste(.activeDataSet, "$", variable, " <- ", command, sep=""))
        }
    if (is.null(.activeModel)){
            tkmessageBox(message="There is no active model.", icon="error", type="ok")
            tkfocus(.commander)
            return()
            }
    if (.modelWithSubset){
        tkmessageBox(message=
            paste("Observation statistics not available\nfor a model fit to a subset of the data."),
            icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Add Observation Statistics to Data")
    selectFrame <- tkframe(top)
    fittedVariable <- tclVar("1")
    residualsVariable <- tclVar("1")
    rstudentVariable <- tclVar("1")
    hatvaluesVariable <- tclVar("1")
    cookdVariable <- tclVar("1")
    obsNumberExists <- is.element("obsNumber", .variables)
    obsNumberVariable <- tclVar(if(obsNumberExists) "0" else "1")
    fittedCheckBox <- tkcheckbutton(selectFrame, variable=fittedVariable)
    residualsCheckBox <- tkcheckbutton(selectFrame, variable=residualsVariable)
    rstudentCheckBox <- tkcheckbutton(selectFrame, variable=rstudentVariable)
    hatvaluesCheckBox <- tkcheckbutton(selectFrame, variable=hatvaluesVariable)
    cookdCheckBox <- tkcheckbutton(selectFrame, variable=cookdVariable)
    obsNumberCheckBox <- tkcheckbutton(selectFrame, variable=obsNumberVariable)
    onOK <- function(){
        if (tclvalue(fittedVariable) == 1) addVariable("fitted")
        if (tclvalue(residualsVariable) == 1) addVariable("residuals")
        if (tclvalue(rstudentVariable) == 1) addVariable("rstudent")
        if (tclvalue(hatvaluesVariable) == 1) addVariable("hatvalues")
        if (tclvalue(cookdVariable) == 1) addVariable("cookd")
        if (tclvalue(obsNumberVariable) == 1){
            proceed <- if (obsNumberExists) tclvalue(checkReplace("obsNumber")) else "yes"
            if (proceed == "yes") {
                command <- paste(.activeDataSet, "$obsNumber <- 1:nrow(", .activeDataSet, ")", sep="")
                justDoIt(command)
                logger(command)
                }
            }
        activeDataSet(.activeDataSet)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }    
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(influence.measures)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(selectFrame, text="Fitted values"), fittedCheckBox, sticky="w")
    tkgrid(tklabel(selectFrame, text="Residuals"), residualsCheckBox, sticky="w")
    tkgrid(tklabel(selectFrame, text="Studentized residuals"), rstudentCheckBox, sticky="w")
    tkgrid(tklabel(selectFrame, text="Hat-values"), hatvaluesCheckBox, sticky="w")
    tkgrid(tklabel(selectFrame, text="Cook's distances"), cookdCheckBox, sticky="w")  
    tkgrid(tklabel(selectFrame, text="Observation indices"), obsNumberCheckBox, sticky="w")
    tkgrid(selectFrame, sticky="w")  
        tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="            "), 
        helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

residualQQPlot <- function(){
    if (is.null(.activeModel)){
            tkmessageBox(message="There is no active model.", icon="error", type="ok")
            tkfocus(.commander)
            return()
            }
    top <- tktoplevel()
    tkwm.title(top, "Residual Quantile-Comparison Plot")
    selectFrame <- tkframe(top)
    simulateVar <- tclVar("1")
    identifyVar <- tclVar("0")
    simulateCheckBox <- tkcheckbutton(selectFrame, variable=simulateVar)
    identifyCheckBox <- tkcheckbutton(selectFrame, variable=identifyVar)
    onOK <- function(){
        tkdestroy(top)
        simulate <- tclvalue(simulateVar) == 1
        identify <- if (tclvalue(identifyVar) == 1) paste("names(residuals(", .activeModel, "))",
            sep="") else "FALSE"
        command <- paste("qq.plot(", .activeModel, ", simulate=", simulate, ", labels=", identify,
            ")", sep="")
        doItAndPrint(command)
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }    
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(qq.plot.lm)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(selectFrame, text="Simulated confidence envelope"), simulateCheckBox, sticky="w")
    tkgrid(tklabel(selectFrame, text="Identify points with mouse"), identifyCheckBox, sticky="w")
    tkgrid(selectFrame, sticky="w")  
        tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="            "), 
        helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    for (row in 0:1) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

testLinearHypothesis <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    env <- environment()
    top <- tktoplevel()
    tkwm.title(top, "Test Linear Hypothesis")
    outerTableFrame <- tkframe(top)
    assign(".tableFrame", tkframe(outerTableFrame), envir=env)
    setUpTable <- function(...){
        tkdestroy(get(".tableFrame", envir=env))
        assign(".tableFrame", tkframe(outerTableFrame), envir=env)
        nrows <- as.numeric(tclvalue(rowsValue))
        col.names <- eval(parse(text=paste("names(coef(", .activeModel, "))")))
        col.names <- substring(paste(abbreviate(col.names, 12), "            "), 1, 12)
        make.col.names <- "tklabel(.tableFrame, text='')"
        for (j in 1:ncols) {
            make.col.names <- paste(make.col.names, ", ", 
                "tklabel(.tableFrame, text='", col.names[j], "')", sep="")
            }
        make.col.names <- paste(make.col.names, ", tklabel(.tableFrame, text='          ')",
            ", tklabel(.tableFrame, text='Right-hand side')", sep="")
        eval(parse(text=paste("tkgrid(", make.col.names, ")", sep="")), envir=env)
        for (i in 1:nrows){   
            varname <- paste(".tab.", i, ".1", sep="") 
            rhs.name <- paste(".rhs.", i, sep="")
            assign(varname, tclVar("0") , envir=env)
            assign(rhs.name, tclVar("0"), envir=env)
            make.row <- paste("tklabel(.tableFrame, text=", i, ")")
            make.row <- paste(make.row, ", ", "tkentry(.tableFrame, width='5', textvariable=", 
                varname, ")", sep="")
            for (j in 2:ncols){
                varname <- paste(".tab.", i, ".", j, sep="")
                assign(varname, tclVar("0"), envir=env)
                make.row <- paste(make.row, ", ", "tkentry(.tableFrame, width='5', textvariable=", 
                    varname, ")", sep="")
                }
            make.row <- paste(make.row, ", tklabel(.tableFrame, text='     '),",
                "tkentry(.tableFrame, width='5', textvariable=", rhs.name, ")", sep="")
            eval(parse(text=paste("tkgrid(", make.row, ")", sep="")), envir=env)
            }
        tkgrid(get(".tableFrame", envir=env), sticky="w")
        }
    ncols <- eval(parse(text=paste("length(coef(", .activeModel, "))")))
    rowsFrame <- tkframe(top)
    rowsValue <- tclVar("1")
    rowsSlider <- tkscale(rowsFrame, from=1, to=ncols, showvalue=FALSE, variable=rowsValue,
        resolution=1, orient="horizontal", command=setUpTable)
    rowsShow <- tklabel(rowsFrame, textvariable=rowsValue, width=2, justify="right")
    onOK <- function(){
        nrows <- as.numeric(tclvalue(rowsValue))
        cell <- 0
        values <- rep(NA, nrows*ncols)
        rhs <- rep(NA, nrows)
        for (i in 1:nrows){
            rhs.name <- paste(".rhs.", i, sep="")
            rhs[i] <- as.numeric(eval(parse(text=paste("tclvalue(", rhs.name,")", sep=""))))
            for (j in 1:ncols){
                cell <- cell+1
                varname <- paste(".tab.", i, ".", j, sep="")
                values[cell] <- as.numeric(eval(parse(text=paste("tclvalue(", varname,")", sep=""))))
                }
            }
        values <- na.omit(values)
        if (length(values) != nrows*ncols){
            tkmessageBox(message=paste("Number of valid entries in hypothesis matrix(", length(values), ")\n",
                "not equal to number of rows (", nrows,") * number of columns (", ncols,").", 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            testLinearHypothesis()
            return()
            }
        if (qr(matrix(values, nrows, ncols, byrow=TRUE))$rank < nrows) {
            tkmessageBox(message="Hypothesis matrix is not of full row rank", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            testLinearHypothesis()
            return()
            }            
        rhs <- na.omit(rhs)
        if (length(rhs) != nrows){
            tkmessageBox(message=paste("Number of valid entries in rhs vector (", length(rhs), ")\n",
                "not equal to number of rows (", nrows,")", 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            testLinearHypothesis()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("matrix(c(", paste(values, collapse=","), "), ", nrows, ", ", ncols,
            ", byrow=TRUE)", sep="")
        assign(".Hypothesis", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".Hypothesis <- ", command, sep=""))
        command <- paste("c(", paste(rhs, collapse=","), ")", sep="")
        assign(".RHS", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".RHS <- ", command, sep=""))
        command <- paste("linear.hypothesis(", .activeModel, ", .Hypothesis, rhs=.RHS)", sep="")
        doItAndPrint(command)
        doItAndPrint("remove(.Hypothesis, .RHS, envir=.GlobalEnv)")                                                    
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(linear.hypothesis)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(rowsFrame, text="Number of Rows:"), rowsSlider, rowsShow, sticky="w")
    tkgrid(rowsFrame, sticky="w")
    tkgrid(tklabel(top, text="Enter hypothesis matrix and right-hand side vector:", fg="blue"), sticky="w")
    tkgrid(outerTableFrame, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)        
    } 

compareModels <- function(){
    models <- union(listLinearModels(), listGeneralizedLinearModels())
    if (length(models) < 2){
        tkmessageBox(message="There are fewer than two models.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Compare Models")
    modelsFrame1 <- tkframe(top)
    modelsBox1 <- tklistbox(modelsFrame1, height=min(4, length(models)),
        selectmode="single", background="white", exportselection="FALSE")
    modelsScroll1 <- tkscrollbar(modelsFrame1, repeatinterval=5, 
        command=function(...) tkyview(modelsBox1, ...))
    tkconfigure(modelsBox1, yscrollcommand=function(...) tkset(modelsScroll1, ...))
    for (mod in models) tkinsert(modelsBox1, "end", mod)
    modelsFrame2 <- tkframe(top)
    modelsBox2 <- tklistbox(modelsFrame2, height=min(4, length(models)),
        selectmode="single", background="white", exportselection="FALSE")
    modelsScroll2 <- tkscrollbar(modelsFrame2, repeatinterval=5, 
        command=function(...) tkyview(modelsBox2, ...))
    tkconfigure(modelsBox2, yscrollcommand=function(...) tkset(modelsScroll2, ...))
    for (mod in models) tkinsert(modelsBox2, "end", mod)
    buttonsFrame <- tkframe(top)
    onOK <- function(){
        model1 <- models[as.numeric(tkcurselection(modelsBox1)) + 1]
        model2 <- models[as.numeric(tkcurselection(modelsBox2)) + 1]
        if (!eval(parse(text=paste("class(", model1, ")[1] == class(", model2, ")[1]",
            sep="")), envir=.GlobalEnv)){
                tkmessageBox(message="Models are not of the same class.", 
                    icon="error", type="ok")
                tkgrab.release(top)
                tkdestroy(top)
                compareModels()
                return()
                }
        tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("anova(", model1, ",", model2, ")", sep=""))
        tkfocus(.commander)
        }
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }    
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(anova)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    tkgrid(tklabel(modelsFrame1, text="First model (pick one)"), sticky="w")
    tkgrid(modelsBox1, modelsScroll1, sticky="nw")
    tkgrid(tklabel(modelsFrame2, text="Second model (pick one)"), sticky="w")
    tkgrid(modelsBox2, modelsScroll2, sticky="nw")
    tkgrid(modelsFrame1, modelsFrame2, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(modelsScroll1, sticky="ns")
    tkgrid.configure(modelsScroll2, sticky="ns")
    for (row in 0:1) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

BreuschPaganTest <- function(){
    checkAddOperator <- function(rhs){
        rhs.chars <- rev(strsplit(rhs, "")[[1]])
        if (length(rhs.chars) < 1) return(FALSE)
        check.char <- if ((rhs.chars[1] != " ") || (length(rhs.chars) == 1)) 
                rhs.chars[1] else rhs.chars[2]
        !is.element(check.char, c("+", "*", ":", "/", "-", "^", "(", "%"))
        }
    if (activeModel() == FALSE) {
        tkmessageBox(message="There is no active model.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (eval(parse(text=paste("class(", .activeModel, ")[1] != 'lm'", sep="")))){
        tkmessageBox(message="Breusch-Pagan test requires a linear model.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Breusch-Pagan Test")
    tkgrid(tklabel(top, text="Score Test for Nonconstant Error Variance"), sticky="w")
    variables <- paste(.variables, ifelse(is.element(.variables, .factors), "[factor]", ""))
    optionsFrame <- tkframe(top)
    xFrame <- tkframe(optionsFrame)
    xBox <- tklistbox(xFrame, height=min(4, length(.variables)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in variables) tkinsert(xBox, "end", x)
    onDoubleClick <- function(){
        var <- as.character(tkget(xBox, "active"))[1]
        tkfocus(rhsEntry)
        rhs <- tclvalue(rhsVariable)
        rhs.chars <- rev(strsplit(rhs, "")[[1]])
        check.char <- if (length(rhs.chars) > 0){
            if ((rhs.chars[1] != " ") || (length(rhs.chars) == 1)) 
                rhs.chars[1] else rhs.chars[2]
            }
            else ""
        tclvalue(rhsVariable) <- if (rhs == "" || 
            is.element(check.char, c("+", "*", ":", "/", "-", "^", "(", "%")))
                paste(rhs, var, sep="")
            else paste(rhs, "+", var)
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onPlus <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "+ ")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onTimes <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "*", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onColon <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, ":", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onSlash <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "/",  sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onIn <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "%in% ")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onMinus <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "- ")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onPower <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "^", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onLeftParen <- function(){
        tkfocus(rhsEntry)
        rhs <- tclvalue(rhsVariable)
        tclvalue(rhsVariable) <- paste(rhs, "(", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onRightParen <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, ")", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onOK <- function(){
        var <- tclvalue(varVariable)
        type <- if (var == "fitted") paste(", varformula = ~ fitted.values(",
                    .activeModel, ")", sep="") 
                else if (var == "predictors") ""
                else paste(", varformula = ~", tclvalue(rhsVariable), 
                ", data=", .activeDataSet, sep="")
        student <- if (tclvalue(studentVariable) == 1) "TRUE" else "FALSE"
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("bptest(", .activeModel, type, ", studentize=", student,
             ")", sep="")
        doItAndPrint(command)  
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    operatorsFrame <- tkframe(optionsFrame)
    plusButton <- tkbutton(operatorsFrame, text="+", width="3", command=onPlus, 
        font=.operatorFont)
    timesButton <- tkbutton(operatorsFrame, text="*", width="3", command=onTimes, 
        font=.operatorFont)
    colonButton <- tkbutton(operatorsFrame, text=":", width="3", command=onColon, 
        font=.operatorFont)
    slashButton <- tkbutton(operatorsFrame, text="/", width="3", command=onSlash, 
        font=.operatorFont)
    inButton <- tkbutton(operatorsFrame, text="%in%", width="3", command=onIn,
        font=.operatorFont)
    minusButton <- tkbutton(operatorsFrame, text="-", width="3", command=onMinus, 
        font=.operatorFont)
    powerButton <- tkbutton(operatorsFrame, text="^", width="3", command=onPower, 
        font=.operatorFont)
    leftParenButton <- tkbutton(operatorsFrame, text="(", width="3", command=onLeftParen, 
        font=.operatorFont)
    rightParenButton <- tkbutton(operatorsFrame, text=")", width="3", command=onRightParen, 
        font=.operatorFont)
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(bptest)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    varVariable <- tclVar("fitted")
    fittedButton <- tkradiobutton(optionsFrame, variable=varVariable, value="fitted")
    predictorsButton <- tkradiobutton(optionsFrame, variable=varVariable, value="predictors")
    otherButton <- tkradiobutton(optionsFrame, variable=varVariable, value="other")
    studentVariable <- tclVar("0")
    studentCheckBox <- tkcheckbutton(optionsFrame, variable=studentVariable)
    rhsVariable <- tclVar("")
    rhsEntry <- tkentry(optionsFrame, width="50", textvariable=rhsVariable)
    rhsXscroll <- tkscrollbar(optionsFrame, repeatinterval=10,
        orient="horizontal", command=function(...) tkxview(rhs, ...))
    tkconfigure(rhsEntry, xscrollcommand=function(...) tkset(rhsXscroll, ...))  
    tkgrid(tklabel(optionsFrame, text="Studentized\ntest statistic", justify="left"),
        studentCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Variance Formula", fg="blue"), sticky="w") 
    tkgrid(tklabel(optionsFrame, text="Fitted values"), fittedButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Explanatory variables"), predictorsButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Other (specify)"), otherButton, 
        tklabel(optionsFrame, text="   ~ "), rhsEntry, sticky="w")
    tkgrid(tklabel(optionsFrame, text=""), tklabel(optionsFrame, text=""),
        tklabel(optionsFrame, text=""), rhsXscroll, sticky="w")
    tkgrid.configure(rhsXscroll, sticky="ew")
    tkgrid(plusButton, timesButton, colonButton, slashButton, inButton, minusButton,
        powerButton, leftParenButton, rightParenButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text=""),tklabel(optionsFrame, text=""),
        tklabel(optionsFrame, text=""), operatorsFrame, sticky="w")
    tkgrid(tklabel(optionsFrame, text=""),tklabel(optionsFrame, text=""),
        tklabel(optionsFrame, text=""),
        tklabel(optionsFrame, text="Variables (double-click to formula)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(tklabel(optionsFrame, text=""),tklabel(optionsFrame, text=""),
        tklabel(optionsFrame, text=""), xFrame, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    tkbind(xBox, "<Double-ButtonPress-1>", onDoubleClick)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

DurbinWatsonTest <- function(){
    if (activeModel() == FALSE) {
        tkmessageBox(message="There is no active model.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (eval(parse(text=paste("class(", .activeModel, ")[1] != 'lm'", sep="")))){
        tkmessageBox(message="Durbin-Watson test requires a linear model.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Durbin-Waton Test")
    tkgrid(tklabel(top, text="Test for First-Order Error Autocorrelation"), sticky="w")
    onOK <- function(){
        altHypothesis <- tclvalue(altHypothesisVariable)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("dwtest(", .activeModel, ', alternative="', altHypothesis,
             '")', sep="")
        doItAndPrint(command)  
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(dwtest)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    optionsFrame <- tkframe(top)
    altHypothesisVariable <- tclVar("greater")
    greaterButton <- tkradiobutton(optionsFrame, variable=altHypothesisVariable, value="greater")
    notequalButton <- tkradiobutton(optionsFrame, variable=altHypothesisVariable, value="two.sided")
    lessButton <- tkradiobutton(optionsFrame, variable=altHypothesisVariable, value="less")
    tkgrid(tklabel(top, text="Alternative Hypothesis", fg="blue"), sticky="w") 
    tkgrid(tklabel(optionsFrame, text="rho >  0"), greaterButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="rho != 0"), notequalButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="rho <  0"), lessButton, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

RESETtest <- function(){
    if (activeModel() == FALSE) {
        tkmessageBox(message="There is no active model.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (eval(parse(text=paste("class(", .activeModel, ")[1] != 'lm'", sep="")))){
        tkmessageBox(message="RESET test requires a linear model.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "RESET Test")
    tkgrid(tklabel(top, text="Test for Nonlinearity"), sticky="w")
    onOK <- function(){
        type <- tclvalue(typeVariable)
        square <- tclvalue(squareVariable)
        cube <- tclvalue(cubeVariable)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        if (square == "0" && cube == "0"){
            tkmessageBox(message="No powers are checked.", 
                icon="error", type="ok")
            tkfocus(.commander)
            RESETtest()
            return()
            }
        powers <- if (square == "1" && cube == "1") "2:3"
            else if (square == "1" && cube == "0") "2"
            else if (square == "0" && cube == "1") "3"
        command <- paste("reset(", .activeModel, ", power=", powers,
            ', type="', type, '")', sep="")
        doItAndPrint(command)  
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(reset)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    optionsFrame <- tkframe(top)
    squareVariable <- tclVar("1")
    squareCheckBox <- tkcheckbutton(optionsFrame, variable=squareVariable)
    cubeVariable <- tclVar("1")
    cubeCheckBox <- tkcheckbutton(optionsFrame, variable=cubeVariable)
    typeVariable <- tclVar("regressor")
    regressorButton <- tkradiobutton(optionsFrame, variable=typeVariable, value="regressor")
    fittedButton <- tkradiobutton(optionsFrame, variable=typeVariable, value="fitted")
    princompButton <- tkradiobutton(optionsFrame, variable=typeVariable, value="princomp")
    tkgrid(tklabel(optionsFrame, text="Powers to Include", fg="blue"), sticky="w")
    tkgrid(tklabel(optionsFrame, text="2 (squares)"), squareCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="3 (cubes)   "), cubeCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Type of Test", fg="blue"), sticky="w") 
    tkgrid(tklabel(optionsFrame, text="Explanatory variables"), regressorButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Fitted values"), fittedButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="First principal component"), princompButton, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

outlierTest <- function(){
    doItAndPrint(paste("outlier.test(", .activeModel, ")", sep=""))
    }
# last modified 20 Mar 04 by J. Fox

.onAttach <- function(...){
    cat("\nRcmdr Version 0.9-6\n")
    Commander()
    }

.onLoad <- function(...){
    save.options <- options(warn=-1)
    on.exit(options(save.options))
    lattice <- require(lattice)
    foreign <- require(foreign)
    mva <- require(mva)
    ctest <- require(ctest)
    tcltk <- require(tcltk)
    abind <- require(abind)
    lmtest <- require(lmtest)
    multcomp <- require(multcomp)
    mvtnorm <- require(mvtnorm)
    relimp <- require(relimp)
    effects <- require(effects)
    rgl <- require(rgl)
    mgcv <- require(mgcv)
    car <- require(car)
    absent <- !c(lattice, foreign, mva, ctest, tcltk, abind, lmtest, multcomp, mvtnorm, relimp,
        effects, rgl, mgcv, car)
    if (any(absent)) {
        cat("\nThe following packages required by Rcmdr are missing:\n")
        cat(paste(c("lattice", "foreign", "mva", "ctest", "tcltk", "abind", "lmtest", "multcomp", 
            "mvtnorm", "relimp", "effects", "rgl", "mgcv", "car")[absent], collapse=", "))
        cat("\n")
        }
    }
# Statistics Menu dialogs

# last modified 27 Jan 04 by J. Fox

    # Dimensional-analysis menu
    
Reliability <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) < 3){
        tkmessageBox(message="There fewer than 3 numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Scale Reliability")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        if (3 > length(x)) {
            tkmessageBox(message="Fewer than 3 variables selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            Reliability()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        x <- paste('"', x, '"', sep="")
        doItAndPrint(paste("reliability(cov(", .activeDataSet, "[,c(", paste(x, collapse=","),
            ')], use="complete.obs"))', sep=""))
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(reliability)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variables (pick three or more)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, tklabel(top, text="    "), helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(xScroll, sticky="ns")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

principalComponents <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) < 2){
        tkmessageBox(message="There fewer than 2 numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Principal Components Analysis")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    optionsFrame <- tkframe(top)
    correlationsVariable <- tclVar("1")
    correlationsCheckBox <- tkcheckbutton(optionsFrame, variable=correlationsVariable)
    screeplotVariable <- tclVar("0")
    screeplotCheckBox <- tkcheckbutton(optionsFrame, variable=screeplotVariable)
    addPCVariable <- tclVar("0")
    addPCCheckBox <- tkcheckbutton(optionsFrame, variable=addPCVariable)
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        nvar <- length(x)
        correlations <- tclvalue(correlationsVariable)
        subset <- tclvalue(subsetVariable)
        screeplot <- tclvalue(screeplotVariable)
        addPC <- tclvalue(addPCVariable)
        if (2 > length(x)) {
            tkmessageBox(message="Fewer than 2 variables selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            principalComponents()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        subset <- if (trim.blanks(subset) == "<all valid cases>") "" else paste(", subset=", subset, sep="")
        correlations <- if (correlations == "1") "TRUE" else "FALSE"
        command <- paste("princomp(~", paste(x, collapse="+"), ", cor=", correlations,
            ", data=", .activeDataSet, subset, ")", sep="")
        assign(".PC", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".PC <- ", command, sep=""))
        doItAndPrint("unclass(loadings(.PC))  # component loadings")
        doItAndPrint(".PC$sd^2  # component variances")
        if (screeplot == "1") {
            justDoIt("screeplot(.PC)")
            logger("screeplot(.PC)")
            }
        if (addPC == "1") {
            if (is.element("PC1", .variables)) {
                if ("no" == tclvalue(checkReplace("PC1"))){
                    if (.grab.focus) tkgrab.release(top)
                    tkdestroy(top)
                    remove(.PC, envir=.GlobalEnv)   
                    logger("remove(.PC)")
                    return()
                    }
                }
            for(i in 1:nvar){
#                justDoIt(paste(.activeDataSet, "$PC", i, " <<- .PC$scores[,", i, "]", sep=""))
                justDoIt(paste(.activeDataSet, "$PC", i, " <- .PC$scores[,", i, "]", sep=""))
                logger(paste(.activeDataSet, "$PC", i, " <- .PC$scores[,", i, "]", sep=""))
                }
            activeDataSet(.activeDataSet)
            remove(.PC, envir=.GlobalEnv)   
            }
        logger("remove(.PC)")
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(princomp)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variables (pick two or more)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Analyze correlation matrix"), 
        correlationsCheckBox, sticky="e")
    tkgrid(tklabel(optionsFrame, text="Screeplot"), screeplotCheckBox, sticky="e")
    tkgrid(tklabel(optionsFrame, text="Add principal components\nto data set", justify="left"),
        addPCCheckBox, sticky="ne")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(correlationsCheckBox, sticky="w")
    tkgrid.configure(screeplotCheckBox, sticky="w")
    tkgrid.configure(addPCCheckBox, sticky="w")   
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(xScroll, sticky="ns")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

factorAnalysis <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) < 3){
        tkmessageBox(message="There fewer than 3 numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Factor Analysis")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    optionsFrame <- tkframe(top)
    nfactorVariable <- tclVar("1")
    nfactorEntry <- tkentry(optionsFrame, width="2", textvariable=nfactorVariable)
    checkFrame <- tkframe(top)
    rotationVariable <- tclVar("varimax")
    rotationFrame <- tkframe(checkFrame)
    noRotateButton <- tkradiobutton(rotationFrame, variable=rotationVariable, value="none")
    varimaxButton <- tkradiobutton(rotationFrame, variable=rotationVariable, value="varimax")
    promaxButton <- tkradiobutton(rotationFrame, variable=rotationVariable, value="promax")
    scoresVariable <- tclVar("none")
    scoresFrame <- tkframe(checkFrame)
    noScoresButton <- tkradiobutton(scoresFrame, variable=scoresVariable, value="none")
    bartlettButton <- tkradiobutton(scoresFrame, variable=scoresVariable, value="Bartlett")
    regressionButton <- tkradiobutton(scoresFrame, variable=scoresVariable, value="regression")
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        nvar <- length(x)
        nfactor <- as.numeric(tclvalue(nfactorVariable))
        subset <- tclvalue(subsetVariable)
        rotation <- tclvalue(rotationVariable)
        scores <- tclvalue(scoresVariable)
        if (3 > length(x)) {
            tkmessageBox(message="Fewer than 3 variables selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            factorAnalysis()
            return()
            }
        f <- function(k, p) ((p - k)^2 - p - k)^2
        max.factors <- floor(optimize(f, c(0, nvar), tol=.0001, p=nvar)$minimum)
        if (nfactor > max.factors) {
            if (max.factors > 1)
                tkmessageBox(message=paste("Number of factors must be between 1 and ",
                    max.factors, ".", sep=""),
                    icon="error", type="ok")
            else
                tkmessageBox(message="Number of factors cannot exceed 1",
                    icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            factorAnalysis()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        subset <- if (trim.blanks(subset) == "<all valid cases>") "" else paste(", subset=", subset, sep="")
        command <- paste("factanal(~", paste(x, collapse="+"), ", factors=", nfactor, ', rotation="', rotation,
            '", scores="', scores, '", data=', .activeDataSet, subset, ")", sep="")
        assign(".FA", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".FA <- ", command, sep=""))
        logger("print(.FA, cutoff=0)")
        justDoIt("print(.FA, cutoff=0)")
        if (scores != "none") {
            if (is.element("F1", .variables)) {
                if ("no" == tclvalue(checkReplace("F1"))){
                    if (.grab.focus) tkgrab.release(top)
                    tkdestroy(top)
                    remove(.FA, envir=.GlobalEnv)   
                    logger("remove(.FA)")
                    return()
                    }
                }
            for(i in 1:nfactor){
#                justDoIt(paste(.activeDataSet, "$F", i, " <<- .FA$scores[,", i, "]", sep=""))
                justDoIt(paste(.activeDataSet, "$F", i, " <- .FA$scores[,", i, "]", sep=""))
                logger(paste(.activeDataSet, "$F", i, " <- .FA$scores[,", i, "]", sep=""))
                }
            activeDataSet(.activeDataSet)
            }
        logger("remove(.FA)")
        remove(.FA, envir=.GlobalEnv)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(factanal)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variables (pick three or more)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Number of factors:"),
        nfactorEntry, sticky="w")
    tkgrid(tklabel(rotationFrame, text="Factor Rotation", fg="blue"), sticky="w")
    tkgrid(tklabel(rotationFrame, text="None"), noRotateButton, sticky="w")
    tkgrid(tklabel(rotationFrame, text="Varimax"), varimaxButton, sticky="w")
    tkgrid(tklabel(rotationFrame, text="Promax"), promaxButton, sticky="w")
    tkgrid(tklabel(scoresFrame, text="Factor Scores", fg="blue"), sticky="w")
    tkgrid(tklabel(scoresFrame, text="None"), noScoresButton, sticky="w")
    tkgrid(tklabel(scoresFrame, text="Bartlett's method"), bartlettButton, sticky="w")
    tkgrid(tklabel(scoresFrame, text="Regression method"), regressionButton, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(rotationFrame, scoresFrame, sticky="w")
    tkgrid(checkFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), 
        helpButton,sticky="w")
    tkgrid(buttonsFrame,  sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
# Statistics Menu dialogs

# last modified 27 Jan 04 by J. Fox

    # Means menu

independentSamplesTTest <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (length(.twoLevelFactors) == 0){
        tkmessageBox(message="There no 2-level factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Independent Samples t-Test")
    groupFrame <- tkframe(top)
    responseFrame <- tkframe(top)
    groupBox <- tklistbox(groupFrame, height=min(4, length(.twoLevelFactors)),
        selectmode="single", background="white", exportselection="FALSE")
    groupScroll <- tkscrollbar(groupFrame, repeatinterval=5, 
        command=function(...) tkyview(groupBox, ...))
    tkconfigure(groupBox, yscrollcommand=function(...) tkset(groupScroll, ...))
    for (group in .twoLevelFactors) tkinsert(groupBox, "end", group)
    responseBox <- tklistbox(responseFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    responseScroll <- tkscrollbar(responseFrame, repeatinterval=5, 
        command=function(...) tkyview(responseBox, ...))    
    tkconfigure(responseBox, yscrollcommand=function(...) tkset(responseScroll, ...))
    for (response in .numeric) tkinsert(responseBox, "end", response)
    onOK <- function(){
        group <- as.character(tkget(groupBox, "active"))
        response <- as.character(tkget(responseBox, "active"))
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        variances <- as.character(tclvalue(variancesVariable))
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("t.test(", response, "~", group,
            ", alternative='", alternative, "', conf.level=", level,
            ", var.equal=", variances,
            ", data=", .activeDataSet, ")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12",command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        } 
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(t.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    alternativeFrame <- tkframe(top)
    alternativeVariable <- tclVar("two.sided")
    twosidedButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="two.sided")
    lessButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="less")
    greaterButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="greater")
    confidenceFrame <- tkframe(top)
    confidenceLevel <- tclVar(".95")
    confidenceField <- tkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    variancesFrame <- tkframe(top)
    variancesVariable <- tclVar("FALSE")
    yesButton <- tkradiobutton(variancesFrame, variable=variancesVariable, value="TRUE")
    noButton <- tkradiobutton(variancesFrame, variable=variancesVariable, value="FALSE")
    tkgrid(tklabel(top, text="Groups (pick one)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid(groupFrame, responseFrame, sticky="nw")
    tkgrid(tklabel(alternativeFrame, text="Alternative Hypothesis", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Two-sided"), twosidedButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference < 0"), lessButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference > 0"), greaterButton, sticky="w")
    tkgrid(tklabel(confidenceFrame, text="Confidence Level"),sticky="w")
    tkgrid(confidenceField, sticky="w")
    tkgrid(tklabel(variancesFrame, text="Assume equal variance?", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(variancesFrame, text="No"), noButton, sticky="w")
    tkgrid(tklabel(variancesFrame, text="Yes"), yesButton, sticky="w")
    tkgrid(alternativeFrame, confidenceFrame, variancesFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, tklabel(top, text=""), helpButton, sticky="w")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:2) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(groupBox, 0)
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

pairedTTest <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) < 2){
        tkmessageBox(message="There fewer than 2 numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Paired t-Test")
    xFrame <- tkframe(top)
    yFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    yBox <- tklistbox(yFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    yScroll <- tkscrollbar(yFrame, repeatinterval=5, command=function(...) tkyview(yBox, ...))    
    tkconfigure(yBox, yscrollcommand=function(...) tkset(yScroll, ...))
    for (y in .numeric) tkinsert(yBox, "end", y)
    onOK <- function(){
        x <- as.character(tkget(xBox, "active"))
        y <- as.character(tkget(yBox, "active"))
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        if (x == y) {
            tkmessageBox(message="Variables must be different.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            pairedTTest()
            return()
            }
        tkdestroy(top)
        doItAndPrint(paste("t.test(", .activeDataSet, "$", x, ", ", 
            .activeDataSet, "$", y,
            ", alternative='", alternative, "', conf.level=", level, 
            ", paired=TRUE)", sep=""))
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }  
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(t.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    alternativeFrame <- tkframe(top)
    alternativeVariable <- tclVar("two.sided")
    twosidedButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="two.sided")
    lessButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="less")
    greaterButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="greater")
    confidenceFrame <- tkframe(top)
    confidenceLevel <- tclVar(".95")
    confidenceField <- tkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    tkgrid(tklabel(top, text="First variable (pick one)"), 
        tklabel(top, text="Second variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(yBox, yScroll, sticky="nw")
    tkgrid(xFrame, yFrame, sticky="nw")    
    tkgrid(tklabel(alternativeFrame, text="Alternative Hypothesis", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Two-sided"), twosidedButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference < 0"), lessButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference > 0"), greaterButton, sticky="w")
    tkgrid(tklabel(confidenceFrame, text="Confidence Level"))
    tkgrid(confidenceField, sticky="w")
    tkgrid(alternativeFrame, confidenceFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(yScroll, sticky="ns")    
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkselection.set(yBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

singleSampleTTest <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There are no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Single-Sample t-Test")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        mu <- tclvalue(muVariable)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("t.test(", .activeDataSet, "$", x,
            ", alternative='", alternative, "', mu=", mu, ", conf.level=", level, 
            ")", sep=""))
        tkdestroy(top)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        } 
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(t.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    alternativeFrame <- tkframe(top)
    alternativeVariable <- tclVar("two.sided")
    twosidedButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="two.sided")
    lessButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="less")
    greaterButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="greater")
    rightFrame <- tkframe(top)
    confidenceFrame <- tkframe(rightFrame)
    confidenceLevel <- tclVar(".95")
    confidenceField <- tkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    muFrame <- tkframe(rightFrame)
    muVariable <- tclVar("0.0")
    muField <- tkentry(muFrame, width="8", textvariable=muVariable)
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="nw")    
    tkgrid(tklabel(alternativeFrame, text="Alternative Hypothesis", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Population mean = mu0"), twosidedButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Population mean < mu0"), lessButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Population mean > mu0"), greaterButton, sticky="w")
    tkgrid(tklabel(muFrame, text="Null hypothesis: mu = "), muField, sticky="w")
    tkgrid(muFrame, sticky="w")
    tkgrid(tklabel(confidenceFrame, text="Confidence Level: "), confidenceField, sticky="w")
    tkgrid(confidenceFrame, sticky="w")
    tkgrid(alternativeFrame, rightFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(confidenceField, sticky="e")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

oneWayAnova <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (length(.factors) == 0){
        tkmessageBox(message="There are no factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "One-Way Analysis of Variance")
    groupFrame <- tkframe(top)
    responseFrame <- tkframe(top)
    groupBox <- tklistbox(groupFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    groupScroll <- tkscrollbar(groupFrame, repeatinterval=5, 
        command=function(...) tkyview(groupBox, ...))
    tkconfigure(groupBox, yscrollcommand=function(...) tkset(groupScroll, ...))
    for (group in .factors) tkinsert(groupBox, "end", group)
    responseBox <- tklistbox(responseFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    responseScroll <- tkscrollbar(responseFrame, repeatinterval=5, 
        command=function(...) tkyview(responseBox, ...))    
    tkconfigure(responseBox, yscrollcommand=function(...) tkset(responseScroll, ...))
    for (response in .numeric) tkinsert(responseBox, "end", response)
    optionsFrame <- tkframe(top)
    pairwiseVariable <- tclVar("0")
    pairwiseCheckBox <- tkcheckbutton(optionsFrame, variable=pairwiseVariable)
    onOK <- function(){
        group <- as.character(tkget(groupBox, "active"))
        response <- as.character(tkget(responseBox, "active"))
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("anova(lm(", response, " ~ ", group, ", data=", .activeDataSet, "))", sep=""))
        doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", ", .activeDataSet, "$", group, 
            ", mean, na.rm=TRUE) # means", sep=""))
        doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", ", .activeDataSet, "$", group, 
            ", sd, na.rm=TRUE) # std. deviations", sep=""))
        doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", ", .activeDataSet, "$", group, 
            ", function(x) sum(!is.na(x))) # counts", sep=""))
        pairwise <- tclvalue(pairwiseVariable)
        if (pairwise == 1) {
            if (eval(parse(text=paste("length(levels(", .activeDataSet, "$", group, ")) < 3")))) 
                tkmessageBox (message="Factor has fewer than 3 levels; pairwise comparisons omitted.",
                    icon="warning", type="ok")
            else doItAndPrint(paste("summary(simtest(", response, " ~ ", group, 
                ', type="Tukey", data=', .activeDataSet, '))', sep=""))
            }
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(anova)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Groups (pick one)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid(groupFrame, responseFrame, sticky="nw")
    tkgrid(tklabel(optionsFrame, text="Pairwise comparisons of means"), pairwiseCheckBox, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(groupBox, 0)
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
    
multiWayAnova <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (length(.factors) == 0){
        tkmessageBox(message="There no factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Multi-Way Analysis of Variance")
    groupFrame <- tkframe(top)
    responseFrame <- tkframe(top)
    groupBox <- tklistbox(groupFrame, height=min(4, length(.factors)),
        selectmode="multiple", background="white", exportselection="FALSE")
    groupScroll <- tkscrollbar(groupFrame, repeatinterval=5, 
        command=function(...) tkyview(groupBox, ...))
    tkconfigure(groupBox, yscrollcommand=function(...) tkset(groupScroll, ...))
    for (group in .factors) tkinsert(groupBox, "end", group)
    responseBox <- tklistbox(responseFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    responseScroll <- tkscrollbar(responseFrame, repeatinterval=5, 
        command=function(...) tkyview(responseBox, ...))    
    tkconfigure(responseBox, yscrollcommand=function(...) tkset(responseScroll, ...))
    for (response in .numeric) tkinsert(responseBox, "end", response)
    onOK <- function(){
        groups <- .factors[as.numeric(tkcurselection(groupBox)) + 1]
        response <- as.character(tkget(responseBox, "active"))
        if (0 == length(groups)) {
            tkmessageBox(message="No factors selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            multiWayAnova()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        groups.list <- paste(paste(groups, "=", .activeDataSet, "$", groups, sep=""), collapse=", ")
        doItAndPrint(paste("Anova(lm(", response, " ~ ", paste(groups, collapse="*"),
             ", data=", .activeDataSet, "))", sep=""))
        doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", list(", groups.list,
             "), mean, na.rm=TRUE) # means", sep=""))
        doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", list(", groups.list,
             "), sd, na.rm=TRUE) # std. deviations", sep=""))
        doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", list(", groups.list,
             "), function(x) sum(!is.na(x))) # counts", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }  
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(Anova)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Factors (pick one or more)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid(groupFrame, responseFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
# Statistics Menu dialogs

# last modified 28 Jan 04 by J. Fox

    # Models menu
    
linearRegressionModel <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Model", name, "already exists.\nOverwrite model?"),
            icon="warning", type="yesno", default="no")
        }
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) < 2){
        tkmessageBox(message="There are fewer than 2 numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Linear Regression")
    variablesFrame <- tkframe(top)
    xFrame <- tkframe(variablesFrame)
    yFrame <- tkframe(variablesFrame)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    yBox <- tklistbox(yFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    yScroll <- tkscrollbar(yFrame, repeatinterval=5, command=function(...) tkyview(yBox, ...))    
    tkconfigure(yBox, yscrollcommand=function(...) tkset(yScroll, ...))
    for (y in .numeric) tkinsert(yBox, "end", y)
    assign(".modelNumber", .modelNumber + 1, envir=.GlobalEnv)
    modelName <- tclVar(paste("RegModel.", .modelNumber, sep=""))
    modelFrame <- tkframe(top)
    model <- tkentry(modelFrame, width="20", textvariable=modelName)
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        y <- as.character(tkget(yBox, "active"))
        if (0 == length(x)) {
            tkmessageBox(message="No explanatory variables selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            linearRegressionModel()
            return()
            }
        if (is.element(y, x)) {
            tkmessageBox(message="Response and explanatory variables must be different.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            linearRegressionModel()
            return()
            }
        subset <- tclvalue(subsetVariable)
        if (trim.blanks(subset) == "<all valid cases>"){
            subset <- ""
            assign(".modelWithSubset", FALSE, envir=.GlobalEnv)
            }
        else{
            subset <- paste(", subset=", subset, sep="")
            assign(".modelWithSubset", TRUE, envir=.GlobalEnv)            
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        modelValue <- trim.blanks(tclvalue(modelName))
        if (!is.valid.name(modelValue)){
            tkmessageBox(message=paste('"', modelValue, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            linearRegressionModel()
            return()
            }
        if (is.element(modelValue, listLinearModels())) {
            if ("no" == tclvalue(checkReplace(modelValue))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                linearRegressionModel()
                return()
                }
            }
        activeModel(modelValue)
        command <- paste("lm(", y, "~", paste(x, collapse="+"),
            ", data=", .activeDataSet, subset, ")", sep="")
        logger(paste(modelValue, " <- ", command, sep=""))
        assign(modelValue, justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("summary(", modelValue, ")", sep=""))
        tkfocus(.commander)
        }
    onCancel <- function() {
        assign(".modelNumber", .modelNumber - 1, envir=.GlobalEnv)
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(lm)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(modelFrame, text="Enter name for model:"), model, sticky="w")
    tkgrid(modelFrame, sticky="w")
    tkgrid(tklabel(variablesFrame, text="Response variable (pick one)"), 
        tklabel(variablesFrame, text="    "),
        tklabel(variablesFrame, text="Explanatory variables (pick one or more)"), sticky="w")
    tkgrid(yBox, yScroll, sticky="nw")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(yFrame, tklabel(variablesFrame, text="    "), xFrame, sticky="nw")
    tkgrid(variablesFrame, sticky="w")    
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="            "), 
        helpButton, sticky="w")
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(buttonsFrame, stick="w")
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(yScroll, sticky="ns")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(yBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

linearModel <- function(){
    formulaFields <- function(model){
        formula <- as.character(model$call$formula)
        lhs <- formula[2]
        rhs <- formula[3]
        data <- as.character(model$call$data)
        which.subset <- which("subset" == names(model$call))
        subset <- if (0 == length(which.subset)) ""
        else as.character(model$call)[[which.subset]]
        list(lhs=lhs, rhs=rhs, data=data, subset=subset)
        }
    checkReplace <- function(name){
        tkmessageBox(message=paste("Model", name, "already exists.\nOverwrite model?"),
            icon="warning", type="yesno", default="no")
        }
    checkAddOperator <- function(rhs){
        rhs.chars <- rev(strsplit(rhs, "")[[1]])
        if (length(rhs.chars) < 1) return(FALSE)
        check.char <- if ((rhs.chars[1] != " ") || (length(rhs.chars) == 1)) 
                rhs.chars[1] else rhs.chars[2]
        !is.element(check.char, c("+", "*", ":", "/", "-", "^", "(", "%"))
        }
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (length(.variables) < 2){
        tkmessageBox(message="There fewer than 2 variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Linear Model")
    variables <- paste(.variables, ifelse(is.element(.variables, .factors), "[factor]", ""))
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.variables)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in variables) tkinsert(xBox, "end", x)
    currentModel <- if (!is.null(.activeModel)) 
        eval(parse(text=paste("class(", .activeModel, ")[1] == 'lm'", sep="")), 
            envir=.GlobalEnv) 
        else FALSE
    if (currentModel) {
        currentFields <- formulaFields(eval(parse(text=.activeModel), 
            envir=.GlobalEnv))
        if (currentFields$data != .activeDataSet) currentModel <- FALSE
        }
    lhsVariable <- if (currentModel) tclVar(currentFields$lhs) else tclVar("")
    rhsVariable <- if (currentModel) tclVar(currentFields$rhs) else tclVar("")
    formulaFrame <- tkframe(top)
    lhsEntry <- tkentry(formulaFrame, width="10", textvariable=lhsVariable)
    lhsScroll <- tkscrollbar(formulaFrame, repeatinterval=5, 
        orient="horizontal", command=function(...) tkxview(lhsEntry, ...))
    tkconfigure(lhsEntry, xscrollcommand=function(...) tkset(lhsScroll, ...))
    rhsEntry <- tkentry(formulaFrame, width="50", textvariable=rhsVariable)
    rhsScroll <- tkscrollbar(formulaFrame, repeatinterval=5, 
        orient="horizontal", command=function(...) tkxview(rhsEntry, ...))
    tkconfigure(rhsEntry, xscrollcommand=function(...) tkset(rhsScroll, ...))
    assign(".modelNumber", .modelNumber + 1, envir=.GlobalEnv)
    modelName <- tclVar(paste("LinearModel.", .modelNumber, sep=""))
    modelFrame <- tkframe(top)
    model <- tkentry(modelFrame, width="20", textvariable=modelName)
    subsetVariable <- if (currentModel && currentFields$subset != "") 
        tclVar(currentFields$subset) else tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    onDoubleClick <- function(){
        var <- as.character(tkget(xBox, "active"))[1]
        lhs <- tclvalue(lhsVariable)
        if (lhs == "") tclvalue(lhsVariable) <- var
        else {
            tkfocus(rhsEntry)
            rhs <- tclvalue(rhsVariable)
            rhs.chars <- rev(strsplit(rhs, "")[[1]])
            check.char <- if (length(rhs.chars) > 0){
                if ((rhs.chars[1] != " ") || (length(rhs.chars) == 1)) 
                    rhs.chars[1] else rhs.chars[2]
                }
                else ""
            tclvalue(rhsVariable) <- if (rhs == "" || 
                is.element(check.char, c("+", "*", ":", "/", "-", "^", "(", "%")))
                    paste(rhs, var, sep="")
                else paste(rhs, "+", var)
            }
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onPlus <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "+ ")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onTimes <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "*", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onColon <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, ":", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onSlash <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "/",  sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onIn <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "%in% ")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onMinus <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "- ")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onPower <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "^", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onLeftParen <- function(){
        tkfocus(rhsEntry)
        rhs <- tclvalue(rhsVariable)
        tclvalue(rhsVariable) <- paste(rhs, "(", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onRightParen <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, ")", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onOK <- function(){
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        modelValue <- trim.blanks(tclvalue(modelName))
        if (!is.valid.name(modelValue)){
            tkmessageBox(message=paste('"', modelValue, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            linearModel()
            return()
            }
        subset <- tclvalue(subsetVariable)
        if (trim.blanks(subset) == "<all valid cases>"){
            subset <- ""
            assign(".modelWithSubset", FALSE, envir=.GlobalEnv)
            }
        else{
            subset <- paste(", subset=", subset, sep="")
            assign(".modelWithSubset", TRUE, envir=.GlobalEnv)            
            }
        check.empty <- gsub(" ", "", tclvalue(lhsVariable))
        if ("" == check.empty) {
            tkmessageBox(message="Left-hand side of model empty.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            linearModel()
            return()
            }
        check.empty <- gsub(" ", "", tclvalue(rhsVariable))
        if ("" == check.empty) {
            tkmessageBox(message="Right-hand side of model empty.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            linearModel()
            return()
            }
        if (is.element(modelValue, listLinearModels())) {
            if ("no" == tclvalue(checkReplace(modelValue))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                linearModel()
                return()
                }
            }
        activeModel(modelValue)
        formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
        command <- paste("lm(", formula,
            ", data=", .activeDataSet, subset, ")", sep="")
        logger(paste(modelValue, " <- ", command, sep=""))
        assign(modelValue, justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("summary(", modelValue, ")", sep=""))
        tkfocus(.commander)
        }
    onCancel <- function() {
        assign(".modelNumber", .modelNumber - 1, envir=.GlobalEnv)
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    operatorsFrame <- tkframe(formulaFrame)
    plusButton <- tkbutton(operatorsFrame, text="+", width="3", command=onPlus, 
        font=.operatorFont)
    timesButton <- tkbutton(operatorsFrame, text="*", width="3", command=onTimes, 
        font=.operatorFont)
    colonButton <- tkbutton(operatorsFrame, text=":", width="3", command=onColon, 
        font=.operatorFont)
    slashButton <- tkbutton(operatorsFrame, text="/", width="3", command=onSlash, 
        font=.operatorFont)
    inButton <- tkbutton(operatorsFrame, text="%in%", width="3", command=onIn,
        font=.operatorFont)
    minusButton <- tkbutton(operatorsFrame, text="-", width="3", command=onMinus, 
        font=.operatorFont)
    powerButton <- tkbutton(operatorsFrame, text="^", width="3", command=onPower, 
        font=.operatorFont)
    leftParenButton <- tkbutton(operatorsFrame, text="(", width="3", command=onLeftParen, 
        font=.operatorFont)
    rightParenButton <- tkbutton(operatorsFrame, text=")", width="3", command=onRightParen, 
        font=.operatorFont)
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(linearModel)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(modelFrame, text="Enter name for model:"), model, sticky="w")
    tkgrid(modelFrame, sticky="w")
    tkgrid(tklabel(top, text="Variables (double-click to formula)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")
    tkgrid(plusButton, timesButton, colonButton, slashButton, inButton, minusButton,
        powerButton, leftParenButton, rightParenButton, sticky="w")
    tkgrid(tklabel(formulaFrame, text="Model formula:"), tklabel(formulaFrame, text=""),
        operatorsFrame, sticky="sw")
    tkgrid(lhsEntry, tklabel(formulaFrame, text=" ~    "), rhsEntry, sticky="w")
    tkgrid(lhsScroll, tklabel(formulaFrame, text=""), 
        rhsScroll, sticky="w")
    tkgrid(formulaFrame)
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="            "), 
        helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(lhsScroll, sticky="ew")
    tkgrid.configure(rhsScroll, sticky="ew")
    for (row in 0:6) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    tkbind(xBox, "<Double-ButtonPress-1>", onDoubleClick)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(lhsEntry)
    tkwait.window(top)
    }

generalizedLinearModel <- function(){
    formulaFields <- function(model){
        formula <- as.character(model$call$formula)
        lhs <- formula[2]
        rhs <- formula[3]
        data <- as.character(model$call$data)
        fam <- as.character(model$call$family)
        family <- fam[1]
        link <- fam[2]
        which.subset <- which("subset" == names(model$call))
        subset <- if (0 == length(which.subset)) ""
        else as.character(model$call)[[which.subset]]
        list(lhs=lhs, rhs=rhs, data=data, subset=subset, family=family, link=link)
        }
    checkReplace <- function(name){
        tkmessageBox(message=paste("Model", name, "already exists.\nOverwrite model?"),
            icon="warning", type="yesno", default="no")
        }
    checkAddOperator <- function(rhs){
        rhs.chars <- rev(strsplit(rhs, "")[[1]])
        if (length(rhs.chars) < 1) return(FALSE)
        check.char <- if ((rhs.chars[1] != " ") || (length(rhs.chars) == 1)) 
                rhs.chars[1] else rhs.chars[2]
        !is.element(check.char, c("+", "*", ":", "/", "-", "^", "(", "%"))
        }
    families <- c("gaussian", "binomial", "poisson", "Gamma", "inverse.gaussian", 
        "quasibinomial", "quasipoisson")
    links <- c("identity", "inverse", "log", "logit", "probit", 
        "cloglog", "sqrt", "1/mu^2")  
    availableLinks <- matrix(c(
        TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE,
        FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  FALSE, FALSE,
        TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, TRUE,  FALSE,
        TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE,
        TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,
        FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  FALSE, FALSE,
        TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, TRUE,  FALSE),
        7, 8, byrow=TRUE)
    rownames(availableLinks) <- families
    colnames(availableLinks) <- links
    canonicalLinks <- c("identity", "logit", "log", "inverse", "1/mu^2", "logit", "log")
    names(canonicalLinks) <- families
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.variables) < 2){
        tkmessageBox(message="There fewer than 2 variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Generalized Linear Model")
    variables <- paste(.variables, ifelse(is.element(.variables, .factors), "[factor]", ""))
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.variables)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in variables) tkinsert(xBox, "end", x)
    currentModel <- if (!is.null(.activeModel)) 
        eval(parse(text=paste("class(", .activeModel, ")[1] == 'glm'", sep="")), 
            envir=.GlobalEnv)
        else FALSE
    if (currentModel) {
        currentFields <- formulaFields(eval(parse(text=.activeModel), 
            envir=.GlobalEnv))
        if (currentFields$data != .activeDataSet) currentModel <- FALSE
        }
    lhsVariable <- if (currentModel) tclVar(currentFields$lhs) else tclVar("")
    rhsVariable <- if (currentModel) tclVar(currentFields$rhs) else tclVar("")
    formulaFrame <- tkframe(top)
    lhsEntry <- tkentry(formulaFrame, width="10", textvariable=lhsVariable)
    lhsScroll <- tkscrollbar(formulaFrame, repeatinterval=5, 
        orient="horizontal", command=function(...) tkxview(lhsEntry, ...))
    tkconfigure(lhsEntry, xscrollcommand=function(...) tkset(lhsScroll, ...))
    rhsEntry <- tkentry(formulaFrame, width="50", textvariable=rhsVariable)
    rhsScroll <- tkscrollbar(formulaFrame, repeatinterval=5, 
        orient="horizontal", command=function(...) tkxview(rhsEntry, ...))
    tkconfigure(rhsEntry, xscrollcommand=function(...) tkset(rhsScroll, ...))
    assign(".modelNumber", .modelNumber + 1, envir=.GlobalEnv)
    modelName <- tclVar(paste("GLM.", .modelNumber, sep=""))
    modelFrame <- tkframe(top)
    model <- tkentry(modelFrame, width="20", textvariable=modelName)
    linkFamilyFrame <- tkframe(top)
    familyFrame <- tkframe(linkFamilyFrame)
    familyBox <- tklistbox(familyFrame, height="4", exportselection="FALSE",
        selectmode="single", background="white")
    familyScroll <- tkscrollbar(familyFrame, repeatinterval=5, 
        command=function(...) tkyview(familyBox, ...))
    tkconfigure(familyBox, yscrollcommand=function(...) tkset(familyScroll, ...))
    for (fam in families) tkinsert(familyBox, "end", fam)
    linkFrame <- tkframe(linkFamilyFrame)
    linkBox <- tklistbox(linkFrame, height="4", exportselection="FALSE",
        selectmode="single", background="white")
    subsetVariable <- if (currentModel && currentFields$subset != "") 
        tclVar(currentFields$subset) else tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    onDoubleClick <- function(){
        var <- as.character(tkget(xBox, "active"))[1]
        lhs <- tclvalue(lhsVariable)
        if (lhs == "") tclvalue(lhsVariable) <- var
        else {
            tkfocus(rhsEntry)
            rhs <- tclvalue(rhsVariable)
            rhs.chars <- rev(strsplit(rhs, "")[[1]])
            check.char <- if (length(rhs.chars) > 0){
                if ((rhs.chars[1] != " ") || (length(rhs.chars) == 1)) 
                    rhs.chars[1] else rhs.chars[2]
                }
                else ""
            tclvalue(rhsVariable) <- if (rhs == "" || 
                is.element(check.char, c("+", "*", ":", "/", "-", "^", "(", "%")))
                    paste(rhs, var, sep="")
                else paste(rhs, "+", var)
            }
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onPlus <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "+ ")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onTimes <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "*", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onColon <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, ":", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onSlash <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "/",  sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onIn <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "%in% ")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onMinus <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "- ")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onPower <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "^", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onLeftParen <- function(){
        tkfocus(rhsEntry)
        rhs <- tclvalue(rhsVariable)
        tclvalue(rhsVariable) <- paste(rhs, "(", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onRightParen <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, ")", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onFamilySelect <- function(){
        family <- families[as.numeric(tkcurselection(familyBox)) + 1]
        availLinks <- links[availableLinks[family,]]
        tkdelete(linkBox, "0", "end")
        for (lnk in availLinks) tkinsert(linkBox, "end", lnk)
        canLink <- canonicalLinks[family]
        tkconfigure(linkBox, height=length(availLinks))
        tkselection.set(linkBox, which(canLink == availLinks) - 1)
        }
    onOK <- function(){
        check.empty <- gsub(" ", "", tclvalue(lhsVariable))
        if ("" == check.empty) {
            tkmessageBox(message="Left-hand side of model empty.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            generalizedLinearModel()
            return()
            }
        check.empty <- gsub(" ", "", tclvalue(rhsVariable))
        if ("" == check.empty) {
            tkmessageBox(message="Right-hand side of model empty.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            generalizedLinearModel()
            return()
            }
        modelValue <- trim.blanks(tclvalue(modelName))
        if (!is.valid.name(modelValue)){
            tkmessageBox(message=paste('"', modelValue, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            generalizedLinearModel()
            return()
            }
        if (is.element(modelValue, listGeneralizedLinearModels())) {
            if ("no" == tclvalue(checkReplace(modelValue))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                generalizedLinearModel()
                return()
                }
            }
        activeModel(modelValue)
        formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
        family <- families[as.numeric(tkcurselection(familyBox)) + 1]
        availLinks <- links[availableLinks[family,]]
        link <- availLinks[as.numeric(tkcurselection(linkBox)) + 1]
        subset <- tclvalue(subsetVariable)
        if (trim.blanks(subset) == "<all valid cases>"){
            subset <- ""
            assign(".modelWithSubset", FALSE, envir=.GlobalEnv)
            }
        else{
            subset <- paste(", subset=", subset, sep="")
            assign(".modelWithSubset", TRUE, envir=.GlobalEnv)            
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("glm(", formula, ", family=", family, "(", link,
            "), data=", .activeDataSet, subset, ")", sep="")
        logger(paste(modelValue, " <- ", command, sep=""))
        assign(modelValue, justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("summary(", modelValue, ")", sep=""))
        tkfocus(.commander)
        }
    onCancel <- function() {
        assign(".modelNumber", .modelNumber - 1, envir=.GlobalEnv)
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    operatorsFrame <- tkframe(formulaFrame)
    plusButton <- tkbutton(operatorsFrame, text="+", width="3", command=onPlus, 
        font=.operatorFont)
    timesButton <- tkbutton(operatorsFrame, text="*", width="3", command=onTimes, 
        font=.operatorFont)
    colonButton <- tkbutton(operatorsFrame, text=":", width="3", command=onColon, 
        font=.operatorFont)
    slashButton <- tkbutton(operatorsFrame, text="/", width="3", command=onSlash, 
        font=.operatorFont)
    inButton <- tkbutton(operatorsFrame, text="%in%", width="3", command=onIn,
        font=.operatorFont)
    minusButton <- tkbutton(operatorsFrame, text="-", width="3", command=onMinus, 
        font=.operatorFont)
    powerButton <- tkbutton(operatorsFrame, text="^", width="3", command=onPower, 
        font=.operatorFont)
    leftParenButton <- tkbutton(operatorsFrame, text="(", width="3", command=onLeftParen, 
        font=.operatorFont)
    rightParenButton <- tkbutton(operatorsFrame, text=")", width="3", command=onRightParen, 
        font=.operatorFont)
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(generalizedLinearModel)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(modelFrame, text="Enter name for model:"), model, sticky="w")
    tkgrid(modelFrame, sticky="w")
    tkgrid(tklabel(top, text="Variables (double-click to formula)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")
    tkgrid(plusButton, timesButton, colonButton, slashButton, inButton, minusButton,
        powerButton, leftParenButton, rightParenButton, sticky="w")
    tkgrid(tklabel(formulaFrame, text="Model formula:"), tklabel(formulaFrame, text=""),
        operatorsFrame, sticky="sw")
    tkgrid(lhsEntry, tklabel(formulaFrame, text=" ~    "), rhsEntry, sticky="w")
    tkgrid(lhsScroll, tklabel(formulaFrame, text=""), 
        rhsScroll, sticky="w")
    tkgrid(formulaFrame)
    tkgrid(tklabel(linkFamilyFrame, text="Family (double-click to select)"), 
        tklabel(linkFamilyFrame, text="   "), tklabel(linkFamilyFrame, text="Link function"), sticky="w")
    tkgrid(familyBox, familyScroll, sticky="nw")
    tkgrid(linkBox, sticky="nw")
    tkgrid(familyFrame, tklabel(linkFamilyFrame, text="   "), linkFrame, sticky="nw")
    tkgrid(linkFamilyFrame, sticky="w")
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="            "), 
        helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(rhsScroll, sticky="ew")
    tkgrid.configure(lhsScroll, sticky="ew")
    tkgrid.configure(familyScroll, sticky="ns")
    for (row in 0:6) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    fam <- if (currentModel) which(currentFields$family == families) - 1
        else 1
    tkselection.set(familyBox, fam)
    availLinks <- links[availableLinks[fam + 1,]]
    for (lnk in availLinks) tkinsert(linkBox, "end", lnk)
    tkconfigure(linkBox, height=length(availLinks))
    lnk <- if (currentModel) which(currentFields$link == availLinks) - 1
            else 0
    tkselection.set(linkBox, lnk)
    tkbind(top, "<Return>", onOK)
    tkbind(xBox, "<Double-ButtonPress-1>", onDoubleClick)
    tkbind(familyBox, "<Double-ButtonPress-1>", onFamilySelect)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(lhsEntry)
    tkwait.window(top)
    }
# Statistics Menu dialogs

# last modified 27 Jan 04 by J. Fox

    # Nonparametric tests menu
    
twoSampleWilcoxonTest <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There are no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (length(.twoLevelFactors) == 0){
        tkmessageBox(message="There are no 2-level factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Two-Sample Wilcoxon Test")
    groupFrame <- tkframe(top)
    responseFrame <- tkframe(top)
    groupBox <- tklistbox(groupFrame, height=min(4, length(.twoLevelFactors)),
        selectmode="single", background="white", exportselection="FALSE")
    groupScroll <- tkscrollbar(groupFrame, repeatinterval=5, 
        command=function(...) tkyview(groupBox, ...))
    tkconfigure(groupBox, yscrollcommand=function(...) tkset(groupScroll, ...))
    for (group in .twoLevelFactors) tkinsert(groupBox, "end", group)
    responseBox <- tklistbox(responseFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    responseScroll <- tkscrollbar(responseFrame, repeatinterval=5, 
        command=function(...) tkyview(responseBox, ...))    
    tkconfigure(responseBox, yscrollcommand=function(...) tkset(responseScroll, ...))
    for (response in .numeric) tkinsert(responseBox, "end", response)
    onOK <- function(){
        group <- as.character(tkget(groupBox, "active"))
        response <- as.character(tkget(responseBox, "active"))
        alternative <- as.character(tclvalue(alternativeVariable))
        test <- as.character(tclvalue(testVariable))
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("tapply(", paste(.activeDataSet,"$", response, sep=""),
            ", ", paste(.activeDataSet,"$", group, sep=""), ", median, na.rm=TRUE)", sep=""))
        if (test == "default"){
            doItAndPrint(paste("wilcox.test(", response, " ~ ", group, ', alternative="', 
            alternative, '", data=', .activeDataSet, ")", sep=""))
            }
        else doItAndPrint(paste("wilcox.test(", response, " ~ ", group, ", alternative='", 
            alternative, "', exact=", test=="exact", 
            ", correct=", test=="correct",", data=", .activeDataSet, ")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }  
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(wilcox.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    alternativeFrame <- tkframe(top)
    alternativeVariable <- tclVar("two.sided")
    twosidedButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="two.sided")
    lessButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="less")
    greaterButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="greater")
    testFrame <- tkframe(top)
    testVariable <- tclVar("default")
    defaultButton <- tkradiobutton(testFrame, variable=testVariable, value="default")
    exactButton <- tkradiobutton(testFrame, variable=testVariable, value="exact")
    normalButton <- tkradiobutton(testFrame, variable=testVariable, value="normal")
    correctButton <- tkradiobutton(testFrame, variable=testVariable, value="correct")
    tkgrid(tklabel(top, text="Groups (pick one)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid(groupFrame, responseFrame, sticky="nw")
    tkgrid(tklabel(alternativeFrame, text="Alternative Hypothesis", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Two-sided"), twosidedButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference < 0"), lessButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference > 0"), greaterButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Type of Test", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(testFrame, text="Default"), defaultButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Exact"), exactButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Normal approximation"), normalButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Normal approximation with\ncontinuity correction", justify="left"), 
        correctButton, sticky="w")
    tkgrid(alternativeFrame, testFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(groupBox, 0)
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }    

pairedWilcoxonTest <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) < 2){
        tkmessageBox(message="There fewer than 2 numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Paired Wilcoxon Test")
    xFrame <- tkframe(top)
    yFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    yBox <- tklistbox(yFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    yScroll <- tkscrollbar(yFrame, repeatinterval=5, command=function(...) tkyview(yBox, ...))    
    tkconfigure(yBox, yscrollcommand=function(...) tkset(yScroll, ...))
    for (y in .numeric) tkinsert(yBox, "end", y)
    onOK <- function(){
        x <- as.character(tkget(xBox, "active"))
        y <- as.character(tkget(yBox, "active"))
        alternative <- as.character(tclvalue(alternativeVariable))
        test <- as.character(tclvalue(testVariable))
        if (x == y) {
            tkmessageBox(message="Two variables must be different.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            pairedWilcoxonTest()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        if (test == "default"){
             doItAndPrint(paste("wilcox.test(", .activeDataSet, "$", x, ", ", 
                .activeDataSet, "$", y,
                ", alternative='", alternative,
                "')", sep=""))           
            }
        else if (test == "exact"){
            doItAndPrint(paste("wilcox.test(", .activeDataSet, "$", x, ", ", 
                .activeDataSet, "$", y,
                ", alternative='", alternative,
                "', exact=TRUE, paired=TRUE)", sep=""))
                }
        else {
            doItAndPrint(paste("wilcox.test(", .activeDataSet, "$", x, ", ", 
                .activeDataSet, "$", y,
                ", alternative='", alternative, "', correct=", test=="correct",
                ", exact=FALSE, paired=TRUE)", sep=""))
                }
        tkdestroy(top)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }  
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(wilcox.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    alternativeFrame <- tkframe(top)
    alternativeVariable <- tclVar("two.sided")
    twosidedButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="two.sided")
    lessButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="less")
    greaterButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="greater")
    testFrame <- tkframe(top)
    testVariable <- tclVar("default")
    defaultButton <- tkradiobutton(testFrame, variable=testVariable, value="default")
    exactButton <- tkradiobutton(testFrame, variable=testVariable, value="exact")
    normalButton <- tkradiobutton(testFrame, variable=testVariable, value="normal")
    correctButton <- tkradiobutton(testFrame, variable=testVariable, value="correct")
    tkgrid(tklabel(top, text="First variable (pick one)"), 
        tklabel(top, text="Second variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(yBox, yScroll, sticky="nw")
    tkgrid(xFrame, yFrame, sticky="nw")    
    tkgrid(tklabel(alternativeFrame, text="Alternative Hypothesis", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Two-sided"), twosidedButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference < 0"), lessButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference > 0"), greaterButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Type of Test", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(testFrame, text="Default"), defaultButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Exact"), exactButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Normal approximation"), normalButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Normal approximation with\ncontinuity correction", justify="left"), 
        correctButton, sticky="w")    
    tkgrid(alternativeFrame, testFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(yScroll, sticky="ns")    
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkselection.set(yBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
    
KruskalWallisTest <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There are no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (length(.factors) == 0){
        tkmessageBox(message="There are no factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Kruskal-Wallis Rank Sum Test")
    groupFrame <- tkframe(top)
    responseFrame <- tkframe(top)
    groupBox <- tklistbox(groupFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    groupScroll <- tkscrollbar(groupFrame, repeatinterval=5, 
        command=function(...) tkyview(groupBox, ...))
    tkconfigure(groupBox, yscrollcommand=function(...) tkset(groupScroll, ...))
    for (group in .factors) tkinsert(groupBox, "end", group)
    responseBox <- tklistbox(responseFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    responseScroll <- tkscrollbar(responseFrame, repeatinterval=5, 
        command=function(...) tkyview(responseBox, ...))    
    tkconfigure(responseBox, yscrollcommand=function(...) tkset(responseScroll, ...))
    for (response in .numeric) tkinsert(responseBox, "end", response)
    onOK <- function(){
        group <- as.character(tkget(groupBox, "active"))
        response <- as.character(tkget(responseBox, "active"))
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("tapply(", paste(.activeDataSet, "$", response, sep=""),
            ", ", paste(.activeDataSet, "$", group, sep=""), ", median, na.rm=TRUE)", sep=""))
        doItAndPrint(paste("kruskal.test(", response, " ~ ", group, ", data=",
            .activeDataSet, ")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(kruskal.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Groups (pick one)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid(groupFrame, responseFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(groupBox, 0)
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
# Statistics Menu dialogs

# last modified 27 Jan 04 by J. Fox

    # Proportions menu
    
singleProportionTest <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.twoLevelFactors) == 0){
        tkmessageBox(message="There are no 2-level factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Single-Sample Proportion Test")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.twoLevelFactors)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .twoLevelFactors) tkinsert(xBox, "end", x)
    onOK <- function(){
        x <- .twoLevelFactors[as.numeric(tkcurselection(xBox)) + 1]
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        test <- as.character(tclvalue(testVariable))
        p <- tclvalue(pVariable)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("xtabs(~", x, ", data=", .activeDataSet, ")")
        logger(paste(".Table <-", command))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(".Table")
        if (test == "normal") doItAndPrint(paste("prop.test(rbind(.Table), alternative='", 
            alternative, "', p=", p, ", conf.level=", level, ", correct=FALSE)", sep=""))
        else if (test == "corrected") doItAndPrint(paste("prop.test(rbind(.Table), alternative='", 
            alternative, "', p=", p, ", conf.level=", level, ", correct=TRUE)", sep=""))
        else doItAndPrint(paste("binom.test(rbind(.Table), alternative='", 
            alternative, "', p=", p, ", conf.level=", level, ")", sep=""))
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }  
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(prop.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    alternativeFrame <- tkframe(top)
    alternativeVariable <- tclVar("two.sided")
    twosidedButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="two.sided")
    lessButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="less")
    greaterButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="greater")
    rightFrame <- tkframe(top)
    confidenceFrame <- tkframe(rightFrame)
    confidenceLevel <- tclVar(".95")
    confidenceField <- tkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    pFrame <- tkframe(rightFrame)
    pVariable <- tclVar(".5")
    pField <- tkentry(pFrame, width="6", textvariable=pVariable)
    testFrame <- tkframe(top)
    testVariable <- tclVar("normal")
    normalButton <- tkradiobutton(testFrame, variable=testVariable, value="normal")
    correctedButton <- tkradiobutton(testFrame, variable=testVariable, value="corrected")
    exactButton <- tkradiobutton(testFrame, variable=testVariable, value="exact")    
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="nw")    
    tkgrid(tklabel(alternativeFrame, text="Alternative Hypothesis", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Population proportion = p0"), twosidedButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Population proportion < p0"), lessButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Population proportion > p0"), greaterButton, sticky="w")
    tkgrid(tklabel(pFrame, text="Null hypothesis: p = "), pField, sticky="w")
    tkgrid(pFrame, sticky="w")
    tkgrid(tklabel(confidenceFrame, text="Confidence Level: "), confidenceField, sticky="w")
    tkgrid(confidenceFrame, sticky="w")
    tkgrid(alternativeFrame, rightFrame, sticky="nw")
    tkgrid(tklabel(testFrame, text="Type of Test", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(testFrame, text="Normal approximation"), normalButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Normal approximation with\ncontinuity correction", justify="left"), 
        correctedButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Exact binomial"), exactButton, sticky="w")
    tkgrid(testFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(confidenceField, sticky="e")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

twoSampleProportionsTest <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.twoLevelFactors) < 2){
        tkmessageBox(message="There are fewer than 2 two-level factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Two-Sample Proportions Test")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.twoLevelFactors)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .twoLevelFactors) tkinsert(xBox, "end", x)
    groupsFrame <- tkframe(top)
    groupsBox <- tklistbox(groupsFrame, height=min(4, length(.twoLevelFactors)),
        selectmode="single", background="white", exportselection="FALSE")
    groupsScroll <- tkscrollbar(groupsFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(groupsBox, yscrollcommand=function(...) tkset(groupsScroll, ...))
    for (group in .twoLevelFactors) tkinsert(groupsBox, "end", group)
    onOK <- function(){
        x <- .twoLevelFactors[as.numeric(tkcurselection(xBox)) + 1]
        groups <- .twoLevelFactors[as.numeric(tkcurselection(groupsBox)) + 1]
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        test <- as.character(tclvalue(testVariable))
        if (x == groups) {
            tkmessageBox(message="Groups and response variables must be different.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            twoSampleProportionsTest()
            return()
            }

        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("xtabs(~", groups, "+", x, ", data=", .activeDataSet, ")", sep="")
        logger(paste(".Table <-", command))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(".Table")
        if (test == "normal") doItAndPrint(paste("prop.test(.Table, alternative='", 
            alternative, "', conf.level=", level, ", correct=FALSE)", sep=""))
        else doItAndPrint(paste("prop.test(.Table, alternative='", 
            alternative, "', conf.level=", level, ", correct=TRUE)", sep=""))
        logger("remove(.Table)")
        remove(.Table, envir=.GlobalEnv)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(prop.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    alternativeFrame <- tkframe(top)
    alternativeVariable <- tclVar("two.sided")
    twosidedButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="two.sided")
    lessButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="less")
    greaterButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="greater")
    rightFrame <- tkframe(top)
    confidenceFrame <- tkframe(rightFrame)
    confidenceLevel <- tclVar(".95")
    confidenceField <- tkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    testFrame <- tkframe(top)
    testVariable <- tclVar("normal")
    normalButton <- tkradiobutton(testFrame, variable=testVariable, value="normal")
    correctedButton <- tkradiobutton(testFrame, variable=testVariable, value="corrected")
    tkgrid(tklabel(top, text="Groups (pick one)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(groupsBox, groupsScroll, sticky="nw")
    tkgrid(groupsFrame, xFrame, sticky="nw")    
    tkgrid(tklabel(alternativeFrame, text="Alternative Hypothesis", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Two-sided"), twosidedButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference < 0"), lessButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference > 0"), greaterButton, sticky="w")
    tkgrid(tklabel(confidenceFrame, text="Confidence Level: "), confidenceField, sticky="w")
    tkgrid(confidenceFrame, sticky="w")
    tkgrid(alternativeFrame, rightFrame, sticky="nw")
    tkgrid(tklabel(testFrame, text="Type of Test", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(testFrame, text="Normal approximation"), normalButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Normal approximation with\ncontinuity correction", justify="left"), 
        correctedButton, sticky="w")
    tkgrid(testFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(groupsScroll, sticky="ns")
    tkgrid.configure(confidenceField, sticky="e")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkselection.set(groupsBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
# Statistics Menu dialogs

# last modified 4 Feb 04 by J. Fox

    # Summaries menu
    
summarizeDataSet <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    nvar <- length(.variables)
    if (nvar > 10){
        response <- tkmessageBox(message=paste("There are ", nvar, " variables in the data set ",
            .activeDataSet, ".\nDo you want to proceed?", sep=""),
            icon="question", type="okcancel", default="cancel")
        if ("cancel" == tclvalue(response)) {
            tkfocus(.commander)        
            return()
            }
        }
    doItAndPrint(paste("summary(", .activeDataSet, ")", sep=""))
    }

numericalSummaries <- function(){
    env <- environment()
    .groupsLabel <- tclVar("Summarize by groups")
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Numerical Summaries")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    checkBoxFrame <- tkframe(top)
    meanVariable <- tclVar("1")
    meanCheckBox <- tkcheckbutton(checkBoxFrame, variable=meanVariable)
    sdVariable <- tclVar("1")
    sdCheckBox <- tkcheckbutton(checkBoxFrame, variable=sdVariable)
    quantilesVariable <- tclVar("1")
    quantilesFrame <- tkframe(top)
    quantilesCheckBox <- tkcheckbutton(quantilesFrame, variable=quantilesVariable)
    quantiles <- tclVar("0,.25,.5,.75,1")
    quantilesEntry <- tkentry(quantilesFrame, width="20", textvariable=quantiles)
    .groups <- FALSE
    onOK <- function(){
        x <- as.character(tkget(xBox, "active"))
        quants <- paste("c(", gsub(" ", ",", tclvalue(quantiles)), ")")
        var <- paste(.activeDataSet, "$", x, sep="")
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        if (.groups == FALSE) {
            if (tclvalue(meanVariable) == "1") doItAndPrint(paste("mean(", var, ", na.rm=TRUE)", sep=""))
            if (tclvalue(sdVariable) == "1") doItAndPrint(paste("sd(", var, ", na.rm=TRUE)", sep=""))
            if (tclvalue(quantilesVariable) == "1") doItAndPrint(paste("quantile(", var, ", ",
                quants, ", na.rm=TRUE)", sep=""))
            }
        else {
            grps <- paste(.activeDataSet, "$", .groups, sep="")
            if (tclvalue(meanVariable) == "1") doItAndPrint(paste("by(", var, ",", grps,
                ", mean, na.rm=TRUE)", sep=""))
            if (tclvalue(sdVariable) == "1") doItAndPrint(paste("by(", var, ",", grps,
                ", sd, na.rm=TRUE)", sep=""))
            if (tclvalue(quantilesVariable) == "1") doItAndPrint(paste("by(", var, ",", grps,
                ", quantile, na.rm=TRUE, probs=", quants,")", sep=""))
            }
        tkfocus(.commander)
        }
    onGroups <- function(){
        subdialog <- tktoplevel()
        tkwm.title(subdialog, "Groups")
        groupsFrame <- tkframe(subdialog)
        groupsBox <- tklistbox(groupsFrame, height=min(4, length(.factors)),
            selectmode="single", background="white", exportselection="FALSE")
        groupsScroll <- tkscrollbar(groupsFrame, repeatinterval=5, command=function(...) tkyview(groupsBox, ...))
        tkconfigure(groupsBox, yscrollcommand=function(...) tkset(groupsScroll, ...))
        for (groups in .factors) tkinsert(groupsBox, "end", groups)
        onOKsub <- function() {
            groups <- as.character(tkget(groupsBox, "active"))
            assign(".groups", groups, envir=env)
            tclvalue(.groupsLabel) <- paste("Summarize by:", groups)
            tkconfigure(groupsButton, fg="blue")
            if (.grab.focus) tkgrab.release(subdialog)
            tkdestroy(subdialog)
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        onCancelSub <- function() {
            assign(".groups", FALSE, envir=env)
            tclvalue(.groupsLabel) <- "Summarize by groups"
            tkconfigure(groupsButton, fg="black")
            if (.grab.focus) tkgrab.release(subdialog)  
            tkdestroy(subdialog)
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        subButtonFrame <- tkframe(subdialog)
        OKSubButton <- tkbutton(subButtonFrame, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
        cancelSubButton <- tkbutton(subButtonFrame, text="Cancel", fg="red", width="12", command=onCancelSub)
        tkgrid(tklabel(subdialog, text="Groups (pick one)"), sticky="w")
        tkgrid(groupsBox, groupsScroll, sticky="nw")
        tkgrid(groupsFrame, sticky="w")
        tkgrid(OKSubButton, cancelSubButton, sticky="w")
        tkgrid(subButtonFrame, sticky="w")
        for (row in 0:2) tkgrid.rowconfigure(subdialog, row, weight=0)
        for (col in 0:0) tkgrid.columnconfigure(subdialog, col, weight=0)
        .Tcl("update idletasks")
        tkwm.resizable(subdialog, 0, 0)
        tkgrid.configure(groupsScroll, sticky="ns")
        tkselection.set(groupsBox, 0)
        tkbind(subdialog, "<Return>", onOKsub)
        if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOKsub)
        tkbind(groupsBox, "<Double-ButtonPress-1>", onOKsub)
        tkwm.deiconify(subdialog)
        if (.grab.focus) tkgrab.set(subdialog)
        tkfocus(subdialog)
        tkwait.window(subdialog)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(quantile)
        }
    helpButton <- tkbutton(buttonFrame, text="Help", width="12", command=onHelp)
    groupsButton <- tkbutton(top, textvariable=.groupsLabel, command=onGroups)
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")    
    tkgrid(tklabel(checkBoxFrame, text="Mean"), meanCheckBox, sticky="w")
    tkgrid(tklabel(checkBoxFrame, text="Standard deviation"), sdCheckBox, sticky="w")
    tkgrid(checkBoxFrame, sticky="w")
    tkgrid(tklabel(quantilesFrame, text="Quantiles"), quantilesCheckBox,
        tklabel(quantilesFrame, text=" quantiles:"), quantilesEntry, sticky="w")
    tkgrid(quantilesFrame)
    tkgrid(groupsButton, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonFrame, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

frequencyDistribution <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.factors) == 0){
        tkmessageBox(message="There no factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Frequency Distribution")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .factors) tkinsert(xBox, "end", x)
    optionsFrame <- tkframe(top)
    goodnessOfFitVariable <- tclVar("0")
    goodnessOfFitCheckBox <- tkcheckbutton(optionsFrame, variable=goodnessOfFitVariable)
    onOK <- function(){
        x <- as.character(tkget(xBox, "active"))
        goodnessOfFit <- tclvalue(goodnessOfFitVariable)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("table(", .activeDataSet, "$", x, ")", sep="")
        logger(paste(".Table <-", command))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(".Table  # counts")
        doItAndPrint("100*.Table/sum(.Table)  # percentages")        
        env <- environment()
        if (goodnessOfFit == 1){
            subwin <- tktoplevel()
            tkwm.title(subwin, "Goodness-of-Fit Test")
            hypothesisFrame <- tkframe(subwin)
            levs <- eval(parse(text=paste("levels(", .activeDataSet, "$", x, ")", sep="")))
            n.levs <- length(levs)
            assign(".entry.1", tclVar(paste("1/", n.levs, sep="")), envir=env)
            make.entries <- "tklabel(hypothesisFrame, text='Hypothesized probabilities', fg='blue')"
            make.lev.names <- "tklabel(hypothesisFrame, text='Factor levels', fg='blue')"
            for (i in 1:n.levs) {
                entry.varname <- paste(".entry.", i, sep="")
                assign(entry.varname, tclVar(paste("1/", n.levs, sep="")), envir=env)
                make.entries <- paste(make.entries, ", ", "tkentry(hypothesisFrame, width='5', textvariable=", 
                        entry.varname, ")", sep="")
                make.lev.names <- paste(make.lev.names, ", tklabel(hypothesisFrame, text='", levs[i], "')", sep="")
                }
            eval(parse(text=paste("tkgrid(", make.lev.names, ", sticky='w')", sep="")), envir=env)
            eval(parse(text=paste("tkgrid(", make.entries, ", stick='w')", sep="")), envir=env)
            tkgrid(hypothesisFrame, sticky="w")
            onOKsub <- function(){
                probs <- rep(NA, n.levs)
                for (i in 1:n.levs){
                    entry.varname <- paste(".entry.", i, sep="")
                    probs[i] <- eval(parse(text=eval(parse(text=paste("tclvalue(", entry.varname,")", sep="")), envir=env)))
                    }
                probs <- na.omit(probs)
                if (length(probs) != n.levs){
                    tkmessageBox(message=paste("Number of valid entries (", length(probs), ")\n",
                        "not equal to number levels (", n.levs,").", 
                        sep=""), icon="error", type="ok")
                    if (.grab.focus) tkgrab.release(sub)
                    tkdestroy(subwin)
                    return()
                    }
                if (any(probs < 0)){
                    tkmessageBox(message="Negative probabilities not allowed.", icon="error", type="ok")
                    if (.grab.focus) tkgrab.release(subwin)
                    tkdestroy(subwin)
                    return()
                    }
                if (abs(sum(probs) - 1) > 0.001){
                    tkmessageBox(message="Probabilities rescaled to sum to 1.", icon="warning", type="ok")
                    probs <- probs/sum(probs)
                    }
                if (.grab.focus) tkgrab.release(subwin)
                tkdestroy(subwin)
                command <- paste("c(", paste(probs, collapse=","), ")", sep="")
                logger(paste(".Probs <-", command))
                assign(".Probs", justDoIt(command), envir=.GlobalEnv)
                doItAndPrint("chisq.test(.Table, p=.Probs)")
                logger("remove(.Probs)")
                remove(.Probs, envir=.GlobalEnv)
                }
            onCancelSub <- function() {
                if (.grab.focus) tkgrab.release(subwin)
                tkfocus(.commander)
                tkdestroy(subwin)  
                } 
            buttonFrameSub <- tkframe(subwin)
            OKbuttonSub <- tkbutton(buttonFrameSub, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
            cancelButtonSub <- tkbutton(buttonFrameSub, text="Cancel", fg="red", width="12",command=onCancelSub)
            tkgrid(OKbuttonSub, tklabel(buttonFrameSub, text="    "), cancelButtonSub, sticky="w")
            tkgrid(buttonFrameSub, sticky="w")
            for (row in 0:2) tkgrid.rowconfigure(subwin, row, weight=0)
            for (col in 0:0) tkgrid.columnconfigure(subwin, col, weight=0)
            .Tcl("update idletasks")
            tkwm.resizable(subwin, 0, 0)
            tkbind(subwin, "<Return>", onOKsub)
            if (.double.click) tkbind(subwin, "<Double-ButtonPress-1>", onOKsub)
            tkwm.deiconify(subwin)
            if (.grab.focus) tkgrab.set(subwin)
            tkfocus(subwin)
            tkwait.window(subwin)
            }            
        logger("remove(.Table)") 
        remove(.Table, envir=.GlobalEnv)  
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        } 
    buttonFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(table)
        }
    helpButton <- tkbutton(buttonFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")    
    tkgrid(tklabel(optionsFrame, text="Chi-square goodness-of-fit test"), goodnessOfFitCheckBox, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonFrame, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkbind(xBox, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

statisticsTable <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (length(.factors) == 0){
        tkmessageBox(message="There no factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Table of Statistics")
    variablesFrame <- tkframe(top)
    groupFrame <- tkframe(variablesFrame)
    responseFrame <- tkframe(variablesFrame)
    groupBox <- tklistbox(groupFrame, height=min(4, length(.factors)),
        selectmode="multiple", background="white", exportselection="FALSE")
    groupScroll <- tkscrollbar(groupFrame, repeatinterval=5, 
        command=function(...) tkyview(groupBox, ...))
    tkconfigure(groupBox, yscrollcommand=function(...) tkset(groupScroll, ...))
    for (group in .factors) tkinsert(groupBox, "end", group)
    responseBox <- tklistbox(responseFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    responseScroll <- tkscrollbar(responseFrame, repeatinterval=5, 
        command=function(...) tkyview(responseBox, ...))    
    tkconfigure(responseBox, yscrollcommand=function(...) tkset(responseScroll, ...))
    for (response in .numeric) tkinsert(responseBox, "end", response)
    statisticVariable <- tclVar("mean")
    otherVariable <- tclVar("")
    statisticFrame <- tkframe(top)
    meanButton <- tkradiobutton(statisticFrame, variable=statisticVariable, value="mean")
    medianButton <- tkradiobutton(statisticFrame, variable=statisticVariable, value="median")
    sdButton <- tkradiobutton(statisticFrame, variable=statisticVariable, value="sd")
    otherButton <- tkradiobutton(statisticFrame, variable=statisticVariable, value="other")
    otherEntry <- tkentry(statisticFrame, width="20", textvariable=otherVariable)   
    onOK <- function(){
        groups <- .factors[as.numeric(tkcurselection(groupBox)) + 1]
        if (0 == length(groups)) {
            tkmessageBox(message="No factors selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            statisticsTable()
            return()
            }
        response <- as.character(tkget(responseBox, "active"))
        statistic <- tclvalue(statisticVariable)
        if (statistic == "other") statistic <- tclvalue(otherVariable)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        groups.list <- paste(paste(groups, "=", .activeDataSet, "$", groups, sep=""), collapse=", ")
        doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", list(", groups.list,
             "), ", statistic, ", na.rm=TRUE)", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }  
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(tapply)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(variablesFrame, text="Factors (pick one or more)"),
        tklabel(variablesFrame, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid(groupFrame, responseFrame, sticky="nw")
    tkgrid(variablesFrame)
    tkgrid(tklabel(statisticFrame, text="Statistic", fg="blue"), sticky="w")
    tkgrid(tklabel(statisticFrame, text="Mean"), meanButton, sticky="w")
    tkgrid(tklabel(statisticFrame, text="Median"), medianButton, sticky="w")
    tkgrid(tklabel(statisticFrame, text="Standard deviation"), sdButton, sticky="w")
    tkgrid(tklabel(statisticFrame, text="Other (specify)"), otherButton, otherEntry, sticky="w")
    tkgrid(statisticFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
    
correlationMatrix <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) < 2){
        tkmessageBox(message="There fewer than 2 numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Correlation Matrix")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    correlationsVariable <- tclVar("Pearson")
    correlationsFrame <- tkframe(top)
    pearsonButton <- tkradiobutton(correlationsFrame, variable=correlationsVariable, value="Pearson")
    spearmanButton <- tkradiobutton(correlationsFrame, variable=correlationsVariable, value="Spearman")
    partialButton <- tkradiobutton(correlationsFrame, variable=correlationsVariable, value="partial")
    onOK <- function(){
        correlations <- tclvalue(correlationsVariable)
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        if (2 > length(x)) {
            tkmessageBox(message="Fewer than 2 variables selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            correlationMatrix()
            return()
            }
        if ((correlations == "partial") && (3 > length(x))) {
            tkmessageBox(message="Fewer than 3 variables selected\nfor partial correlations.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            correlationMatrix()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        x <- paste('"', x, '"', sep="")
        if (correlations == "Pearson")
            doItAndPrint(paste("cor(", .activeDataSet, "[,c(", paste(x, collapse=","),
                ')], use="complete.obs")', sep=""))
        else if (correlations == "Spearman"){
            logger("# Spearman rank-order correlations")
             doItAndPrint(paste("cor(apply(", .activeDataSet, "[,c(", paste(x, collapse=","),
                ')], 2, rank), use="complete.obs")', sep="")) 
             }
        else doItAndPrint(paste("partial.cor(", .activeDataSet, "[,c(", paste(x, collapse=","),
                ')], use="complete.obs")', sep=""))    
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(cor)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variables (pick two or more)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")
    tkgrid(tklabel(correlationsFrame, text="Type of Correlations", fg="blue"), sticky="w")
    tkgrid(tklabel(correlationsFrame, text="Pearson product-moment"), pearsonButton, sticky="w")
    tkgrid(tklabel(correlationsFrame, text="Spearman rank-order"), spearmanButton, sticky="w")
    tkgrid(tklabel(correlationsFrame, text="Partial"), partialButton, sticky="w")
    tkgrid(correlationsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, tklabel(top, text="    "), helpButton, sticky="w")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(xScroll, sticky="ns")
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
# Statistics Menu dialogs

# last modified 27 January 04 by J. Fox

    # Tables menu
    
twoWayTable <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.factors) < 2){
        tkmessageBox(message="There fewer than 2 factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Two-Way Table")
    rowFrame <- tkframe(top)
    columnFrame <- tkframe(top)
    rowBox <- tklistbox(rowFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    rowScroll <- tkscrollbar(rowFrame, repeatinterval=5, 
        command=function(...) tkyview(rowBox, ...))
    tkconfigure(rowBox, yscrollcommand=function(...) tkset(rowScroll, ...))
    for (var in .factors) tkinsert(rowBox, "end", var)
    columnBox <- tklistbox(columnFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    columnScroll <- tkscrollbar(columnFrame, repeatinterval=5, 
        command=function(...) tkyview(columnBox, ...))    
    tkconfigure(columnBox, yscrollcommand=function(...) tkset(columnScroll, ...))
    for (var in .factors) tkinsert(columnBox, "end", var)
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    onOK <- function(){
        row <- as.character(tkget(rowBox, "active"))
        column <- as.character(tkget(columnBox, "active"))
        percents <- as.character(tclvalue(percentsVariable))
        chisq <- tclvalue(chisqTest)
        expected <- tclvalue(expFreq)
        fisher <- tclvalue(fisherTest)
        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == "<all valid cases>") "" 
            else paste(", subset=", subset, sep="")
        if (row == column) {
            tkmessageBox(message="Row and column variables are the same.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            twoWayTable()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("xtabs(~", row, "+", column, ", data=", .activeDataSet, 
            subset, ")", sep="")
        logger(paste(".Table <- ", command, sep=""))
        logger(".Table")
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        print(.Table)
        cat("\n")
        if (percents == "row") doItAndPrint("rowPercents(.Table) # Row Percentages")
        if (percents == "column") doItAndPrint("colPercents(.Table) # Column Percentages")
        cat("\n")
        if (chisq == 1) {
            command <- "chisq.test(.Table, correct=FALSE)"
            logger(paste(".Test <- ", command, sep=""))
            assign(".Test", justDoIt(command), envir=.GlobalEnv)
            doItAndPrint(".Test")
            if (expected == 1) doItAndPrint(".Test$expected # Expected Counts")
            warnText <- NULL
            if (0 < (nlt1 <- sum(.Test$expected < 1))) warnText <- paste(nlt1,
                "expected frequencies are less than 1")
            if (0 < (nlt5 <- sum(.Test$expected < 1))) warnText <- paste(warnText, "\n", nlt5,
                " expected frequencies are less than 5", sep="")
            if (!is.null(warnText)) tkmessageBox(message=warnText,
                icon="warning", type="ok")
            logger("remove(.Test)") 
            remove(.Test, envir=.GlobalEnv) 
            }
        if (fisher == 1) doItAndPrint("fisher.test(.Table)")
        logger("remove(.Table)") 
        remove(.Table, envir=.GlobalEnv)                                                      
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(xtabs)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    percentsVariable <- tclVar("none")
    percentsFrame <- tkframe(top)
    rowPercentsButton <- tkradiobutton(percentsFrame, variable=percentsVariable, value="row")
    columnPercentsButton <- tkradiobutton(percentsFrame, variable=percentsVariable, value="column")
    nonePercentsButton <- tkradiobutton(percentsFrame, variable=percentsVariable, value="none")
    chisqTest <- tclVar("1")
    expFreq <- tclVar("0")
    fisherTest <- tclVar("0")
    testsFrame <- tkframe(top)
    chisqCheckBox <- tkcheckbutton(testsFrame, variable=chisqTest)
    expFreqCheckBox <- tkcheckbutton(testsFrame, variable=expFreq)
    fisherCheckBox <- tkcheckbutton(testsFrame, variable=fisherTest)
    tkgrid(tklabel(top, text="Row variable (pick one)"), 
        tklabel(top, text="Column variable (pick one)"), sticky="w")
    tkgrid(rowBox, rowScroll, sticky="nw")
    tkgrid(columnBox, columnScroll, sticky="nw")
    tkgrid(rowFrame, columnFrame, sticky="nw")
    tkgrid(tklabel(percentsFrame, text="Compute Percentages", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(percentsFrame, text="Row percentages"), rowPercentsButton, sticky="w")
    tkgrid(tklabel(percentsFrame, text="Column percentages"), columnPercentsButton, sticky="w")
    tkgrid(tklabel(percentsFrame, text="No percentages"), nonePercentsButton, sticky="w")
    tkgrid(percentsFrame, sticky="w")
    tkgrid(tklabel(testsFrame, text="Hypothesis Tests", fg="blue"), sticky="w")
    tkgrid(tklabel(testsFrame, text="Chisquare test of independence"), chisqCheckBox, sticky="e")
    tkgrid(tklabel(testsFrame, text="Print expected frequencies"), expFreqCheckBox, sticky="e")
    tkgrid(tklabel(testsFrame, text="Fisher's exact test"), fisherCheckBox, sticky="e")
    tkgrid(testsFrame)
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(chisqCheckBox, sticky="w")
    tkgrid.configure(fisherCheckBox, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(rowScroll, sticky="ns")
    tkgrid.configure(columnScroll, sticky="ns")
    for (row in 0:6) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(rowBox, 0)
    tkselection.set(columnBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

multiWayTable <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.factors) < 3){
        tkmessageBox(message="There fewer than 3 factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Multi-Way Table")
    rowFrame <- tkframe(top)
    columnFrame <- tkframe(top)
    controlFrame <- tkframe(top)
    rowBox <- tklistbox(rowFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    rowScroll <- tkscrollbar(rowFrame, repeatinterval=5, 
        command=function(...) tkyview(rowBox, ...))
    tkconfigure(rowBox, yscrollcommand=function(...) tkset(rowScroll, ...))
    for (var in .factors) tkinsert(rowBox, "end", var)
    columnBox <- tklistbox(columnFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    columnScroll <- tkscrollbar(columnFrame, repeatinterval=5, 
        command=function(...) tkyview(columnBox, ...))   
    tkconfigure(columnBox, yscrollcommand=function(...) tkset(columnScroll, ...))
    for (var in .factors) tkinsert(columnBox, "end", var)
    controlBox <- tklistbox(controlFrame, height=min(4, length(.factors)),
        selectmode="multiple", background="white", exportselection="FALSE")
    controlScroll <- tkscrollbar(controlFrame, repeatinterval=5, 
        command=function(...) tkyview(controlBox, ...))    
    tkconfigure(controlBox, yscrollcommand=function(...) tkset(controlScroll, ...))
    for (var in .factors) tkinsert(controlBox, "end", var)
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    onOK <- function(){
        row <- as.character(tkget(rowBox, "active"))
        column <- as.character(tkget(columnBox, "active"))
        controls <- .factors[as.numeric(tkcurselection(controlBox)) + 1]
        if (length(controls) == 0) {
            tkmessageBox(message="No control variable(s) specified",
                icon="error", type="ok")
            tkdestroy(top)
            multiWayTable()
            return()
            }
        if ((row == column) || is.element(row, controls) || is.element(column, controls)) {
            tkmessageBox(message="Row, column, and control variables must be different.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            multiWayTable()
            return()
            }

        percents <- as.character(tclvalue(percentsVariable))
        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == "<all valid cases>") "" 
            else paste(", subset=", subset, sep="")
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("xtabs(~", row, "+", column, "+", paste(controls, collapse="+"),
            ", data=", .activeDataSet, subset, ")", sep="")
        logger(paste(".Table <- ", command, sep=""))
        logger(".Table")
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        print(.Table)
        cat("\n")
        if (percents == "row") doItAndPrint("rowPercents(.Table) # Row Percentages")
        if (percents == "column") doItAndPrint("colPercents(.Table) # Column Percentages")
        logger("remove(.Table)") 
        remove(.Table, envir=.GlobalEnv)                                             
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        } 
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(xtabs)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    percentsVariable <- tclVar("none")
    percentsFrame <- tkframe(top)
    rowPercentsButton <- tkradiobutton(percentsFrame, variable=percentsVariable, value="row")
    columnPercentsButton <- tkradiobutton(percentsFrame, variable=percentsVariable, value="column")
    nonePercentsButton <- tkradiobutton(percentsFrame, variable=percentsVariable, value="none")
    tkgrid(tklabel(top, text="Row variable (pick one)"), 
        tklabel(top, text="Column variable (pick one)"), 
        tklabel(top, text="Control variable(s) (pick one or more)"), sticky="w")
    tkgrid(rowBox, rowScroll, sticky="nw")
    tkgrid(columnBox, columnScroll, sticky="nw")
    tkgrid(controlBox, controlScroll, sticky="nw")
    tkgrid(rowFrame, columnFrame, controlFrame, sticky="nw")
    tkgrid(tklabel(percentsFrame, text="Compute Percentages", fg="blue"), columnspan=3, sticky="w")
    tkgrid(tklabel(percentsFrame, text="Row percentages"), rowPercentsButton, sticky="w")
    tkgrid(tklabel(percentsFrame, text="Column percentages"), columnPercentsButton, sticky="w")
    tkgrid(tklabel(percentsFrame, text="No percentages"), nonePercentsButton, sticky="w")
    tkgrid(percentsFrame, sticky="w")
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(rowScroll, sticky="ns")
    tkgrid.configure(columnScroll, sticky="ns")
    tkgrid.configure(controlScroll, sticky="ns")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:2) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(rowBox, 0)
    tkselection.set(columnBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

enterTable <- function(){
    env <- environment()
    top <- tktoplevel()
    tkwm.title(top, "Enter and Analyze Two-Way Table")
    outerTableFrame <- tkframe(top)
    assign(".tableFrame", tkframe(outerTableFrame), envir=env)
    setUpTable <- function(...){
        tkdestroy(get(".tableFrame", envir=env))
        assign(".tableFrame", tkframe(outerTableFrame), envir=env)
        nrows <- as.numeric(tclvalue(rowsValue))
        ncols <- as.numeric(tclvalue(colsValue))
        make.col.names <- "tklabel(.tableFrame, text='')"
        for (j in 1:ncols) {
            col.varname <- paste(".colname.", j, sep="")
            assign(col.varname, tclVar(j), envir=env)
            make.col.names <- paste(make.col.names, ", ", "tkentry(.tableFrame, width='5', textvariable=", 
                    col.varname, ")", sep="")
            }
        eval(parse(text=paste("tkgrid(", make.col.names, ")", sep="")), envir=env)
        for (i in 1:nrows){   
            varname <- paste(".tab.", i, ".1", sep="") 
            assign(varname, tclVar("") , envir=env)
            row.varname <- paste(".rowname.", i, sep="")
            assign(row.varname, tclVar(i), envir=env)
            make.row <- paste("tkentry(.tableFrame, width='5', textvariable=",
                row.varname, ")", sep="")
            make.row <- paste(make.row, ", ", "tkentry(.tableFrame, width='5', textvariable=", 
                varname, ")", sep="")
            for (j in 2:ncols){
                varname <- paste(".tab.", i, ".", j, sep="")
                assign(varname, tclVar(""), envir=env)
                make.row <- paste(make.row, ", ", "tkentry(.tableFrame, width='5', textvariable=", 
                    varname, ")", sep="")
                }
            eval(parse(text=paste("tkgrid(", make.row, ")", sep="")), envir=env)
            }
        tkgrid(get(".tableFrame", envir=env), sticky="w")
        }
    rowColFrame <- tkframe(top)
    rowsValue <- tclVar("2")
    rowsSlider <- tkscale(rowColFrame, from=2, to=10, showvalue=FALSE, variable=rowsValue,
        resolution=1, orient="horizontal", command=setUpTable)
    rowsShow <- tklabel(rowColFrame, textvariable=rowsValue, width=2, justify="right")
    colsValue <- tclVar("2")
    colsSlider <- tkscale(rowColFrame, from=2, to=10, showvalue=FALSE, variable=colsValue,
        resolution=1, orient="horizontal", command=setUpTable)
    colsShow <- tklabel(rowColFrame, textvariable=colsValue, width=2, justify="right")
    onOK <- function(){
        nrows <- as.numeric(tclvalue(rowsValue))
        ncols <- as.numeric(tclvalue(colsValue))
        cell <- 0
        counts <- rep(NA, nrows*ncols)
        row.names <- rep("", nrows)
        col.names <- rep("", ncols)
        for (i in 1:nrows) row.names[i] <- 
            eval(parse(text=paste("tclvalue(", paste(".rowname.", i, sep=""),")", sep="")))
        for (j in 1:ncols) col.names[j] <- 
            eval(parse(text=paste("tclvalue(", paste(".colname.", j, sep=""),")", sep="")))
        for (i in 1:nrows){
            for (j in 1:ncols){
                cell <- cell+1
                varname <- paste(".tab.", i, ".", j, sep="")
                counts[cell] <- as.numeric(eval(parse(text=paste("tclvalue(", varname,")", sep=""))))
                }
            }
        counts <- na.omit(counts)
        if (length(counts) != nrows*ncols){
            tkmessageBox(message=paste("Number of valid entries (", length(counts), ")\n",
                "not equal to number of rows (", nrows,") * number of columns (", ncols,").", 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            enterTable()
            return()
            }
        if (length(unique(row.names)) != nrows){
            tkmessageBox(message="Row names are not unique.", icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            enterTable()
            return()
            }     
        if (length(unique(col.names)) != ncols){
            tkmessageBox(message="Column names are not unique.", icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            enterTable()
            return()
            }     
        percents <- as.character(tclvalue(percentsVariable))
        chisq <- tclvalue(chisqTest)
        expected <- tclvalue(expFreq)
        fisher <- tclvalue(fisherTest)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("matrix(c(", paste(counts, collapse=","), "), ", nrows, ", ", ncols,
            ", byrow=TRUE)", sep="")
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".Table <- ", command, sep=""))
        command <- paste("c(",paste(paste("'", row.names, "'", sep=""), collapse=", "), ")", sep="")
#        justDoIt(paste("rownames(.Table) <<- ", command, sep=""))
        justDoIt(paste("rownames(.Table) <- ", command, sep=""))
        logger(paste("rownames(.Table) <- ", command, sep=""))
        command <- paste("c(",paste(paste("'", col.names, "'", sep=""), collapse=", "), ")", sep="")
#        justDoIt(paste("colnames(.Table) <<- ", command, sep=""))
        justDoIt(paste("colnames(.Table) <- ", command, sep=""))
        logger(paste("colnames(.Table) <- ", command, sep=""))
        logger(".Table  # Counts")
        print(.Table)
        cat("\n")
        if (percents == "row") doItAndPrint("rowPercents(.Table) # Row Percentages")
        if (percents == "column") doItAndPrint("colPercents(.Table) # Column Percentages")
        cat("\n")
        if (chisq == 1) {
            command <- "chisq.test(.Table, correct=FALSE)"
            logger(paste(".Test <- ", command, sep=""))
            assign(".Test", justDoIt(command), envir=.GlobalEnv)
            doItAndPrint(".Test")
            if (expected == 1) doItAndPrint(".Test$expected # Expected Counts")
            warnText <- NULL
            if (0 < (nlt1 <- sum(.Test$expected < 1))) warnText <- paste(nlt1,
                "expected frequencies are less than 1")
            if (0 < (nlt5 <- sum(.Test$expected < 1))) warnText <- paste(warnText, "\n", nlt5,
                " expected frequencies are less than 5", sep="")
            if (!is.null(warnText)) tkmessageBox(message=warnText,
                icon="warning", type="ok")
            logger("remove(.Test)") 
            remove(.Test, envir=.GlobalEnv) 
            }
        if (fisher == 1) doItAndPrint("fisher.test(.Table)")
        logger("remove(.Table)") 
        remove(.Table, envir=.GlobalEnv)                                                      
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(chisq.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    percentsVariable <- tclVar("none")
    percentsFrame <- tkframe(top)
    rowPercentsButton <- tkradiobutton(percentsFrame, variable=percentsVariable, value="row")
    columnPercentsButton <- tkradiobutton(percentsFrame, variable=percentsVariable, value="column")
    nonePercentsButton <- tkradiobutton(percentsFrame, variable=percentsVariable, value="none")
    chisqTest <- tclVar("1")
    expFreq <- tclVar("0")
    fisherTest <- tclVar("0")
    testsFrame <- tkframe(top)
    chisqCheckBox <- tkcheckbutton(testsFrame, variable=chisqTest)
    expFreqCheckBox <- tkcheckbutton(testsFrame, variable=expFreq)
    fisherCheckBox <- tkcheckbutton(testsFrame, variable=fisherTest)
    tkgrid(tklabel(rowColFrame, text="Number of Rows:"), rowsSlider, rowsShow, sticky="w")
    tkgrid(tklabel(rowColFrame, text="Number of Columns:"), colsSlider, colsShow, sticky="w")
    tkgrid(rowColFrame, sticky="w")
    tkgrid(tklabel(top, text="Enter counts:", fg="blue"), sticky="w")
    tkgrid(outerTableFrame, sticky="w")
    tkgrid(tklabel(percentsFrame, text="Compute Percentages", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(percentsFrame, text="Row percentages"), rowPercentsButton, sticky="w")
    tkgrid(tklabel(percentsFrame, text="Column percentages"), columnPercentsButton, sticky="w")
    tkgrid(tklabel(percentsFrame, text="No percentages"), nonePercentsButton, sticky="w")
    tkgrid(percentsFrame, sticky="w")
    tkgrid(tklabel(testsFrame, text="Hypothesis Tests", fg="blue"), sticky="w")
    tkgrid(tklabel(testsFrame, text="Chisquare test of independence"), chisqCheckBox, sticky="e")
    tkgrid(tklabel(testsFrame, text="Print expected frequencies"), expFreqCheckBox, sticky="e")
    tkgrid(tklabel(testsFrame, text="Fisher's exact test"), fisherCheckBox, sticky="e")
    tkgrid(testsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(chisqCheckBox, sticky="w")
    tkgrid.configure(fisherCheckBox, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)        
    } 
# Statistics Menu dialogs

# last modified 27 Jan 04 by J. Fox

    # Variances menu
    
twoVariancesFTest <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There are no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (length(.twoLevelFactors) == 0){
        tkmessageBox(message="There are no 2-level factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Two Variances F-Test")
    groupFrame <- tkframe(top)
    responseFrame <- tkframe(top)
    groupBox <- tklistbox(groupFrame, height=min(4, length(.twoLevelFactors)),
        selectmode="single", background="white", exportselection="FALSE")
    groupScroll <- tkscrollbar(groupFrame, repeatinterval=5, 
        command=function(...) tkyview(groupBox, ...))
    tkconfigure(groupBox, yscrollcommand=function(...) tkset(groupScroll, ...))
    for (group in .twoLevelFactors) tkinsert(groupBox, "end", group)
    responseBox <- tklistbox(responseFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    responseScroll <- tkscrollbar(responseFrame, repeatinterval=5, 
        command=function(...) tkyview(responseBox, ...))    
    tkconfigure(responseBox, yscrollcommand=function(...) tkset(responseScroll, ...))
    for (response in .numeric) tkinsert(responseBox, "end", response)
    onOK <- function(){
        group <- as.character(tkget(groupBox, "active"))
        response <- as.character(tkget(responseBox, "active"))
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", ", 
            .activeDataSet, "$", group, ",  var, na.rm=TRUE)", sep=""))
        doItAndPrint(paste("var.test(", response, " ~ ", group,
            ", alternative='", alternative, "', conf.level=", level,
            ", data=", .activeDataSet, ")", sep=""))
        tkfocus(.commander)
        tkdestroy(top)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }    
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(var.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    alternativeFrame <- tkframe(top)
    alternativeVariable <- tclVar("two.sided")
    twosidedButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="two.sided")
    lessButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="less")
    greaterButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="greater")
    confidenceFrame <- tkframe(top)
    confidenceLevel <- tclVar(".95")
    confidenceField <- tkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    tkgrid(tklabel(top, text="Groups (pick one)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid(groupFrame, responseFrame, sticky="nw")
    tkgrid(tklabel(alternativeFrame, text="Alternative Hypothesis", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Two-sided"), twosidedButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference < 0"), lessButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference > 0"), greaterButton, sticky="w")
    tkgrid(tklabel(confidenceFrame, text="Confidence Level"))
    tkgrid(confidenceField)
    tkgrid(alternativeFrame, confidenceFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(groupBox, 0)
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

BartlettTest <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There are no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (length(.factors) == 0){
        tkmessageBox(message="There are no factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Bartlett's Test")
    groupFrame <- tkframe(top)
    responseFrame <- tkframe(top)
    groupBox <- tklistbox(groupFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    groupScroll <- tkscrollbar(groupFrame, repeatinterval=5, 
        command=function(...) tkyview(groupBox, ...))
    tkconfigure(groupBox, yscrollcommand=function(...) tkset(groupScroll, ...))
    for (group in .factors) tkinsert(groupBox, "end", group)
    responseBox <- tklistbox(responseFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    responseScroll <- tkscrollbar(responseFrame, repeatinterval=5, 
        command=function(...) tkyview(responseBox, ...))    
    tkconfigure(responseBox, yscrollcommand=function(...) tkset(responseScroll, ...))
    for (response in .numeric) tkinsert(responseBox, "end", response)
    onOK <- function(){
        group <- as.character(tkget(groupBox, "active"))
        response <- as.character(tkget(responseBox, "active"))
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("tapply(", paste(.activeDataSet, "$", response, sep=""),
            ", ", paste(.activeDataSet, "$", group, sep=""), ", var, na.rm=TRUE)", sep=""))
        doItAndPrint(paste("bartlett.test(", response, " ~ ", group, ", data=",
            .activeDataSet, ")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(bartlett.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Groups (pick one)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid(groupFrame, responseFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(groupBox, 0)
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

LeveneTest <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There are no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (length(.factors) == 0){
        tkmessageBox(message="There are no factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Levene's Test")
    groupFrame <- tkframe(top)
    responseFrame <- tkframe(top)
    groupBox <- tklistbox(groupFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    groupScroll <- tkscrollbar(groupFrame, repeatinterval=5, 
        command=function(...) tkyview(groupBox, ...))
    tkconfigure(groupBox, yscrollcommand=function(...) tkset(groupScroll, ...))
    for (group in .factors) tkinsert(groupBox, "end", group)
    responseBox <- tklistbox(responseFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    responseScroll <- tkscrollbar(responseFrame, repeatinterval=5, 
        command=function(...) tkyview(responseBox, ...))    
    tkconfigure(responseBox, yscrollcommand=function(...) tkset(responseScroll, ...))
    for (response in .numeric) tkinsert(responseBox, "end", response)
    onOK <- function(){
        group <- as.character(tkget(groupBox, "active"))
        response <- as.character(tkget(responseBox, "active"))
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("tapply(", paste(.activeDataSet, "$", response, sep=""),
            ", ", paste(.activeDataSet, "$", group, sep=""), ", var, na.rm=TRUE)", sep=""))
        doItAndPrint(paste("levene.test(", paste(.activeDataSet, "$", response, sep=""), 
            ", ", paste(.activeDataSet, "$", group, sep=""), ")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(levene.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Groups (pick one)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid(groupFrame, responseFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(groupBox, 0)
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
# last modified 28 Mar 04 by J. Fox

# utility functions

listDataSets <- function(envir=.GlobalEnv, ...) {
    names(which(sapply(ls(envir=envir, all.names=TRUE, ...), 
        function(.x) is.data.frame(eval(parse(text=.x), envir=envir)))))
    }

listLinearModels <- function(envir=.GlobalEnv, ...) {
    objects <- ls(envir=envir, ...)
    if (length(objects) == 0) NULL
    else objects[sapply(objects, 
        function(.x) "lm" == (class(eval(parse(text=.x), envir=envir))[1]))]
    }

listGeneralizedLinearModels <- function(envir=.GlobalEnv, ...) {
    objects <- ls(envir=envir, ...)
    if (length(objects) == 0) NULL
    else objects[sapply(objects, 
        function(.x) "glm" == (class(eval(parse(text=.x), envir=envir))[1]))]
    }
    
activeDataSet <- function(dsname){
    if (missing(dsname)) {
        if (is.null(.activeDataSet)){
            tkmessageBox(message="There is no active data set.", icon="error", type="ok")
            return(FALSE)
            }
        else return(.activeDataSet)
        }
    if (!is.data.frame(get(dsname, envir=.GlobalEnv))){
        tkmessageBox(message=paste(dsname, " is not a data frame and cannot be attached.",
            sep=""), icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (!is.null(.activeDataSet) && (tclvalue(.attachDataSet) == "1") 
        && (length(grep(.activeDataSet, search())) !=0)) {
        detach(pos = match(.activeDataSet, search()))
        logger(paste("detach(", .activeDataSet, ")", sep=""))
        }
    assign(".activeModel", NULL, envir=.GlobalEnv)
    tclvalue(.modelName) <- "<No active model>"
    tkconfigure(.modelLabel, fg="red")
    assign(".activeDataSet", dsname, envir=.GlobalEnv)
    assign(".variables", listVariables(), envir=.GlobalEnv)
    assign(".numeric", listNumeric(), envir=.GlobalEnv)
    assign(".factors", listFactors(), envir=.GlobalEnv)
    assign(".twoLevelFactors", listTwoLevelFactors(), envir=.GlobalEnv)
    tclvalue(.dataSetName) <- paste(.activeDataSet, " ")
    tkconfigure(.dataSetLabel, fg="blue")
    if (tclvalue(.attachDataSet) == "1"){
        attach(get(dsname, envir=.GlobalEnv), name=dsname)
        logger(paste("attach(", dsname, ")", sep=""))
        }
    dsname
    }


activeModel <- function(model){
    if (missing(model)) {
        if (is.null(.activeModel)){
            tkmessageBox(message="There is no active model.", icon="error", type="ok")
            return(FALSE)
            }
        else return(.activeModel)
        }
    assign(".activeModel", model, envir=.GlobalEnv)
    tclvalue(.modelName) <- .activeModel
    tkconfigure(.modelLabel, fg="blue")
    model
    }
    
listVariables <- function(dataSet=.activeDataSet) {
    vars <- eval(parse(text=paste("names(", dataSet,")")), envir=.GlobalEnv)
    if (.sort.names) sort(vars) else vars
    }

listFactors <- function(dataSet=.activeDataSet) {
    variables <- if (exists(".variables")) .variables else listVariables(dataSet)
    variables[sapply(variables, function(.x)
        is.factor(eval(parse(text=.x), envir=eval(parse(text=dataSet), envir=.GlobalEnv))))]
    }

listTwoLevelFactors <- function(dataSet=.activeDataSet){
    factors <- listFactors(dataSet)
    if(length(factors) == 0) return(NULL)
    factors[sapply(factors, function(.x)
        2 == length(levels(eval(parse(text=.x), envir=eval(parse(text=dataSet), 
            envir=.GlobalEnv)))))]
    }
    
listNumeric <- function(dataSet=.activeDataSet) {
    variables <- if (exists(".variables")) .variables else listVariables(dataSet)
    variables[sapply(variables,function(.x)
        is.numeric(eval(parse(text=.x), envir=eval(parse(text=dataSet), envir=.GlobalEnv))))]
    }

trim.blanks <- function(text){
    gsub("^\ ", "", gsub("\ *$", "", text))
    }
    
is.valid.name <- function(x){
    length(x) == 1 && is.character(x) && x == make.names(x)
    }

    
    # statistical
    
colPercents <- function(tab, digits=2){
    dim <- length(dim(tab))
    if (is.null(dimnames(tab))){
        dims <- dim(tab)
        dimnames(tab) <- lapply(1:dim, function(i) 1:dims[i])
        }
    sums <- apply(tab, 2:dim, sum)
    per <- apply(tab, 1, function(x) x/sums)
    dim(per) <- dim(tab)[c(2:dim,1)]
    per <- aperm(per, c(dim, 1:(dim-1)))
    dimnames(per) <- dimnames(tab)
    per <- round(100*per, digits)
    result <- abind(per, Total=apply(per, 2:dim, sum), Count=sums, along=1)
    names(dimnames(result)) <- names(dimnames(tab))
    result
    }

rowPercents <- function(tab, digits=2){
    dim <- length(dim(tab))
    if (dim == 2) return(t(colPercents(t(tab), digits=digits)))
    tab <- aperm(tab, c(2,1,3:dim))
    aperm(colPercents(tab, digits=digits), c(2,1,3:dim))
    }

# the following function slightly modified from Brian Ripley via R-help

levene.test <- function(y, group) {
    meds <- tapply(y, group, median, na.rm=TRUE)
    resp <- abs(y - meds[group])
    table <- anova(lm(resp ~ group))
    rownames(table)[2] <- " "
    cat("Levene's Test for Homogeneity of Variance\n\n")
    table[,c(1,4,5)]
    } 

# the following function adapted from Fox, An R and S-PLUS Companion to Applied Regression

influence.plot <- function(model, scale=10, col=c(1,2),
    labels=names(rstud), ...){
    hatval <- hatvalues(model)
    rstud <- rstudent(model)
    cook <- sqrt(cookd(model))
    scale <- scale/max(cook, na.rm=TRUE)
    p <- length(coef(model))
    n <- length(rstud)
    cutoff <- sqrt(4/(n - p))
    plot(hatval, rstud, xlab='Hat-Values',
        ylab='Studentized Residuals', type='n', ...)
    abline(v=c(2, 3)*p/n, lty=2)
    abline(h=c(-2, 0, 2), lty=2)
    points(hatval, rstud, cex=scale*cook, 
            col=ifelse(cook > cutoff, col[2], col[1]))
    if (labels[1] != FALSE) identify(hatval, rstud, labels)
    }



reliability <- function(S){
    reliab <- function(S, R){
        k <- dim(S)[1]
        ones <- rep(1, k)
        v <- as.vector(ones %*% S %*% ones)
        alpha <- (k/(k - 1)) * (1 - (1/v)*sum(diag(S)))
        rbar <- mean(R[lower.tri(R)])
        std.alpha <- k*rbar/(1 + (k - 1)*rbar)
        c(alpha=alpha, std.alpha=std.alpha)
        }
    k <- dim(S)[1]
    s <- sqrt(diag(S))
    R <- S/(s %o% s)
    rel <- reliab(S, R)
    cat(paste("Alpha reliability = ", round(rel[1], 4), "\n"))
    cat(paste("Standardized alpha = ", round(rel[2], 4), "\n"))
    cat("\nReliability deleting each item in turn:\n")
    rel <- matrix(0, k, 3)
    for (i in 1:k) {
        rel[i, c(1,2)] <- reliab(S[-i, -i], R[-i, -i])
        a <- rep(0, k)
        b <- rep(1, k)
        a[i] <- 1
        b[i] <- 0
        cov <- a %*% S %*% b
        var <- b %*% S %*% b
        rel[i, 3] <- cov/(sqrt(var * S[i,i]))
        }
    rownames(rel) <- rownames(S)
    colnames(rel) <- c("Alpha", "Std.Alpha", "r(item, total)")
    round(rel, 4)
    }
    
partial.cor <- function(X, ...){
    R <- cor(X, ...)
    RI <- solve(R)
    D <- diag(1/sqrt(diag(RI)))
    R <- -D %*% RI %*% D
    diag(R) <- 0
    rownames(R) <- colnames(R) <- colnames(X)
    R
    }


    # wrapper function for histograms

Hist <- function(x, scale=c("frequency", "percent", "density"), ...){
    xlab <- deparse(substitute(x))
    x <- na.omit(x)
    scale <- match.arg(scale)
    if (scale == "frequency") hist(x, xlab=xlab, main="",  ...)
    else if (scale == "density") hist(x, freq=FALSE, xlab=xlab, main="", ...)
    else {
        n <- length(x)
        hist(x, axes=FALSE, xlab=xlab, ylab="Percent", main="", ...)
        axis(1)
        max <- ceiling(10*par("usr")[4]/n)
        at <- if (max <= 3) (0:(2*max))/20
                else (0:max)/10
        axis(2, at=at*n, labels=at*100)
        }  
    box()   
    abline(h=0, col="gray") 
    invisible(NULL)
    }

stem.leaf <- function(data, unit, m, Min, Max, rule.line=c("Dixon", "Velleman", "Sturges"),
     style=c("Tukey", "bare"), trim.outliers=TRUE, depths=TRUE, reverse.negative.leaves=TRUE,
     print=TRUE){
#Author:  Peter Wolf 05/2003  (modified slightly by J. Fox, 20 July 03)
    rule.line <- match.arg(rule.line)
    style <- match.arg(style)
    n <- length(data<-sort(data))
    row.max <- floor(  c(Dixon   =10*log(n,10),
                        Velleman=2*sqrt(n),
                        Sturges =1+log(n,2)        ))[rule.line]
    stats <- boxplot(data, plot=FALSE)
    if(missing(Min)) Min <- if (trim.outliers) stats$stats[1,1] else min(data, na.rm=TRUE)
    if(missing(Max)) Max <- if (trim.outliers) stats$stats[5,1] else max(data, na.rm=TRUE)
    spannweite.red <- Max - Min
    zeilen.intervall.laenge<-spannweite.red / row.max
    factor <- if(missing(unit)) 10^ceiling(log(zeilen.intervall.laenge,10))
                else 10^round(log(unit*10,10))
    z <- zeilen.intervall.laenge/factor  # z in (0.1 ,1]
    delta.tick <- c(.2,.2,.5,1)[sum(z > c(0,.1,.2,.5))]
    if(missing(m)) m <- round(1/delta.tick) else delta.tick <- 1/m
    data.tr <- data/factor
    Min.tr <- Min/factor
    Max.tr <- Max/factor
    spannweite.red <- Max.tr - Min.tr
    sk.min <-  floor(Min.tr)
    sk.max <- ceiling(Max.tr)
    skala <- seq(sk.min, sk.max, by=delta.tick)
    if(sk.min < 0) skala <- c(sk.min-delta.tick, skala)
    if(sk.max < 0) skala <- skala[-length(skala)]
    lo.limit <- if (trim.outliers) skala[1] else -Inf
    lo.log   <- if(skala[1] <  0) data.tr <= lo.limit else data.tr <  lo.limit
    n.sk <- length(skala)
    hi.limit <- if (trim.outliers) skala[n.sk] + delta.tick else Inf
    hi.log   <- if(skala[n.sk] >= 0) data.tr >= hi.limit else data.tr >  hi.limit
    n.lower.extr.values <- sum(lo.log); n.upper.extr.values <- sum(hi.log)
    if(0 < n.lower.extr.values){
        lower.line<- paste("LO:", paste(data[lo.log],collapse=" "))
        }
    if(0 < n.upper.extr.values){
        upper.line<- paste("HI:", paste(data[hi.log],collapse=" "))
        }
    data.tr.red <-data.tr[(!lo.log)&(!hi.log)]
    stem <- ifelse(data.tr.red < 0, ceiling(data.tr.red), floor(data.tr.red) )
    leaf <- floor(abs(data.tr.red*10 - stem*10))
    class.of.data.tr <- unlist(c(
        sapply(data.tr.red[data.tr.red < 0],
            function(x, sk) length(sk) - sum(-sk <= -x), skala)
            ,sapply(data.tr.red[data.tr.red>=0],
            function(x,sk) sum(sk <= x), skala)
        ))
    class.of.data.tr  <- c(1:length(skala), class.of.data.tr)
    class.negative <- skala < 0
    leaf.grouped      <- split(c(rep(-1, length(skala)), leaf), class.of.data.tr)
    leaf.grouped      <- lapply(leaf.grouped, function(x){ sort(x[-1]) })
    if (reverse.negative.leaves){
        for (i in seq(class.negative))
            if (class.negative[i]) leaf.grouped[[i]] <- rev(leaf.grouped[[i]])
        }
    leaf.grouped.ch <- paste("|",unlist(lapply(leaf.grouped,paste,collapse="")))
    class.neg.zero <- floor(skala) == -1
    line.names <- skala
    line.names[class.negative] <- line.names[class.negative] + 1
    line.names <- as.character(floor(line.names))
    line.names[class.neg.zero] <- "-0"
    if(style=="Tukey"){
        switch(as.character(m),
        "1"={},
        "2"={
                h<-round(2*(skala%%1)) #; line.names[h!=0] <- ""
                line.names<-paste(line.names,
                        ifelse(skala<0,c(".","*")[1+h],c("*",".")[1+h]),sep="")
            },
        "5"={
                h<-round(5*(skala%%1)); line.names[h>0 & h<4] <- ""
                line.names<-paste(line.names, ifelse(skala<0,
                                c(".","s","f","t","*")[1+h],
                                c("*","t","f","s",".")[1+h]), sep="")
            }
            )
        }
    ragged.left<-function(ch.lines){
        max.n <-max(n.lines<-nchar(ch.lines))
        h     <-paste(rep(" ",max.n),collapse="")
        ch.lines <-paste( substring(h,1,1+max.n-n.lines), ch.lines)
        ch.lines
        }
    line.names <- ragged.left(line.names)
    n.class <- unlist(lapply(leaf.grouped, length))
    select <- (cumsum(n.class) > 0) & rev((cumsum(rev(n.class)) > 0))
    depth    <-    cumsum(n.class)          + n.lower.extr.values
    depth.rev <- rev(cumsum(rev(n.class))     + n.upper.extr.values)
    uplow <- depth >= depth.rev
    pos.median <- which(uplow)[1] + (-1:0)
    h <- abs(depth[pos.median]-depth.rev[pos.median])
    pos.median <- pos.median[1]+(h[1]>h[2])
    depth[uplow] <- depth.rev[uplow]
    depth <- paste(depth,"")
    depth[pos.median] <- paste("(",n.class[pos.median],")",sep="")
    depth[n.class == 0] <- " "
    depth <- if (depths) ragged.left(depth) else ""
    info<-     c(  paste("1 | 2: represents",1.2*factor),
                paste(" leaf unit:",factor/10),
                paste("         n:",n     ),
                "")
    stem <- paste(depth, line.names, leaf.grouped.ch)
    if ((style != "Tukey") || (m != 5) || (sum(select) > 4)) stem <- stem[select]
    if(exists("lower.line")) stem <- c(lower=lower.line, stem)
    if(exists("upper.line")) stem <- c(stem, upper=upper.line)
    result <- list(info=info, stem=stem)
    if (print){
        for(i in seq(result)) cat(result[[i]],sep="\n")
        invisible(NULL)
        }
    else result
    }

plotMeans <- function(response, factor1, factor2, error.bars = c("se", "sd", "conf.int", "none"),
    level=0.95, xlab=deparse(substitute(factor1)), ylab=paste("mean of", deparse(substitute(response))), 
    legend.lab=deparse(substitute(factor2)), main="Plot of Means",
    pch=1:n.levs.2, lty=1:n.levs.2, col=palette()){
    if (!is.numeric(response)) stop("Argument response must be numeric.")
    xlab # force evaluation
    ylab
    legend.lab
    error.bars <- match.arg(error.bars)
    if (missing(factor2)){
        if (!is.factor(factor1)) stop("Argument factor1 must be a factor.")
        valid <- !(is.na(factor1) | is.na(response))
        factor1 <- factor1[valid]
        response <- response[valid]
        means <- tapply(response, factor1, mean)
        sds <- tapply(response, factor1, mean)
        ns <- tapply(response, factor1, length)
        if (error.bars == "se") sds <- sds/sqrt(ns)
        if (error.bars == "conf.int") sds <- qt((1 - level)/2, df=ns - 1, lower.tail=FALSE) * sds/sqrt(ns)
        yrange <-  if (error.bars != "none") c( min(means - sds), max(means + sds)) else range(means)
        levs <- levels(factor1)
        n.levs <- length(levs)
        plot(c(1, n.levs), yrange, type="n", xlab=xlab, ylab=ylab, axes=FALSE, main=main)
        points(1:n.levs, means, type="b", pch=16, cex=2)
        box()
        axis(2)
        axis(1, at=1:n.levs, labels=levs)
        if (error.bars != "none") arrows(1:n.levs, means - sds, 1:n.levs, means + sds, 
            angle=90, lty=2, code=3, length=0.125)
        }
    else {
        if (!(is.factor(factor1) | is.factor(factor2))) stop("Arguments factor1 and factor2 must be factors.")
        valid <- !(is.na(factor1) | is.na(factor2) | is.na(response))
        factor1 <- factor1[valid]
        factor2 <- factor2[valid]
        response <- response[valid]
        means <- tapply(response, list(factor1, factor2), mean)
        sds <- tapply(response, list(factor1, factor2), mean)
        ns <- tapply(response, list(factor1, factor2), length)
        if (error.bars == "se") sds <- sds/sqrt(ns)
        if (error.bars == "conf.int") sds <- qt((1 - level)/2, df=ns - 1, lower.tail=FALSE) * sds/sqrt(ns)
        yrange <-  if (error.bars != "none") c( min(means - sds), max(means + sds)) else range(means)
        levs.1 <- levels(factor1)
        levs.2 <- levels(factor2)
        n.levs.1 <- length(levs.1)
        n.levs.2 <- length(levs.2)
        if (n.levs.2 > length(col)) stop(paste("Number of groups for factor2, ", n.levs.2,
            ", exceeds number of distinct colours, ", length(col), ".", sep=""))
        plot(c(1, n.levs.1 + 1), yrange, type="n", xlab=xlab, ylab=ylab, axes=FALSE, main=main)
        box()
        axis(2)
        axis(1, at=1:n.levs.1, labels=levs.1)
        for (i in 1:n.levs.2){
            points(1:n.levs.1, means[, i], type="b", pch=pch[i], cex=2, col=col[i], lty=lty[i])
            if (error.bars != "none") arrows(1:n.levs.1, means[, i] - sds[, i], 
                1:n.levs.1, means[, i] + sds[, i], angle=90, code=3, col=col[i], lty=lty[i], length=0.125)
            }
        x.posn <- n.levs.1 + 0.25
        y.posn <- sum(c(0.1, 0.9) * par("usr")[c(3,4)])
        text(x.posn, y.posn, legend.lab, adj=c(0, -.5))
        legend(x.posn, y.posn, levs.2, pch=pch, col=col, lty=lty)
        }
    invisible(NULL)
    }


# 3D scatterplots via rgl

scatter3d <- function(x, y, z, xlab=deparse(substitute(x)), ylab=deparse(substitute(y)),
                      zlab=deparse(substitute(z)), revolutions=0, bg.col=c("black", "white"), axis.col=NULL,
                      surface.col=c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"), 
                      neg.res.col="red", pos.res.col="green", point.col="yellow",
                      text.col=axis.col, grid.col=if (bg.col == "white") "black" else "gray", 
                      fogtype=c("exp2", "linear", "exp", "none"), 
                      residuals=(length(fit) == 1), surface=TRUE, grid=TRUE, df.smooth=NULL, df.additive=NULL,
                      sphere.size=1, threshold=0.01, speed=1, fov=60, 
                      fit="linear", groups=NULL, parallel=TRUE, model.summary=FALSE){
    require(rgl)
    require(mgcv)
    if ((!is.null(groups)) && (nlevels(groups) > length(surface.col))) stop(paste("Number of groups (", 
        nlevels(groups), ") exceeds number of colors (", length(surface.col), ").", sep=""))
    if ((!is.null(groups)) && (!is.factor(groups))) stop("groups variable must be a factor.")
    bg.col <- match.arg(bg.col)
    fogtype <- match.arg(fogtype)
    if ((length(fit) > 1) && residuals && surface)
        stop("cannot plot both multiple surfaces and residuals")
    if (is.null(axis.col)) axis.col <- if (bg.col == "white") "black" else "white"
    xlab
    ylab
    zlab
    rgl.clear()
    rgl.viewpoint(fov=fov)
    rgl.bg(col=bg.col, fogtype=fogtype)
    valid <- if (is.null(groups)) !(is.na(x) | is.na(y) | is.na(z))
        else !(is.na(x) | is.na(y) | is.na(z) | is.na(groups))
    x <- x[valid]
    y <- y[valid]
    z <- z[valid]
    if (!is.null(groups)) groups <- groups[valid]
    x <- (x - min(x))/(max(x) - min(x))
    y <- (y - min(y))/(max(y) - min(y))
    z <- (z - min(z))/(max(z) - min(z))
    size <- sphere.size*((100/length(x))^(1/3))*0.015
    if (is.null(groups)){
        if (size > threshold) rgl.spheres(x, y, z, color=point.col, radius=size)
            else rgl.points(x, y, z, color=point.col)
            }
    else {
        if (size > threshold) rgl.spheres(x, y, z, color=surface.col[as.numeric(groups)], radius=size)
            else rgl.points(x, y, z, color=surface.col[as.numeric(groups)])
            }    
    rgl.lines(c(0,1), c(0,0), c(0,0), color=axis.col)
    rgl.lines(c(0,0), c(0,1), c(0,0), color=axis.col)
    rgl.lines(c(0,0), c(0,0), c(0,1), color=axis.col)
    rgl.texts(1, 0, 0, xlab, justify="right", color=text.col)
    rgl.texts(0, 1, 0, ylab, justify="right", color=text.col)
    rgl.texts(0, 0, 1, zlab, justify="right", color=text.col)
    if (surface){
        for (i in 1:length(fit)){
            f <- match.arg(fit[i], c("linear", "quadratic", "smooth", "additive"))
            vals <- seq(0, 1, length=26)
            dat <- expand.grid(x=vals, z=vals)
            if (is.null(groups)){
                mod <- switch(f,
                    linear = lm(y ~ x + z),
                    quadratic = lm(y ~ (x + z)^2 + I(x^2) + I(z^2)),
                    smooth = if (is.null(df.smooth)) gam(y ~ s(x, z))
                        else gam(y ~ s(x, z, fx=TRUE, k=df.smooth)),
                    additive = if (is.null(df.additive)) gam(y ~ s(x) + s(z))
                        else gam(y ~ s(x, fx=TRUE, k=df.additive[1]+1) + 
                            s(z, fx=TRUE, k=(rev(df.additive+1)[1]+1)))
                    )
                if (model.summary) print(summary(mod))
                yhat <- matrix(predict(mod, newdata=dat), 26, 26)
                rgl.surface(vals, vals, yhat, color=surface.col[i], alpha=0.5, lit=FALSE)
                if(grid) rgl.surface(vals, vals, yhat, color=grid.col, alpha=0.5, lit=FALSE, front="lines", back="lines")
                if (residuals){
                    n <- length(y)
                    fitted <- fitted(mod)
                    colors <- ifelse(residuals(mod) > 0, pos.res.col, neg.res.col)
                    rgl.lines(as.vector(rbind(x,x)), as.vector(rbind(y,fitted)), as.vector(rbind(z,z)),
                        color=as.vector(rbind(colors,colors)))
                    }
                }
            else{
                if (parallel){
                    mod <- switch(f,
                        linear = lm(y ~ x + z + groups),
                        quadratic = lm(y ~ (x + z)^2 + I(x^2) + I(z^2) + groups),
                        smooth = if (is.null(df.smooth)) gam(y ~ s(x, z) + groups)
                            else gam(y ~ s(x, z, fx=TRUE, k=df.smooth) + groups),
                        additive = if (is.null(df.additive)) gam(y ~ s(x) + s(z) + groups)
                            else gam(y ~ s(x, fx=TRUE, k=df.additive[1]+1) + 
                                s(z, fx=TRUE, k=(rev(df.additive+1)[1]+1)) + groups)
                        )
                    if (model.summary) print(summary(mod))
                    levs <- levels(groups)
                    for (j in 1:length(levs)){
                        group <- levs[j]
                        select.obs <- groups == group
                        yhat <- matrix(predict(mod, newdata=cbind(dat, groups=group)), 26, 26)
                        rgl.surface(vals, vals, yhat, color=surface.col[j], alpha=0.5, lit=FALSE)
                        if (grid) rgl.surface(vals, vals, yhat, color=grid.col, alpha=0.5, lit=FALSE, front="lines", back="lines")
                        rgl.texts(0, predict(mod, newdata=data.frame(x=0, z=0, groups=group)), 0, 
                            paste(group, " "), justify="right", color=surface.col[j])
                        if (residuals){
                            yy <- y[select.obs]
                            xx <- x[select.obs]
                            zz <- z[select.obs]
                            fitted <- fitted(mod)[select.obs]
                            rgl.lines(as.vector(rbind(xx,xx)), as.vector(rbind(yy,fitted)), as.vector(rbind(zz,zz)),
                                col=surface.col[j])
                            }
                        }
                    }
                else {
                    levs <- levels(groups)
                    for (j in 1:length(levs)){
                        group <- levs[j]
                        select.obs <- groups == group
                        mod <- switch(f,
                            linear = lm(y ~ x + z, subset=select.obs),
                            quadratic = lm(y ~ (x + z)^2 + I(x^2) + I(z^2), subset=select.obs),
                            smooth = if (is.null(df.smooth)) gam(y ~ s(x, z), subset=select.obs)
                                else gam(y ~ s(x, z, fx=TRUE, k=df.smooth), subset=select.obs),
                            additive = if (is.null(df.additive)) gam(y ~ s(x) + s(z), subset=select.obs)
                                else gam(y ~ s(x, fx=TRUE, k=df.additive[1]+1) + 
                                    s(z, fx=TRUE, k=(rev(df.additive+1)[1]+1)), subset=select.obs)
                            )
                        if (model.summary) print(summary(mod))
                        yhat <- matrix(predict(mod, newdata=dat), 26, 26)
                        rgl.surface(vals, vals, yhat, color=surface.col[j], alpha=0.5, lit=FALSE)
                        rgl.surface(vals, vals, yhat, color=grid.col, alpha=0.5, lit=FALSE, front="lines", back="lines")
                        rgl.texts(0, predict(mod, newdata=data.frame(x=0, z=0, groups=group)), 0, 
                            paste(group, " "), justify="right", color=surface.col[j])
                        if (residuals){
                            yy <- y[select.obs]
                            xx <- x[select.obs]
                            zz <- z[select.obs]
                            fitted <- fitted(mod)
                            rgl.lines(as.vector(rbind(xx,xx)), as.vector(rbind(yy,fitted)), as.vector(rbind(zz,zz)),
                                col=surface.col[j])
                            }
                        }
                    }
                }    
            }
        }
    if (revolutions > 0) {
        for (i in 1:revolutions){
            for (angle in seq(1, 360, length=360/speed)) rgl.viewpoint(-angle, fov=fov)
            }
        }
    }


    # Pager

# this is slightly modified from tkpager to use the Rcmdr monospaced font
#   and a white background
    
RcmdrPager <- function (file, header, title, delete.file) 
{
    for (i in seq(along = file)) {
        zfile <- file[[i]]
        tt <- tktoplevel()
        tkwm.title(tt, if (length(title)) 
            title[(i - 1)%%length(title) + 1]
        else "")
        txt <- tktext(tt, bg = "white", font = .logFont)
        scr <- tkscrollbar(tt, repeatinterval = 5, command = function(...) tkyview(txt, 
            ...))
        tkconfigure(txt, yscrollcommand = function(...) tkset(scr, 
            ...))
        tkpack(txt, side = "left", fill = "both", expand = TRUE)
        tkpack(scr, side = "right", fill = "y")
        chn <- tkcmd("open", zfile)
        tkinsert(txt, "end", header[[i]])
        tkinsert(txt, "end", gsub("_\b", "", tclvalue(tkcmd("read", 
            chn))))
        tkcmd("close", chn)
        tkconfigure(txt, state = "disabled")
        tkmark.set(txt, "insert", "0.0")
        tkfocus(txt)
        if (delete.file) 
            tkcmd("file", "delete", zfile)
    }
}
