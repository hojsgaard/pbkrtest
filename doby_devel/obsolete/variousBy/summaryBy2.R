summaryBy <- function (formula, data = parent.frame(), FUN = mean) 
{
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action", 
        "offset"), names(mf), 0)
    ff <- as.formula(mf[[2]])
    d <- data
    m <- match.call(expand.dots = TRUE)
    if (class(m$ff[[2]]) == "call") {
        y.name <- paste(m$ff[[2]][-1])
    }
    else {
        y.name <- paste(m$ff[[2]])
    }
    yy <- eval(m$formula[[2]], d)
    if (is.null(dim(yy))) 
        dim(yy) <- c(length(yy), 1)
    if (!is.list(FUN)) 
        fun.names <- paste(deparse(substitute(FUN)))
    else fun.names <- unlist(lapply(substitute(FUN)[-1], function(a) paste(a)))
    if (!is.list(FUN)) 
        FUN <- list(FUN)
    group <- attr(terms(ff), "term.labels")
    resp <- NULL
    if (length(ff[[2]]) > 1) {
        for (j in 2:length(ff[[2]])) resp <- c(resp, ff[[2]][[j]])
    }
    else {
        resp <- ff[[2]]
    }
    resp <- as.character(resp)
    colnames(yy) <- resp
    groupList <- NULL
    s <- unlist(lapply(fun.names, paste, resp, sep = "."))
    extra <- which(is.na(match(colnames(yy), names(d))))
    d <- cbind(d, yy[, extra, drop = FALSE])

    vv <- by(d, d[, group], function(x) {
      xf <- x[1, group, drop=FALSE]
      xr <- x[, resp, drop = FALSE]
      v <- NULL
      for (j in 1:length(FUN)) {
        FF <- FUN[[j]]
        vf <- apply(xr, 2, FF)
        v <- c(v, vf)
      }
      v <- c(xf, v)
    })

    idx <- unlist(lapply(vv,length))>0
    vv <- vv[idx]

    val <- unlist(vv)
    val <- as.data.frame(matrix(val, ncol = length(vv[[1]]), 
        byrow = TRUE))
    names(val) <- c(group, s)
    grpi <- match(group, names(d))
    for (j in 1:length(group)) {
        if (is.factor(d[, group[j]])) {
            l <- levels(d[, grpi[j]])[val[, group[j]]]
            val[, group[j]] <- as.factor(l)
        }
    }
    return(val)
}
