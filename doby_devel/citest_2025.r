#' @title Test for conditional independence in contingecy table.
#'
#' @description Test for conditional independence in contingecy table.
#' 
#' @name ci_test_table
#' 
#' @param x An array / table.
#' 
#' @param set A specification of the test to be made. The tests are of the form
#'     u and v are independent condionally on S where u and v are variables and
#'     S is a set of variables. See 'details' for details about specification of
#'     \code{set}.
#' 
#' @param \dots Additional arguments to be passed on to other methods.
#'
#' @return An object of class `citest` (which is a list).
#'
#' @details
#'  \code{set} can be
#'  1. a vector,
#'  1. a right-hand sided
#'     formula in which variables are separated by '+'.
#'
#' In either case, it is tested if the first two variables in the
#' \code{set} are conditionally independent given the remaining
#' variables in \code{set}.  (Notice an abuse of the '+' operator in
#' the right-hand sided formula: The order of the variables does
#' matter.)
#'
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{chisq.test}}
#' @keywords htest
#' @examples
#' 
#' 
#' ## Testing for conditional independence:
#' Titanic |> ci_test()
#' Titanic |> ci_test(set=1:2)
#' Titanic |> ci_test(set=c(1,2,4), method="mc", B=400)


#' @export 
ci_test <- function(x, set=NULL, ...){
  UseMethod("ci_test")
}

#' @export
ci_test.table <- function(x, set=NULL, ...){
  ci_test_table(x, set, ...)
}

#' @export
ci_test.array <- function(x, set=NULL, ...){
  ci_test_table(x, set, ...)
}

#' @export
print.ci_test_class <- function(x, ...){
    if (length(x$varNames) > 2){
        cat("Testing", x$varNames[1], "_|_", x$varNames[2], "|",x$varNames[-(1:2)],"\n")
    } else {
        cat("Testing", x$varNames[1], "_|_", x$varNames[2], "\n")
    }
    cat(sprintf("Statistic (%s): %8.3f df: %s p-value: %6.4f method: %s\n",
                x$statname, x$statistic, x$df, x$p.value, x$method))

    if ( !is.null(x$slice) ){
        cat("Slice information:\n")
        print( x$slice, digits=4 )
    }

    invisible( x )
}

#' @export
summary.ci_test_class <- function(object,...){
    print( object )
    if ( !is.null(object$slice) ){
        cat("Slice information:\n")
        print( object$slice, digits=4 )
    }
    invisible( object )
}



#' @rdname ci_test_table
ci_test_table <- function(x, set=NULL, statistic="dev", method="chisq",
                         adjust.df=TRUE, slice.info=TRUE, B=200, ...){

    statistic <- match.arg(toupper(statistic), c("DEV",   "X2"))
    method    <- match.arg(toupper(method),    c("CHISQ", "MC", "SMC"))
    
    if (is.null(set)){
        set <- names( dimnames(x) )
    } else {
        if ( inherits(set, "integer") || inherits(set, "numeric") ){
            x   <- tabMarg(x, set)
        } else
            if (inherits(set,c("formula", "character"))){
                set <- unlist(rhsFormula2list(set))
                vn  <- names(dimnames(x))
                set <- vn[pmatch(set, vn)]
                x   <- tabMarg(x, set)
            }
    }

    switch(method,
           "CHISQ"={
               ci_test_x2_worker(x, statistic=statistic, adjust.df=adjust.df,
                           slice.info=slice.info)
           },
           "MC"={
               ci_test_mc_worker(x, statistic=statistic, method=method,
                            slice.info=slice.info, B=B)
           })
}

###
### CIP test; asymptotic, based on either deviance or Pearsons X2
###

# 'x' is a named array; let u be the first name, w be the second and R
# denote the 'rest'. The function tests u _|_ w | R.

ci_test_x2_worker <- function(x, statistic="DEV", adjust.df=TRUE, slice.info=TRUE){

    ## str(x)
    statistic <- match.arg(toupper(statistic), c("DEV",   "X2"))

    vn    <- names(dimnames(x))
    di    <- dim(x)
    u     <- vn[1]
    w     <- vn[2]
    R     <- vn[-(1:2)]
    dim.u <- di[1]
    dim.w <- di[2]
    dim.R <- prod(di[-(1:2)])
    
    t.uR <- tabMarg(x, c(u, R))
    t.wR <- tabMarg(x, c(w, R))
    t.R  <- tabMarg(t.uR, R) ## Marginal counts
    
    ll <<- list(t.uR=t.uR, t.wR=t.wR, R=R, vn=vn)
    fit.table <- fit2way(t.uR, t.wR, R, vn)
    print(fit.table)
    
    ## Evaluate test statistic FIXME There are functions for that in other functions
    if (statistic == "DEV"){           ## Deviance
        tobs  <- 2 * x * log(x / fit.table)
    } else {                           ## Pearson X2
        tobs <- (x - fit.table)^2 / fit.table   
    }
    tobs[!is.finite(tobs)] <- 0

    tobs_global <- sum(tobs)

    ## Calculate df with or without adjustment for sparsity
    if (!adjust.df) {
        df_slice <- rep.int((dim.u - 1) * (dim.w - 1), dim.R)
    } else {
        t.uRmat    <- matrix(t.uR, nrow=dim.R, byrow=TRUE)
        t.wRmat    <- matrix(t.wR, nrow=dim.R, byrow=TRUE)
        
        z <- (t.uRmat > 0) * 1
        dim.u.adj <- if (!is.null(dim(z))) rowSums(z) else sum(z)
        
        z <- (t.wRmat > 0) * 1
        dim.w.adj <- if (!is.null(dim(z))) rowSums(z) else sum(z)
        
        d1         <- dim.u.adj - 1
        d1[d1 < 0] <- 0
        d2         <- dim.w.adj - 1
        d2[d2 < 0] <- 0
        df_slice   <- d1 * d2
    } 
    
    df_global <- sum(df_slice)
    p_global  <- 1 - pchisq(tobs_global, df_global)

    if (length(R) && slice.info){
        tobs_slice <- rowSums(matrix(tobs, nrow=dim.R, byrow=TRUE))
        p_slice    <- 1 - pchisq(tobs_slice, df=df_slice)
        slice_info <- list(statistic=tobs_slice, p.value=p_slice, df=df_slice, n=t.R)
        des        <- expand.grid(dimnames(x)[-(1:2)])
        slice      <- cbind(as.data.frame(slice_info[1:4]), des)
    } else {
        slice <- NULL
    }
    
    ans <- list(statistic=tobs_global, p.value=p_global, df=df_global, statname=statistic,
                method="CHISQ", adjust.df=adjust.df, varNames=vn, slice=slice)

    class(ans) <- "ci_test_class"
    ans
}


fit2way <- function(tab1, tab2, R=NULL, vn){
    if (length(R)>0){
        tab.R <- table_marg(tab1, R)
        tmp <- table_op(tab1, tab2, op=`*`)
        out <- table_op(tmp, tab.R, op=`/`)
        return(table_perm(out, vn))        
    } else {
        tmp <- table_op(tab1, tab2, op=`*`)
        out <- tmp / sum(tab1)
        return(table_perm(tmp, vn))        
    }
}



###
### CIP test; exact, based on sequential monte carlo
###

ci_test_mc_worker <- function(x, statistic="DEV", method="SMC", B=200, slice.info=FALSE){
    
    statistic <- match.arg(toupper(statistic), c("DEV",   "X2"))


    .statFun <- if (statistic=="DEV") .devFun2 else .X2Fun2
    
    dn     <-  dim(x)
    v.idx  <-  seq_len(length(dn))
    v1R    <-  v.idx[-2]
    v2R    <-  v.idx[-1]
    dim12  <-  dim(x)[1:2]
    dim.R  <-  prod(dn[-(1:2)]) ## Careful when R is empty
    vn    <- names(dimnames(x))
    R     <- vn[-(1:2)]
    
    ## Marginal tables for (v1,R) and (v2,R) as matrices. Each row
    ## is a configuration of R
    t1R  <-  tabMarg(x, v1R)
    t2R  <-  tabMarg(x, v2R)
    t.R  <- tabMarg(x, R) ## Marginal counts    
    t1R  <-  matrix(t1R, nrow=dim.R, byrow=TRUE)
    t2R  <-  matrix(t2R, nrow=dim.R, byrow=TRUE)
    xmat <-  matrix(x,   nrow=dim.R, byrow=TRUE)
    
    ## Find observed statistics for each slice
    tobs.slice <- vector("numeric", dim.R)
    for (ii in seq_len(dim.R)){
        r.sum    <- t1R[ii, ]
        c.sum    <- t2R[ii, ]
        expected <- outer(r.sum, c.sum) / sum(r.sum)
        mm       <- xmat[ii, ]
        dim(mm)  <- dim12
        tobs.slice[ii] <- .statFun(mm, expected)
    }
    
    ## Find reference distribution for each slice
    tref.slice      <- matrix(NA, nrow=dim.R, ncol=B)
    n.extreme.slice <- vector("numeric", dim.R)
    for (ii in seq_len(nrow(t1R))){
        r.sum    <- t1R[ii,]
        c.sum    <- t2R[ii,]
        expected <- outer(r.sum, c.sum) / sum(r.sum)
        zzz      <- r2dtable(B, r.sum, c.sum)
        for (kk in seq_len(B))
            tref.slice[ii, kk] <- .statFun(zzz[[kk]],expected)
        n.extreme.slice[ii] <- sum(tobs.slice[ii] < tref.slice[ii, ])
    }
    
    tref.total  <- colSums(tref.slice)
    tobs.total  <- sum(tobs.slice)
    n.extreme   <- sum(tobs.total < tref.total)
    p.value.slice <- n.extreme.slice / B
    p.value.total <- n.extreme / B
    
    if (slice.info){
        des   <- expand.grid(dimnames(x)[-(1:2)])
        slice <- cbind(data.frame(statistic=tobs.slice, n.extreme=n.extreme.slice,
                                  p.value=p.value.slice, df=NA, n=t.R), des)
    } else {
        slice=NULL
    }
    
    ans <- list(statistic=tobs.total, p.value=p.value.total, df=NA, statname=statistic,
                method="MC", varNames=names(dimnames(x)), n.extreme=n.extreme, B=B, slice=slice)
    class(ans) <- "ci_test_class"
    ans
}




## Calculates deviance for independence model in r x c table
.devFun2 <- function(obs, fit){ 
    ii  <- obs * fit > 0
    2 * sum(obs[ii] * log(obs[ii] / fit[ii]))
}

## Calculates deviance for independence model in r x c table
.X2Fun2 <- function(obs, fit){ 
    ii  <- obs * fit > 0
    a   <- (obs - fit)^2 / fit
    sum(a[ii])
}
