

parm <- c(.5, 1, 0, 0, .5, .5, .5)
trans <- list(co=1:2, var=4:5)

trans <- list(cor=1, var=2)
parm <- 0

make_transformations <- function(parm, trans=list()) {
    
    ## trans can be (abbreviations of) corr, var and prob
    ## eg trans <- list(co=1:2, var=4:5)
    ## Now trans has correct names

    if (length(trans)==0)
        return(parm)

    code <- c("corr", "var", "prob")
    names(trans) <- code[pmatch(names(trans), code)]
    trans

    idx <- seq_along(parm)
    trans <- lapply(trans,function(v) intersect(v, idx))
    
    
    ## Transforms parameters to free scale
    free_fn <-
        list(corr = tanh,
             var  = exp,
             prob = function(x){log(x/(1-x))})
    
    ## Create list of identity functions (to be modified later)
    fun_lst <- lapply(seq_along(parm), 
                      function(k){function(x)x})
    names(fun_lst) <- rep("id", length(fun_lst))
    fun_lst
    ## Modify fun_lst
    for (i in 1:length(trans)){
        ti  <- trans[i]
        nms <- names(ti)
        vls <- ti[[1]]
        fun_lst[vls] <- free_fn[nms]
        names(fun_lst)[vls] <- rep(nms, length(vls))
    }    
    return(fun_lst)
}

vector_function <- function(parm, fn) {
    out <-
        mapply(function(parm.i, fn.i){
            fn.i(parm.i)
        }, parm, fn)
    names(out) <- names(fn)
    return(out)
}

set_free <- function(parm, trans=list()){
    if (length(trans) == 0)
        return(parm)
    vector_function(parm, make_transformations(parm, trans))    
}
