
library(caracas)
def_sym(xx, yy, zz)
s <- xx + yy + zz
subs(s, c(xx,yy), c(2,3)) ## Slow
subs(s, c(xx,yy), c(2,xx)) ## Slow


x <- c(xx,yy)
v <- c(2,3)
v <- c(2,xx)

## FIXME: MIKKEL a hack because as_character_matrix gives space in names
x.name <- gsub(" *", "", c(as_character_matrix(x))) 

vv <- as.list(v)
if (!(length(x.name) == length(vv)))
    stop("'x' and 'v' do not have same length")


oo <- sapply(seq_along(vv), function(i){
    paste0("(", x.name[i], ", ", vv[[i]], ")")
})


st <- paste0("[", paste(oo, collapse=", "), "]")

r <- caracas:::eval_to_symbol(st)
r

v2 <- s$pyobj$subs(r$pyobj)
v2
y <- caracas:::construct_symbol_from_pyobj(v2)



## FORSØG FRA I GÅR

matrix_to_list <- function(v){
    if (ncol(v) == 1L) {
        vv <- lapply(seq_len(nrow(v)), function(i) v[i, ])
    } else if (nrow(v) == 1L) {
        vv <- lapply(seq_len(ncol(v)), function(i) v[, i])
    } else {
        stop("When v is a caracas matrix, one dimension must be 1")
    }
    vv
}

get_val <- function(v){
    if (inherits(v, "caracas_symbol")) {
        v$pyobj
    } else {
        v
    }
}

## Dette er for langsomt
subs_vec <- function(s, x, v){
    ## caracas::ensure_sympy()
    ## stopifnot_symbol(s)

    if (is.character(x)){
        x <- as_sym(matrix(x))
    }
    
    if (any(dim(x) > 1)){
        x <- as_sym(matrix(c(as_character_matrix(x))))
    }
    
    if (is.character(v) || is.numeric(v)){
        v <- as_sym(v)
    }
    
    if (any(dim(v) > 1)){
        v <- as_sym(c(as_character_matrix(v)))      
    }
    
    ## stopifnot_matrix(x)
    
    vv <- v
    
    if (inherits(v, "caracas_symbol") && symbol_is_matrix(v)) {
        vv <- matrix_to_list(v)
    } 
        

    py <- s$pyobj
    for (i in seq_along(vv)){
        sym <- as.character(x[i])
        val <- get_val(vv[[i]])
        py$subs(sym, val)
    }

    y <- caracas:::construct_symbol_from_pyobj(py)
    return(y)
}






