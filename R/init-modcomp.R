modcomp_init <- function(m1, m2, matrixOK=FALSE){
    UseMethod("modcomp_init")
}

modcomp_init.merMod <- function(m1, m2, matrixOK = FALSE) {
    ## Comparison of the mean structures of the models
    ## It is tested for that (1) m1 is merMod and (2) m2 is either merMod or a matrix

    ## cat("m1:\n"); print(m1)
    ## cat("m2:\n"); print(m2)
    
    if (is.numeric(m2) && !is.matrix(m2)) {
        m2 <- matrix(m2, nrow=1)
    }
    
    if (!.is.mm(m1))
        stop("Model m1 ", substitute(m1), " is not merMod\n")

    if (!(.is.mm(m2) | is.matrix(m2)))
        stop("Model m2 ", substitute(m2), " is not merMod or restriction matrix\n")
    
    ##checking matrixcOK is FALSE but m2 is a matrix
    
    if (!matrixOK & is.matrix(m2)) {
        cat ('Error in modcomp_init \n')
        cat (paste('matrixOK is FALSE but the second model: ', substitute(m2),
                   '\n is  specified via a restriction matrix \n \n',sep=''))
        stop()
    }
    
    Xlarge <- getME(m1, "X")
    rlarge <- rankMatrix_(Xlarge)

    ## print(Xlarge)
    
    ## -1 : Models have identical mean structures or are not nested
    ## 0  : m1 is submodel of m2
    ## 1  : m2 is submodel of m1

    ## Xl <<- Xlarge
    ## rl <<- rankMatrix(Xl)
    
    code <- if (.is.mm(m2)){
                Xsmall <- getME(m2, "X")
                ## Xs <<- Xsmall
                ## rs <<- rankMatrix(Xs)
                ## print(Xsmall)
                rsmall <- rankMatrix_(Xsmall)
                rboth  <- rankMatrix_(cbind(Xlarge, Xsmall))
                if (rboth == pmax(rlarge, rsmall)) {
                    if (rsmall < rlarge) {
                        1
                    } else {
                        if (rsmall > rlarge) {
                            0
                        } else {
                            -1
                        }
                    }
                } else {
                    -1
                }
            } else {
                ##now model m2  is a restriction matrix
                if (rankMatrix_(rbind(Xlarge, m2)) > rlarge) {
                    -1
                } else {
                    1
                }
            }
    code
}

##KRmodcomp_init.mer <- KRmodcomp_init.lmerMod




        ## if (!mers) {
        ##     cat("Error in modcomp_init\n")
        ##     cat(paste("either model ",substitute(m1), 
        ##               "\n is not a linear mixed of class mer(CRAN) or lmerMod (GitHub)\n \n",sep=' '))
        ##     cat(paste("or model ", substitute(m2),"\n is neither of that class nor a matrix",sep=''))
        ##     stop()
        ## }
