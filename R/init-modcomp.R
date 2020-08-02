modcomp_init <- function(m1, m2, matrixOK=FALSE){
    UseMethod("modcomp_init")
}

modcomp_init.lmerMod <- function(m1, m2, matrixOK = FALSE) {
    ## Comparison of the mean structures of the models
    ## It is tested for that (1) m1 is lmerMod and (2) m2 is either lmerMod or a matrix

    if (is.numeric(m2) && !is.matrix(m2)) m2 <- matrix(m2, nrow=1)
    
    if (!.is.lmm(m1))
        stop("Model m1 ", substitute(m1), " is not lmerMod\n")

    if (!(.is.lmm(m2) | is.matrix(m2)))
        stop("Model m2 ", substitute(m2), " is not lmerMod or restriction matrix\n")
    
    ##checking matrixcOK is FALSE but m2 is a matrix
    if (!matrixOK & is.matrix(m2)) {
        cat ('Error in modcomp_init \n')
        cat (paste('matrixOK is FALSE but the second model: ', substitute(m2),
                   '\n is  specified via a restriction matrix \n \n',sep=''))
        stop()
    }
    
    Xlarge <- getME(m1, "X")
    rlarge <- rankMatrix(Xlarge)
    code <- if (.is.lmm(m2)){
                Xsmall <- getME(m2, "X")
                rsmall <- rankMatrix(Xsmall)
                rboth  <- rankMatrix(cbind(Xlarge, Xsmall))
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
                if (rankMatrix(rbind(Xlarge, m2)) > rlarge) {
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
