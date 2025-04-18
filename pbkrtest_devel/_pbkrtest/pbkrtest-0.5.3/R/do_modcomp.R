do_modcomp <- function(largeModel, smallModel, method=c("KR", "SAT", "PB")){
    method <- match.arg(method)
    fun <- switch(method,
                  "KR"=KRmodcomp,
                  "SAT"=SATmodcomp,
                  "PB"=PBmodcomp)
    out <- fun(largeModel, smallModel)
    out
}

do_anova <- function(object, ..., method=c("KR", "SAT", "PB")){

    fun <- switch(method,
                  "KR"=KRmodcomp,
                  "SAT"=SATmodcomp,
                  "PB"=PBmodcomp)

    dots <- list(...)
    ## print(dots)
    if (length(dots) == 0){
        an <- anova(object)
        nms <- rownames(an)

        ttt <- mclapply(nms, function(nn){
            kk <- fun(object, nn)
            as.data.frame(kk)[1,]
        })
        
        ttt <- do.call(rbind, ttt)
        rownames(ttt) <- nms
        return(ttt)
    } else {
        if (length(dots)==1){
            mod <- dots[[1]]
            if (!inherits(mod, "lmerMod"))
                stop("Second argument is not lmerMod\n")
            ttt <- fun(object, mod)
            return(ttt)
        }
            
    }
}

##' fm0 <- lmer(sugpct ~ block + sow + harvest + (1|block:harvest), data=beets)

##' do_modcomp(fm0, ~.-harvest, "KR")
##' do_modcomp(fm0, ~.-harvest, "SAT")
##' do_modcomp(fm0, ~.-harvest, "PB")

##' do_anova(fm0, method="KR")
##' do_anova(fm0, method="SAT")
##' do_anova(fm0, method="PB")
