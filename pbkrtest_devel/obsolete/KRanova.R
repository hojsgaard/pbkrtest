#' @title Anova for mixed models based on Kenward-Roger approximation
#'
#' @description Anova for mixed models based on Kenward-Roger approximation
#'
#' @name kr-anova
#'
#' @param object A linear mixed model object (medMod object)
#' @param ... Additional arguments, currently not used
#'
#' @note This is a very recent addition to the package; implementation
#'     is very crude and likely to change.

#' @rdname kr-anova
#' @export
KRanova <- function(object, ..., test="F"){

    test <- match.arg(toupper(test), c("F", "CHISQ"))
    ff <- .formula2list(formula(object))
    one.f <- as.formula(paste0(". ~ . -", paste(ff$rhs.fix, collapse=" - "), " + 1"))
    sml <- update(object, one.f, control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))


    out.list <- vector("list", length(ff$rhs.fix))

    for(i in seq_along(ff$rhs.fix)){
        lrg <- update(sml, as.formula(paste0(". ~ . + ", ff$rhs.fix[i])))

        if (identical(test, "F")){
            res <- suppressMessages(KRmodcomp(lrg, sml))
            stats <- res$stats
            out <- unlist(stats[1:4])
        } else {
            out <- getLRT(lrg, sml)[c(2,1,3)]
            names(out)[2] <- "X2"
        }
        
        out.list[[i]] <- out
        sml <- lrg
    }
    
    val <- as.data.frame(do.call(rbind, out.list))
    rownames(val) <- ff$rhs.fix
    class(val) <- c("anova", "data.frame")
    val
}

#' @rdname kr-anova
#' @export
X2anova <- function(object, ...){
    ee <- anova(object)
    ee$p.value <- pchisq(ee$"F value" * ee$npar, df=ee$npar, lower.tail=FALSE)
    ee
}
