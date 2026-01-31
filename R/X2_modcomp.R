#' @title Chisq test 
#' 
#' @description Chisq test 
#' @concept model_comparison
#' @name x2__modcomp
#' 
#' @param largeModel An \code{lmer} model
#' @param smallModel An \code{lmer} model or a restriction matrix
#' @param betaH A number or a vector of the beta of the hypothesis,
#'     e.g. L beta=L betaH. If `smallModel` is a model object then betaH=0.
#' @param details If larger than 0 some timing details are printed.
#' @param ... Additional arguments, currently not used.
#' 
#' @author Ulrich Halekoh \email{uhalekoh@@health.sdu.dk}, Søren Højsgaard
#'     \email{sorenh@@math.aau.dk}
#'
#' (fm0 <- lmer(Reaction ~ (Days|Subject), sleepstudy))
#' (fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
#' (fm2 <- lmer(Reaction ~ Days + I(Days^2) + (Days|Subject), sleepstudy))
#'
#' ## Test for no effect of Days in fm1, i.e. test fm0 under fm1
#' 
#' X2modcomp(fm1, "Days")
#' X2modcomp(fm1, ~.-Days)
#' X2modcomp(fm1, fm0)
#' anova(fm1, fm0)
#'
#' L1 <- cbind(0, 1)
#' X2modcomp(fm1, L1) ## FIXME
#'
#' lm_fit1 <- lm(Reaction ~ Days, data=sleepstudy)
#' X2modcomp(lm_fit1, .~.-Days)
#' Fmodcomp(lm_fit1, .~.-Days)
#'
#' glm_fit1 <- glm(Reaction ~ Days, data=sleepstudy)
#' X2modcomp(glm_fit1, .~.-Days)
#' Fmodcomp(glm_fit1, .~.-Days)
#' 
#' @export
#' @rdname x2__modcomp
Fmodcomp <- function(largeModel, smallModel, betaH=0, details=0, ...){
    UseMethod("Fmodcomp")
}


#' @export
#' @rdname x2__modcomp
Fmodcomp.default <- function(largeModel, smallModel, betaH=0, details=0, ...) {
    Fmodcomp_internal(largeModel=largeModel, smallModel=smallModel, betaH=betaH, details=details)
}


Fmodcomp_internal <- function(largeModel, smallModel, betaH=0, details=0) {

    M <- get_nested_model_info(largeModel, smallModel)
    Fmodcomp_worker(M$largeModel, M$smallModel, betaH=betaH, details=details)
}

Fmodcomp_worker <- function(largeModel, smallModel, betaH=0, details=0) {

    if (!inherits(largeModel, "lm")){
        stop("F test available for for lm's\n")
    }

    if (inherits(largeModel, "glm")){
        return(X2modcomp(largeModel, smallModel,
                  betaH=betaH, details=details))        
    }
    
    ## cat("Fmodcomp_worker\n")
    if (is.null(betaH)) betaH <- 0
    if (is.null(details)) details <- 0
    
    t0    <- proc.time()
    L     <- NULL ##model2restriction_matrix(largeModel, smallModel)
    LRTstat     <- getLRT(largeModel, smallModel)  

    stats <- NULL ## Keep this    
    ans   <- X2compute_p_values(LRTstat, stats)

    ## DUMMY object
    ans2 <- anova(largeModel, smallModel) |> as.data.frame()

    df2 <- min(ans2$Res.Df)
    df1 <- abs(diff(ans2$Res.Df))
    F   <- ans2[2, "F"]
    p.value <- pf(F, df1=df1, df2=df2, lower.tail=FALSE)
    
    test <- data.frame(statistic=F, df=df1, ddf=df2, p.value=p.value)
    rownames(test) <- "F"
    ans$test <- test
    ans$type <- "F"
    class(ans) <- "Fmodcomp"

    ## print(ans)

    
    formula.large <- formula(largeModel)
    formula.small <- formula(smallModel)    
    attributes(formula.large) <- NULL
    
    ans$formula.large <- formula.large
    ans$formula.small <- formula.small
    ans$ctime   <- (proc.time() - t0)[3]
    ans$L       <- L

    out <- ans$test[1,, drop=FALSE]
    attr(out, "aux") <- ans

    attr(out, "heading") <- c(
        deparse(formula.large),
        deparse(formula.small))

    class(out) <- c("X2modcomp", "anova", "data.frame")
    return(out)
}








#' @export
#' @rdname x2__modcomp
X2modcomp <- function(largeModel, smallModel, betaH=0, details=0, ...){
    UseMethod("X2modcomp")
}

#' @export
#' @rdname x2__modcomp
X2modcomp.default <- function(largeModel, smallModel, betaH=0, details=0, ...) {
    X2modcomp_internal(largeModel=largeModel, smallModel=smallModel, betaH=betaH, details=details)
}



X2modcomp_internal <- function(largeModel, smallModel, betaH=0, details=0) {

    M <- get_nested_model_info(largeModel, smallModel)
    X2modcomp_worker(M$largeModel, M$smallModel, betaH=betaH, details=details)
}

X2modcomp_worker <- function(largeModel, smallModel, betaH=0, details=0) {
    ##cat("X2modcomp_worker\n")
    if (is.null(betaH)) betaH <- 0
    if (is.null(details)) details <- 0
    
    t0    <- proc.time()
    L     <- NULL ##model2restriction_matrix(largeModel, smallModel)
    LRTstat     <- getLRT(largeModel, smallModel)  

    stats <- NULL ## Keep this    
    ans   <- X2compute_p_values(LRTstat, stats)
    
    formula.large <- formula(largeModel)
    formula.small <- formula(smallModel)    
    attributes(formula.large) <- NULL
    
    ans$formula.large <- formula.large
    ans$formula.small <- formula.small
    ans$ctime   <- (proc.time() - t0)[3]
    ans$L       <- L

    out <- ans$test[1,, drop=FALSE]
    attr(out, "aux") <- ans

    attr(out, "heading") <- c(
        deparse(formula.large),
        deparse(formula.small))

    class(out) <- c("X2modcomp", "anova", "data.frame")
    return(out)
}

X2compute_p_values <- function(LRTstat, stats=NULL){

    tobs  <- unname(LRTstat[1])
    ndf   <- unname(LRTstat[2])
    p.chi <- unname(LRTstat[3])
        
    test = list(
        X2 = c(statistic=tobs, df=ndf, ddf=NA, p.value=p.chi)
    )
    
    test  <- as.data.frame(do.call(rbind, test))
    test$df <- as.integer(test$df)
    out   <- list(test=test, type="X2", aux=stats$aux, stats=stats)
    ## Notice: stats are carried to the output. Used for get getKR function...
    class(out) <- c("X2modcomp")
    out
}



