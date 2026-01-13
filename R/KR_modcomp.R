## ##########################################################################
##
#' @title F-test and degrees of freedom based on Kenward-Roger approximation
#' 
#' @description An approximate F-test based on the Kenward-Roger approach.
#' @concept model_comparison
#' @name kr__modcomp
#' 
## ##########################################################################
#' @details
#'
#' An F test is calculated according to the approach of Kenward and
#' Roger (1997).  The function works for linear mixed models fitted
#' with the lmer() function of the `lme4` package. Only models where
#' the covariance structure is a linear combination (a weighted sum)
#' of known matrices can be compared.
#' 
#' The `smallModel` is the model to be tested against the `largeModel`.
#' 
#' The `largeModel` is a model fitted with `lmer()`. A technical
#' detail: The model must be fitted with `REML=TRUE`. If the model is
#' fitted with `REML=FALSE` then the model is refitted with
#' `REML=TRUE` before the p-values are calculated. Put differently,
#' the user needs not worry about this issue.
#' 
#' The `smallModel` can be one of several things:
#' 
#' 1) a model fitted with `lmer()`. It must have the same covariance
#' structure as `largeModel`. Furthermore, its linear space of
#' expectation must be a subspace of the space for `largeModel`.
#'
#' 2) a restriction matrix `L` specifying the hypothesis
#' \deqn{L \beta = L \beta_H}
#' where `L` is a `k x p` matrix (there are k restrictions and p is
#' the number of fixed effect parameters (the length of
#' `fixef(largeModel)`) and `beta_H` is a p column vector.
#'
#' 3) A formula or a text string specifying what is to be removed from the
#' larger model to form the smaller model.
#'  
#' Notice: if you want to test a hypothesis
#'
#' \deqn{L \beta = c}
#'
#' with a \eqn{k} vector \eqn{c}, a suitable \eqn{\beta_H} is obtained
#' via \eqn{\beta_H=L c} where \eqn{L_n} is a g-inverse of \eqn{L}.
#' 
#' Notice: It cannot be guaranteed that the results agree with other
#' implementations of the Kenward-Roger approach!
#' 
#' @aliases KRmodcomp KRmodcomp.lmerMod KRmodcomp_internal
#'     KRmodcomp.mer
#' @param largeModel An \code{lmer} model
#' @param smallModel An \code{lmer} model or a restriction matrix
#' @param betaH A number or a vector of the beta of the hypothesis,
#'     e.g. L beta=L betaH. If `smallModel` is a model object then betaH=0.
#' @param details If larger than 0 some timing details are printed.
#'
#' @author Ulrich Halekoh \email{uhalekoh@@health.sdu.dk}, Søren Højsgaard
#'     \email{sorenh@@math.aau.dk}
#' 
#' @seealso \code{\link{getKR}}, \code{\link[lme4]{lmer}},
#'     \code{\link{vcovAdj}}, \code{\link{PBmodcomp}},
#'     \code{\link{SATmodcomp}}
#' 
#' @references Ulrich Halekoh, Søren Højsgaard (2014)., A
#'     Kenward-Roger Approximation and Parametric Bootstrap Methods
#'     for Tests in Linear Mixed Models - The R Package pbkrtest.,
#'     Journal of Statistical Software, 58(10), 1-30.,
#'     \url{https://www.jstatsoft.org/v59/i09/}
#' 
#' Kenward, M. G. and Roger, J. H. (1997), \emph{Small Sample Inference for
#' Fixed Effects from Restricted Maximum Likelihood}, Biometrics 53: 983-997.
#' 
#' 
#' @keywords models inference
#' @examples
#' 
#' (fm0 <- lmer(Reaction ~ (Days|Subject), sleepstudy))
#' (fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
#' (fm2 <- lmer(Reaction ~ Days + I(Days^2) + (Days|Subject), sleepstudy))
#'
#' ## Test for no effect of Days in fm1, i.e. test fm0 under fm1
#' KRmodcomp(fm1, "Days")
#' KRmodcomp(fm1, ~.-Days)
#' L1 <- cbind(0, 1) 
#' KRmodcomp(fm1, L1)
#' KRmodcomp(fm1, fm0)
#' anova(fm1, fm0)
#'
#' ## Test for no effect of Days and Days-squared in fm2, i.e. test fm0 under fm2
#' KRmodcomp(fm2, "(Days+I(Days^2))")
#' KRmodcomp(fm2, ~. - Days - I(Days^2))
#' L2 <- rbind(c(0, 1, 0), c(0, 0, 1))
#' KRmodcomp(fm2, L2)
#' KRmodcomp(fm2, fm0)
#' anova(fm2, fm0)
#' 
#' ## Test for no effect of Days-squared in fm2, i.e. test fm1 under fm2
#' KRmodcomp(fm2, "I(Days^2)")
#' KRmodcomp(fm2, ~. - I(Days^2))
#' L3 <- rbind(c(0, 0, 1))
#' KRmodcomp(fm2, L3)
#' KRmodcomp(fm2, fm1)
#' anova(fm2, fm1)


#' @export
#' @rdname kr__modcomp
KRmodcomp <- function(largeModel, smallModel, betaH=0, details=0){
    UseMethod("KRmodcomp")
}

#' @export
#' @rdname kr__modcomp
KRmodcomp.lmerMod <- function(largeModel, smallModel, betaH=0, details=0) {
    KRmodcomp_internal(largeModel=largeModel, smallModel=smallModel, betaH=betaH, details=details)
}

KRmodcomp_internal <- function(largeModel, smallModel, betaH=0, details=0) {

    if (is.character(smallModel))
        smallModel <- doBy::formula_add_str(formula(largeModel), terms=smallModel, op="-")
    
    if (inherits(smallModel, "formula"))
        smallModel  <- update(largeModel, smallModel)
            
    w <- modcomp_init(largeModel, smallModel, matrixOK = TRUE)

    if (w == -1) stop('Models have equal mean stucture or are not nested!')
    if (w == 0){
        ## First given model is submodel of second; exchange the models
        tmp <- largeModel; largeModel <- smallModel; smallModel <- tmp
    }
    
    ## Refit large model with REML if necessary
    if (!(getME(largeModel, "is_REML"))){
        largeModel <- update(largeModel, .~., REML=TRUE)
    }

    KRmodcomp_worker(largeModel, smallModel, betaH=betaH, details=details)    
}


KRmodcomp_worker <- function(largeModel, smallModel, betaH=0, details=0) {

    if (is.null(betaH)) betaH <- 0
    if (is.null(details)) details <- 0
    
    ## All computations are based on 'largeModel' and the restriction matrix 'L'
    ## -------------------------------------------------------------------------
    t0    <- proc.time()
    L     <- model2restriction_matrix(largeModel, smallModel)
    
    PhiA  <- vcovAdj(largeModel, details)
    stats <- .KR_adjust(PhiA, Phi=vcov(largeModel), L, beta=fixef(largeModel), betaH)
    stats <- lapply(stats, c) ## To get rid of all sorts of attributes
    
    formula.small <-
        if (.is.lmm(smallModel)){
            .zzz <- formula(smallModel)
            attributes(.zzz) <- NULL
            .zzz
        } else {
            list(L = L, betaH = betaH)
        }
    formula.large <- formula(largeModel)
    attributes(formula.large) <- NULL


    test = list(
        KR      = c(statistic=stats$Fstat,     df=stats$ndf,  ddf=stats$ddf,  F.scaling=stats$F.scaling,
                       p.value=stats$p.value),
        KRU     = c(statistic=stats$FstatU,    df=stats$ndf,  ddf=stats$ddf,  F.scaling=NA,
                       p.value=stats$p.valueU))
    test  <- as.data.frame(do.call(rbind, test))
    test$df <- as.integer(test$df)

    out <- list(
        test=test,
        sigma=getME(largeModel, "sigma"),
        formula.large=formula(largeModel),
        formula.small=formula(smallModel),
        ctime=(proc.time() - t0)[3],
        L=L
    )
    class(out) <- c("KRmodcomp")
    return(out)
}
    



.KRcommon <- function(x){
  cat("large : ")
  print(x$formula.large)

  if (inherits(x$formula.small, "call")){
    cat("small : ")
    print(x$formula.small)
  } else {
    formSmall <- x$formula.small

    cat("L = \n")
    print(formSmall$L)
    if (!all(formSmall$betaH == 0)){
        cat('betaH=\n')
        print(formSmall$betaH)
    }
  }
}

#' @export
print.KRmodcomp <- function(x, ...){

    ## .KRcommon(x)

    cat("large : ")
    print(x$formula.large)
    if (inherits(x$formula.small, "formula")) cat("small : ")
    else cat("small (restriction matrix) : \n")
    prform(x$formula.small)


    FF.thresh <- 0.2
    tab <- x$test
    F.scale <- tab[['F.scaling']]
    
    if (max(F.scale, na.rm=TRUE) > FF.thresh)
        i <- 1
    else
        i <- 2

    ## printCoefmat(tab[i,, drop=FALSE], tst.ind=1+c(1,2,3), na.print='', has.Pvalue=TRUE)
    tab <- tab[i,, drop=FALSE]
    dd <- as.data.frame(tab[c("statistic", "df", "ddf", "p.value")])
    printCoefmat(dd, has.Pvalue=TRUE)    
    invisible(x)
}


#' @export
summary.KRmodcomp <- function(object, ...){

    cat(sprintf("F-test with Kenward-Roger approximation; time: %.2f sec\n",
                object$ctime))

    FF.thresh <- 0.2
    tab <- object$test
    F.scale <- tab[['F.scaling']]
    
    if (max(F.scale, na.rm=TRUE) > FF.thresh)
        i <- 1
    else
        i <- 2

    ## printCoefmat(tab[i,, drop=FALSE], tst.ind=1+c(1,2,3), na.print='', has.Pvalue=TRUE)
    tab <- tab[i,, drop=FALSE]
    dd <- as.data.frame(tab[c("statistic", "df", "ddf", "p.value")])
    printCoefmat(dd, has.Pvalue=TRUE)    

    class(tab) <- c("summary_KRmodcomp", "data.frame")
    invisible(tab)

    ## invisible(x)

    
    ## .KRcommon(object)
    ## tab <- object$test
    
    ## printCoefmat(tab, tst.ind=c(1,2,3), na.print='', has.Pvalue=TRUE)

    ## FF.thresh <- 0.2
    ## F.scale <- object$aux['F.scaling']

    ## if (F.scale < FF.thresh & F.scale > 0) {
    ##     cat('Note: The scaling factor for the F-statistic is smaller than 0.2 \n')
    ##     cat('The Unscaled statistic might be more reliable \n ')
    ## } else {
    ##     if (F.scale <=0 ){
    ##         cat('Note: The scaling factor for the F-statistic is negative \n')
    ##         cat('Use the Unscaled statistic instead. \n ')
    ##     }
    ## }

    ## class(tab) <- c("summary_KRmodcomp", "data.frame")
    ## invisible(tab)
    
}




## out   <- .finalizeKR(stats)        
## out$formula.large <- formula.large
## out$formula.small <- formula.small
## out$ctime   <- (proc.time() - t0)[3]
## out$L       <- L
## 
## out    

## .finalizeKR <- function(stats){
    
##     test = list(
##         Ftest      = c(statistic=stats$Fstat,     df=stats$ndf,  ddf=stats$ddf,  F.scaling=stats$F.scaling,
##                        p.value=stats$p.value),
##         FtestU     = c(statistic=stats$FstatU,    df=stats$ndf,  ddf=stats$ddf,  F.scaling=NA,
##                        p.value=stats$p.valueU))
##     test  <- as.data.frame(do.call(rbind, test))
##     test$df <- as.integer(test$df)
##     out   <- list(test=test, type="F", aux=stats$aux, stats=stats)
##     ## Notice: stats are carried to the output. They are used for get getKR function...

##     print(out)
##     class(out) <- c("KRmodcomp")
##     out
## }



## KRmodcomp_internal2 <- function(largeModel, LL, betaH=0, details=0){
    
##     PhiA  <- vcovAdj(largeModel, details)
##     stats <- .KR_adjust(PhiA, Phi=vcov(largeModel), LL, beta=fixef(largeModel), betaH)
##     stats <- lapply(stats, c) ## To get rid of all sorts of attributes
##     out   <- .finalizeKR(stats)
##     out
## }

## --------------------------------------------------------------------
## This is the function that calculates the Kenward-Roger approximation
## --------------------------------------------------------------------
.KR_adjust <- function(PhiA, Phi, L, beta, betaH){
  Theta  <-  t(L) %*% solve( L %*% Phi %*% t(L), L)
  P <- attr( PhiA, "P" )
  W <- attr( PhiA, "W" )

  ## print(Theta %*% Phi)
  ## print(W)
  ## print(P)
  
  A1 <- A2 <- 0
  ThetaPhi <- Theta %*% Phi
  n.ggamma <- length(P)
  for (ii in 1:n.ggamma) {
    for (jj in c(ii:n.ggamma)) {
      e  <- ifelse(ii==jj, 1, 2)
      ui <- ThetaPhi %*% P[[ii]] %*% Phi
      uj <- ThetaPhi %*% P[[jj]] %*% Phi
      ## print(ui); print(uj)
      A1 <- A1 + e * W[ii,jj] * (.spur(ui) * .spur(uj))
      A2 <- A2 + e * W[ii,jj] * sum(ui * t(uj))
    }
  }

  q <- as.numeric(rankMatrix(L))
  B <- (1/(2*q)) * (A1+6*A2)
  g <- ( (q+1)*A1 - (q+4)*A2 )  / ((q+2)*A2)
  c1<- g/(3*q+ 2*(1-g))
  c2<- (q-g) / (3*q + 2*(1-g))
  c3<- (q+2-g) / ( 3*q+2*(1-g))
  ## cat(sprintf("q=%i B=%f A1=%f A2=%f\n", q, B, A1, A2))
  ## cat(sprintf("g=%f, c1=%f, c2=%f, c3=%f\n", g, c1, c2, c3))

###orgDef: E<-1/(1-A2/q)
###orgDef: V<- 2/q * (1+c1*B) /  ( (1-c2*B)^2 * (1-c3*B) )

  ##EE     <- 1/(1-A2/q)
  ##VV     <- (2/q) * (1+c1*B) /  ( (1-c2*B)^2 * (1-c3*B) )
  EE     <- 1 + (A2 / q)
  VV     <- (2 / q) * (1 + B)
  EEstar <- 1 / (1 - A2 / q)
  VVstar <- (2 / q) * ((1 + c1 * B) / ((1 - c2 * B)^2 * (1 - c3 * B)))
  ##  cat(sprintf("EE=%f VV=%f EEstar=%f VVstar=%f\n", EE, VV, EEstar, VVstar))
  V0<-1 + c1*B
  V1<-1 - c2*B
  V2<-1 - c3*B
  V0<-ifelse(abs(V0) < 1e-10, 0, V0)
  
  ##  cat(sprintf("V0=%f V1=%f V2=%f\n", V0, V1, V2))

###orgDef: V<- 2/q* V0 /(V1^2*V2)
###orgDef: rho <-  V/(2*E^2)

  ## str(list(q=q, A2=A2, V1=V1, V0=V0, V2=V2))
  rho <- 1/q * (.divZero(1 - A2 / q, V1))^2 * V0 / V2
  df2 <- 4 + (q + 2) / (q * rho - 1)          ## Here are the adjusted degrees of freedom.

###orgDef: F.scaling <-  df2 /(E*(df2-2))
###altCalc F.scaling<- df2 * .divZero(1-A2/q,df2-2,tol=1e-12)
  ## this does not work because df2-2 can be about 0.1
  F.scaling <- ifelse( abs(df2 - 2) < 1e-2, 1 , df2 * (1 - A2 / q) / (df2 - 2))
  ##cat(sprintf("KR: rho=%f, df2=%f F.scaling=%f\n", rho, df2, F.scaling))

  ## Vector of auxiliary values; just for checking etc...
  aux <- c(A1=A1, A2=A2, V0=V0, V1=V1, V2=V2, rho=rho, F.scaling=F.scaling)

### The F-statistic; scaled and unscaled
  betaDiff <- cbind( beta - betaH )

  
  ## Wald     <- as.numeric(t(betaDiff) %*% t(L) %*% solve(L %*% PhiA %*% t(L), L %*% betaDiff))
  ## WaldU    <- as.numeric(t(betaDiff) %*% t(L) %*% solve(L %*% Phi %*% t(L), L %*% betaDiff))

  Lb2 <- L %*% betaDiff
  Wald     <- as.numeric(t(Lb2) %*% solve(L %*% PhiA %*% t(L), Lb2))
  WaldU    <- as.numeric(t(Lb2) %*% solve(L %*% Phi  %*% t(L), Lb2))

  FstatU <- Wald / q
  pvalU  <- pf(FstatU, df1=q, df2=df2, lower.tail=FALSE)

  Fstat  <- F.scaling * FstatU
  pval   <- pf(Fstat, df1=q, df2=df2, lower.tail=FALSE)

  stats <- list(ndf=q, ddf=df2,
                Fstat  = Fstat,  p.value=pval, F.scaling=F.scaling,
                FstatU = FstatU, p.valueU = pvalU,
                aux = aux)
  stats
}



