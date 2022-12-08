doby/R/

#' budworm data
#' 
#' Effect of Insecticide on survivial of tobacco budworms
#' number of killed budworms exposed to an insecticidepp
#' mortality of the moth tobacco budworm 'Heliothis virescens' for 6 doses of
#' the pyrethroid trans-cypermethrin differentiated with respect to sex
#'
#' @name budworm
#' @docType data
#' 
#' @format This data frame contains 12 rows and 4 columns:
#' \describe{
#' \item{sex:}{sex of the budworm}
#' \item{dose:}{dose of the insecticide trans-cypermethrin in [\eqn{\mu}{mu}g]}
#' \item{ndead:}{budworms killed in a trial}
#' \item{ntotal:}{total number of budworms exposed per trial }
#' }
#'
#' @references Venables, W.N; Ripley, B.D.(1999) Modern Applied Statistics with
#' S-Plus, Heidelberg, Springer, 3rd edition, chapter 7.2
#'
#' @source Collet, D. (1991) Modelling Binary Data, Chapman & Hall, London,
#' Example 3.7
#'
#' @keywords datasets
#' @examples
#' 
#' data(budworm)
#' 
#' ## function to caclulate the empirical logits
#' empirical.logit<- function(y, n) {
#'    el <- log((y + 0.5) / (n - y + 0.5))
#'    el
#' }
#' 
#' 
#' # plot the empirical logits against log-dose
#' 
#' log.dose <- log(budworm$dose)
#' emp.logit <- empirical.logit(budworm$ndead, budworm$ntotal)
#' plot(log.dose, emp.logit, type='n', xlab='log-dose', ylab='emprirical logit')
#' title('budworm: emprirical logits of probability to die ')
#' male <- budworm$sex=='male'
#' female <- budworm$sex=='female'
#' lines(log.dose[male], emp.logit[male], type='b', lty=1, col=1)
#' lines(log.dose[female], emp.logit[female], type='b', lty=2, col=2)
#' legend(0.5, 2, legend=c('male', 'female'), lty=c(1, 2), col=c(1, 2))
#' 
#' \dontrun{
#' * SAS example;
#' data budworm;
#' infile 'budworm.txt' firstobs=2;
#' input sex dose ndead ntotal;
#' run;
#' }
#' 
#' 
"budworm"












## #' @rdname by-subset
## subset_by <- function(data, formula, subset, select, drop=FALSE, join=TRUE,...){
##     ddd <- splitBy(formula, data=data)
##     subsetMissing <- missing(subset)
##     selectMissing <- missing(select)  
##     e <- substitute(subset)
##     ddd <-
##         lapply(ddd, 
##                function(x){
##                    if (subsetMissing) 
##                        r <- TRUE
##                    else {
##                        r <- eval(e, x, parent.frame())
##                        if (!is.logical(r)) 
##                            stop("'subset' must evaluate to logical")
##                        r <- r & !is.na(r)
##                    }
##                    if (selectMissing) 
##                        vars <- TRUE
##                    else {
##                        nl <- as.list(1:ncol(x))
##                        names(nl) <- names(x)
##                        vars <- eval(substitute(select), nl, parent.frame())
##                    }
##                    x[r, vars, drop = drop]
##                }
##                )
##     if (join)
##         do.call("rbind", ddd)
##     else
##         ddd
## }











  ## atr <- attributes(x)[c("at","grid")]
  ## aa <- !unlist(lapply(atr, is.null))
  ## str(atr[aa])






## #' @rdname linest-matrix
## LP_matrix <- function(object, effect=NULL, at=NULL){
##   UseMethod("LP_matrix")
## }

## ## FIXME: LE_matrix.default: Should be a check of what 'object' is
## #' @rdname linest-matrix
## LP_matrix.default <- function(object, effect=NULL, at=NULL){
##     out <- get_linest_list(object, effect, at)
##     out <- aggregate_linest_list (out)
##     class(out) <- c("linest_matrix_class", "matrix")
##     out
## }





## .get_linest_list <- function(object, effect=NULL, at=NULL){
##     ##cat(".get_linest_list\n")
##     trms     <- delete.response( terms(object) )
##     fact.lev <- get_xlevels( object )            ## factor levels
##     ##cat("fact.lev:\n"); print(fact.lev)
##     cov.ave  <- .get_covariate_ave( object, at )  ## average of covariates (except those mentioned in 'at')
##     ##cat("cov.ave:\n"); print(cov.ave)
##     vartype  <- get_vartypes( object )           ## which are factors and which are numerics
##     ##cat("vartype:\n"); print(vartype)
##     at.factor.name <- intersect( vartype$factor, names(at) )
##     cov.ave.name   <- names( cov.ave )
##     effect         <- setdiff( effect, at.factor.name )


    
##     ## tmp <- list(fact.lev=fact.lev, cov.ave=cov.ave, vartype=vartype, at.factor.name=at.factor.name,
##     ## cov.ave.name=cov.ave.name, effect=effect, at=at)
##     ## print(tmp)

##     if (is.null(effect))
##     {
##         new.fact.lev <- if (length(at.factor.name) > 0) at[ at.factor.name ]
##                         else NULL        
##         ## if (length(at.factor.name) > 0){
##         ##     new.fact.lev <- at[ at.factor.name ]
##         ## } else {
##         ##     new.fact.lev <- NULL
##         ## }
##     } else {
##         new.fact.lev  <- set_xlevels(fact.lev, at=at)
##         new.fact.lev  <- new.fact.lev[c(effect, at.factor.name)]#
##     }
    
##     if (is.null(new.fact.lev)){
##         ##cat("case: No 'effect' and no 'at'-factors; hence just a global average... \n")
##         if (length(fact.lev) > 0)
##         {
##             ##cat("there are factors in the model\n")
##             newdata <- expand.grid( fact.lev )
##             if (length( cov.ave.name ) > 0){
##                 ##cat("there are covariates in the model\n")
##                 newdata[, cov.ave.name] <- cov.ave
##             }
##         } else {
##             ## cat("No factors in the model\n")
##             if (length( cov.ave.name ) > 0){
##                 ##cat("yes there are covariates\n")
##                 newdata <- matrix(unlist(cov.ave), nrow=1L)
##                 colnames(newdata) <- cov.ave.name
##                 newdata <- as.data.frame( newdata )
##             } else {
##                 ##cat("there are no factors or covariates\n")
##                 newdata <- data.frame(1)
##             }
##         }

##         XXlist <- list(get_X(object, newdata))
##         ## cat("XXlist:\n"); print(XXlist)
##         attr(XXlist, "at")   <- at[intersect(vartype$numeric, names(at))]
##         attr(XXlist, "grid") <- NULL
##     }
##     else
##     {
##         ##cat("The general case; there are 'effect' factors or 'at' factors...\n")
##         grid.data <- expand.grid(new.fact.lev)
##         grid.data <- as.data.frame(lapply(grid.data, as.character), stringsAsFactors=FALSE)

##         XXlist    <- list()
##         for (ii in 1:nrow(grid.data)){
##             config    <- grid.data[ ii, ,drop=FALSE ]
##             fact.lev2 <- set_xlevels(fact.lev,  at=config)

##             newdata   <- expand.grid( fact.lev2 )
##             newdata[, cov.ave.name]  <- cov.ave
##             XX             <- get_X(object, newdata, at)
##             XXlist[[ ii ]] <- XX
##         }

##         grid.data[, names(cov.ave) ] <- cov.ave
##         attr(XXlist, "at") <- at
##         attr(XXlist, "grid") <- grid.data
##         attr(XXlist, "offset") <- attr(XX, "offset")
##     }
##     class(XXlist) <- "linest_list_class"
##     XXlist
## }



  ## switch(class(object),
  ##        "lmBy"={
  ##            ii <- match(name, c("dataList","idData"))
  ##            if (is.na(ii))
  ##                stop(sprintf("%s not available", name))
  ##            attr(object,name)	
  ##        })



## .wald <- function (obj, L, beta0)
## {
##     if (!is.matrix(L) && !is.data.frame(L))
##         L <- matrix(L, nrow = 1)

##     if (missing(beta0))
##       beta0 <- rep(0, nrow(L))

##     df <- nrow(L)
    
##     if ("geese" %in% class(obj)) {
##       coef.mat  <- obj$beta
##       vcv <- obj$vbeta
##     } else if ("geeglm" %in% class(obj)) {
##       coef.mat  <- obj$coef
##       vcv <- summary(obj)$cov.scaled
##     } else if ("gls" %in% class(obj)) {
##         ##vcv <- vcov(obj)
##         vcv <- vcov(obj, complete=FALSE)
##       coef.mat  <- matrix(coef(obj))
##     } else if ("gee" %in% class(obj)) {
##       coef.mat  <- obj$coef
##       vcv <- obj$robust.variance
##     }
##     else if ("lm" %in% class(obj)) {
##       coef.mat  <- summary.lm(obj)$coefficients[, 1]
##       vcv <- summary.lm(obj)$cov.unscaled * summary.lm(obj)$sigma^2
##       if ("glm" %in% class(obj)) {
##         vcv <- summary(obj)$cov.scaled
##       }
##     }
##     else if ("coxph" %in% class(obj)) {
##       coef.mat <- obj$coef
##       vcv <- obj$var
##     }
##     else
##         stop("obj must be of class 'lm', 'glm', 'aov', 'gls', 'gee', 'geese', 'coxph'")
    
##     u      <- (L %*% coef.mat)-beta0
##     vcv.u  <- L %*% vcv %*% t(L)
##     W      <- t(u) %*% solve(vcv.u) %*% u
##     prob   <- 1 - pchisq(W, df = df)
##     out <- as.data.frame(cbind(W, df, prob))
##     names(out) <- c("X2.stat", "DF", "Pr(>|X^2|)")
##     as.data.frame(out)
## }
