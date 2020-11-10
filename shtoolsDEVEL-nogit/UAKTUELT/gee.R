
gee <- 
function (formula = formula(data), id = id, data = parent.frame(), 
    subset, na.action, R = NA, b = NA, tol = 0.001, maxiter = 25, 
    family = gaussian, corstr = "independence", Mv = 1, silent = TRUE, 
    contrasts = NULL, scale.fix = FALSE, scale.value = 1, v4.4compat = FALSE) 
{
    ##print("Beginning Cgee S-function, @(#) geeformula.q 4.13 98/01/27")
    call <- match.call()
    m <- match.call(expand = FALSE)
    m$R <- m$b <- m$tol <- m$maxiter <- m$link <- m$varfun <- m$corstr <- m$Mv <- m$silent <- m$contrasts <- m$family <- m$scale.fix <- m$scale.value <- m$v4.4compat <- NULL
    if (is.null(m$id)) {
        m$id <- as.name("id")
    }
    if (!is.null(m$na.action) && m$na.action != "na.omit") {
        print("Warning: Only na.omit is implemented for gee")
        print("         continuing with na.action=na.omit")
        m$na.action <- as.name("na.omit")
    }
    m[[1]] <- as.name("model.frame")
    m <- eval(m, parent.frame())
    Terms <- attr(m, "terms")
    y <- as.matrix(model.extract(m, response))
    x <- model.matrix(Terms, m, contrasts)
    N <- rep(1, length(y))
    if (dim(y)[2] == 2) {
        N <- as.vector(y %*% c(1, 1))
        y <- y[, 1]
    }
    else {
        if (dim(y)[2] > 2) 
            stop("Only binomial response matrices (2 columns)")
    }
    offset <- model.extract(m, offset)
    id <- model.extract(m, id)
    if (is.null(id)) {
        stop("Id variable not found")
    }
    nobs <- nrow(x)
    p <- ncol(x)
    xnames <- dimnames(x)[[2]]
    if (is.null(xnames)) {
        xnames <- paste("x", 1:p, sep = "")
        dimnames(x) <- list(NULL, xnames)
    }
    if (is.character(family)) 
        family <- get(family)
    if (is.function(family)) 
        family <- family()
    if (!is.na(b)) {
        beta <- matrix(as.double(b), ncol = 1)
        if (nrow(beta) != p) {
            stop("Dim beta != ncol(x)")
        }
        print("user's initial regression estimate")
        print(beta)
    }
    else {
        ##print("running glm to get initial regression estimate")
        mm <- match.call(expand = FALSE)
        mm$R <- mm$b <- mm$tol <- mm$maxiter <- mm$link <- mm$varfun <- mm$corstr <- mm$Mv <- mm$silent <- mm$contrasts <- mm$scale.fix <- mm$scale.value <- mm$id <- NULL
        mm[[1]] <- as.name("glm")
        beta <- as.numeric(eval(mm, parent.frame())$coef)
        ##print(beta)
    }
    if (length(id) != length(y)) {
        stop("Id and y not same length")
    }
    maxclsz <- as.integer(max(unlist(lapply(split(id, id), "length"))))
    maxiter <- as.integer(maxiter)
    silent <- as.integer(silent)
    if (length(offset) <= 1) {
        offset <- rep(0, length(y))
    }
    if (length(offset) != length(y)) 
        stop("offset and y not same length")
    offset <- as.double(offset)
    if (!is.na(R[1])) {
        Rr <- nrow(R)
        if (Rr != ncol(R)) {
            stop("R is not square!")
        }
        if (Rr < maxclsz) {
            stop("R not big enough to accommodate some clusters.")
        }
    }
    else {
        R <- matrix(as.double(rep(0, maxclsz * maxclsz)), nrow = maxclsz)
    }
    links <- c("identity", "log", "logit", "inverse", "probit", 
        "cloglog")
    fams <- c("gaussian", "poisson", "binomial", "Gamma", "quasi")
    varfuns <- c("constant", "mu", "mu(1-mu)", "mu^2")
    corstrs <- c("independence", "fixed", "stat_M_dep", "non_stat_M_dep", 
        "exchangeable", "AR-M", "unstructured")
    linkv <- as.integer(match(c(family$link), links, -1))
    famv <- match(family$family, fams, -1)
    if (famv < 1) 
        stop("unknown family")
    if (famv <= 4) 
        varfunv <- famv
    else varfunv <- match(family$varfun, varfuns, -1)
    varfunv <- as.integer(varfunv)
    corstrv <- as.integer(match(corstr, corstrs, -1))
    if (linkv < 1) 
        stop("unknown link.")
    if (varfunv < 1) 
        stop("unknown varfun.")
    if (corstrv < 1) 
        stop("unknown corstr.")
    naivvar <- matrix(rep(0, p * p), nrow = p)
    robvar <- matrix(rep(0, p * p), nrow = p)
    phi <- as.double(scale.value)
    scale.fix <- as.integer(scale.fix)
    errstate <- as.integer(1)
    tol <- as.double(tol)
    Mv <- as.integer(Mv)
    maxcl <- as.integer(0)
    if (!(is.double(x))) 
        x <- as.double(x)
    if (!(is.double(y))) 
        y <- as.double(y)
    if (!(is.double(id))) 
        id <- as.double(id)
    if (!(is.double(N))) 
        N <- as.double(N)
    modvec <- as.integer(c(linkv, varfunv, corstrv))
    if (v4.4compat) 
        compatflag <- 1
    else compatflag <- 0
    z <- .C("Cgee", x, y, id, N, offset, nobs, p, modvec, Mv, 
        estb = beta, nv = naivvar, rv = robvar, sc = phi, wcor = R, 
        tol, mc = maxcl, iter = maxiter, silent, err = errstate, 
        scale.fix, as.integer(compatflag), PACKAGE = "gee")
    if (z$err != 0) 
        warning(paste("Note: Cgee had an error (code=", z$err, 
            ").  Results suspect."))
    if (min(eigen(z$wcor)$values) < 0) {
        warning("Working correlation estimate not positive definite")
        z$err <- z$err + 1000
    }
    fit <- list()
    attr(fit, "class") <- c("gee", "glm")
    fit$title <- "GEE:  GENERALIZED LINEAR MODELS FOR DEPENDENT DATA"
    fit$version <- "gee S-function, version 4.13 modified 98/01/27 (1998)"
    links <- c("Identity", "Logarithm", "Logit", "Reciprocal", 
        "Probit", "Cloglog")
    varfuns <- c("Gaussian", "Poisson", "Binomial", "Gamma")
    corstrs <- c("Independent", "Fixed", "Stationary M-dependent", 
        "Non-Stationary M-dependent", "Exchangeable", "AR-M", 
        "Unstructured")
    fit$model <- list()
    fit$model$link <- links[linkv]
    fit$model$varfun <- varfuns[varfunv]
    fit$model$corstr <- corstrs[corstrv]
    if (!is.na(match(c(corstrv), c(3, 4, 6)))) 
        fit$model$M <- Mv
    fit$call <- call
    fit$terms <- Terms
    fit$formula <- as.vector(attr(Terms, "formula"))
    fit$contrasts <- attr(x, "contrasts")
    fit$nobs <- nobs
    fit$iterations <- z$iter
    fit$coefficients <- as.vector(z$estb)
    fit$nas <- is.na(fit$coefficients)
    names(fit$coefficients) <- xnames
    eta <- as.vector(x %*% fit$coefficients)
    fit$linear.predictors <- eta
    mu <- as.vector(family$linkinv(eta))
    fit$fitted.values <- mu
    fit$residuals <- y - mu
    fit$family <- family
    fit$y <- as.vector(y)
    fit$id <- as.vector(id)
    fit$max.id <- z$mc
    z$wcor <- matrix(z$wcor, ncol = maxclsz)
    fit$working.correlation <- z$wcor
    fit$scale <- z$sc
    fit$robust.variance <- z$rv
    fit$naive.variance <- z$nv
    fit$xnames <- xnames
    fit$error <- z$err
    dimnames(fit$robust.variance) <- list(xnames, xnames)
    dimnames(fit$naive.variance) <- list(xnames, xnames)
    fit
}

lsmean2.lm <-
  function (object, data = eval(object$call$data), factors = names(predictors[predictors]), 
    expr = formula(object), contrast = object$contrasts, effects = FALSE, 
    se.fit = TRUE, adjust.covar = TRUE, pdiff = FALSE, reorder = FALSE, 
    lsd = FALSE, level = 0.05, rdf = df.resid(object), coef = coefficients(object), 
    cov = std.dev(object)^2 * cov.coef(object), ...) 
{
    expr.labels <- attr(terms(expr), "term.labels")
    predictors <- unlist(lapply(data, is.factor))[all.names(expr, 
        unique = TRUE, functions = FALSE)[-1]]
    predictors <- predictors[!is.na(charmatch(names(predictors), 
        expr.labels))]
    covars <- names(predictors[!predictors])
    if (!length(covars)) 
        covars <- NULL
    aux.factors <- predictors[predictors]
    if (length(aux.factors) & !is.null(factors)) {
        pm <- pmatch(unlist(factors), names(aux.factors))
        pm <- pm[!is.na(pm)]
    }
    else pm <- numeric(0)
    for (i in names(contrast)) if (pmatch("contr", as.character(contrast[[i]][1]), 
        nomatch = 0)) 
        contrast[[i]] <- contrasts(data[[i]])
    if (length(pm) == 0) {
        factors <- NULL
        aux.factors <- names(aux.factors)
    }
    else {
        factors <- names(aux.factors[pm])
        aux.factors <- names(aux.factors[-pm])
        if (effects) {
            for (i in factors) if (any(abs(apply(contrast[[i]], 
                2, sum) > 1e-12))) 
                stop(paste("effects=TRUE not implemented for factors", 
                  "without sum-to-zero contrasts (", i, ")"))
        }
    }
    tmp <- length(aux.factors)
    cont.factors <- rep(FALSE, tmp)
    if (tmp) 
        for (i in seq(tmp)) cont.factors[i] <- any(apply(contrasts(data[[aux.factors[i]]]), 
            2, sum))
    cont.factors <- aux.factors[cont.factors]
    if (length(cont.factors)) 
        if (!is.null(aux.factors)) 
            aux.factors <- aux.factors[-pmatch(aux.factors, cont.factors, 
                nomatch = 0)]
    fac <- attr(terms(expr), "factors")
    tmp <- c(factors, covars, cont.factors)
    term.factors <- fac[tmp, ]
    if (length(tmp) > 1) 
        term.factors <- apply(term.factors, 2, any)
    not.ignore <- 1 - fac[aux.factors, ]
    if (length(aux.factors) > 1) 
        not.ignore <- apply(not.ignore, 2, all)
    if (length(aux.factors) & length(not.ignore)) 
        term.factors <- term.factors & not.ignore
    term.factors <- expr.labels[term.factors]
    if (is.null(factors)) {
        newdata <- data[1, ]
        int.factors <- "mean"
        row.names(newdata) <- int.factors
    }
    else {
        int.factors <- interaction(data[, factors], drop = TRUE)
        design <- !duplicated(int.factors)
        newdata <- data[design, ]
    }
    if (adjust.covar | is.null(factors)) {
        for (i in covars) newdata[[i]] <- rep(mean(data[[i]], 
            na.rm = TRUE), nrow(newdata))
    }
    else {
        for (i in covars) newdata[[i]] <- c(tapply(data[[i]], 
            int.factors, mean))
    }
    if (!is.null(factors)) {
        for (i in rev(factors)) newdata <- newdata[order(newdata[[i]]), 
            ]
        row.names(newdata) <- int.factors <- interaction(newdata[, 
            factors], drop = TRUE)
    }
    if (!is.null(aux.factors)) {
        for (i in aux.factors) levels(newdata[[i]]) <- levels(data[[i]])
    }
    if (!is.null(cont.factors) & length(cont.factors)) {
        factornames <- list()
        for (i in names(data)) factornames[[i]] <- levels(data[[i]])
        cont.levels <- expand.grid(get.list(factornames, cont.factors))
        cont.n <- nrow(cont.levels)
        newdata <- newdata[rep(int.factors, cont.n), ]
        tmp <- rep(seq(len = cont.n), rep(length(int.factors), 
            cont.n))
        newdata[, cont.factors] <- cont.levels[tmp, ]
    }
    coef <- coef[!is.na(coef)]
    mat <- model.matrix.default(expr, newdata, contrast)
    pos <- attr(mat, "assign")
    if (!is.list(pos)) {
        posnames <- attr(terms(formula(object)), "term.labels")
        posnum <- pos
        pos <- list()
        pos[["(Intercept)"]] <- 1
        spos <- seq(length(posnum))
        for (i in seq(length(posnames))) pos[[posnames[i]]] <- spos[i == 
            posnum]
    }
    index <- if (!effects) 
        pos[["(Intercept)"]]
    for (i in term.factors) index <- c(index, pos[[i]])
print(term.factors)
                                        #print(pos)

    print("names"); print(names(coef))
    print("dimnames"); print(dimnames(mat)[[2]])

    is.coef <- match(dimnames(mat)[[2]], names(coef), nomatch = 0)

    print(is.coef)
    print(index)
    mindex <- index[is.coef[index] > 0]
    index <- is.coef[index]
    print(index)
    mat <- t(as.matrix(mat[, mindex]))
    if (!is.null(cont.factors) & length(cont.factors)) {
        tmp <- array(mat, c(nrow(mat), ncol(mat)/cont.n, cont.n))
        dimnames(tmp) <- list(dimnames(mat)[[1]], NULL, NULL)
        mat <- apply(tmp, c(1, 2), mean)
    }
    mat <- as.matrix(mat)
    if (nrow(mat) == 1) 
        mat <- t(mat)
    #print(index)
    #print(coef[index])
    #print(mat)
    pred <- coef[index] * mat
    if (length(index) == 1) 
        pred <- unlist(pred)
    else pred <- apply(pred, 2, sum, na.rm = TRUE)
    if (se.fit) {
        if (length(index) == 1) 
            se <- sqrt(cov[index, index]) * abs(mat)
        else se <- sqrt(apply(mat * (cov[index, index] %*% mat), 
            2, sum))
    }
    if (is.null(factors) & is.null(covars)) 
        newdata <- data.frame(pred = matrix(pred, 1, 1), row.names = int.factors)
    else {
        newdata <- as.data.frame(newdata[int.factors, c(factors, 
            covars)])
        dimnames(newdata) <- list(int.factors, c(factors, covars))
        names(pred) <- as.character(int.factors)
        newdata$pred <- pred
    }
    if (se.fit) {
        if (is.null(factors) & is.null(covars)) 
            se <- matrix(se, 1, 1)
        newdata$se <- se
        names(newdata$se) <- row.names(newdata)
    }
    n <- length(pred)
    if (pdiff & n > 2) {
        tmprc <- lower.tri(diag(n))
        indrc <- col(diag(n))[tmprc]
        ni <- length(indrc)
        order.pred <- order(-pred)
        names.pred <- names(pred[order.pred])
        pdiff <- data.frame(col = ordered(names.pred[indrc], 
            names.pred))
        difmat <- matrix(0, ni, n, dimnames = list(NULL, names(pred)))
        difmat[seq(ni) + ni * (indrc - 1)] <- 1
        indrc <- row(diag(n))[tmprc]
        difmat[seq(ni) + ni * (indrc - 1)] <- -1
        pdiff$row <- ordered(names.pred[indrc], names.pred)
        mat <- mat[, order.pred] %*% t(difmat)
        difpred <- apply(coef[index] * mat, 2, sum, na.rm = TRUE)
        difse <- apply(mat * (cov[index, index] %*% mat), 2, 
            sum)
        pdiff$pvalue <- 2 * pt(-abs(difpred/sqrt(difse)), rdf)
        plet <- pletter.ravel(pdiff, level = level)
        newdata$pdiff <- plet[names(pred)]
        if (lsd) 
            newdata$lsd <- rep(qt(1 - level/2, rdf) * sqrt(mean(difse)), 
                length(pred))
    }
    if (reorder) 
        newdata <- newdata[order.pred, ]
    newdata
}


residuals.gee <- function (object, type = c("pearson", "working", "response"), ...) 
{
  type <- match.arg(type)
  y <- object$y
  r <- object$residuals
  mu <- object$fitted.values
  wts <- rep(1,length(y)) ##object$prior.weights
  res <- switch(type,
                ##deviance = if (object$df.res > 0) {
                ##  d.res <- sqrt(pmax((object$family$dev.resids)(y, mu, 
                ##                                                wts), 0))
                ##  ifelse(y > mu, d.res, -d.res)
                ##}
                ##else rep.int(0, length(mu)),

                pearson = (y - mu) * sqrt(wts)/sqrt(object$family$variance(mu)), 
                working = r,
                response = y - mu,
                partial = r)
  
  if (!is.null(object$na.action)) 
    res <- naresid(object$na.action, res)
  ##if (type == "partial") 
  ##  res <- res + predict(object, type = "terms")
  res
}


model.matrix.gee <- function(fit){
 dat <- get(as.character(fit$call$data))
 mm <- model.matrix(formula(fit), dat)
 return(mm)
}
