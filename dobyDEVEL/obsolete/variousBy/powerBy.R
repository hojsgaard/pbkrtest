
powerBy <- function(formula = ~., data = parent.frame()){

  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m[[1]] <- as.name("model.frame")
  mf <- eval(m, parent.frame())
  y.names     <- dimnames(mf[,1])[[2]]

  sby <- summaryBy(formula, data=data,FUN=c(mean,var))
  
  l <- lapply(y.names, function(a)
              c(paste("mean.",a,sep=''),paste("var.",a,sep=''))) 
  
  var.index <- match(unlist(l), names(sby))
  for (j in var.index){
    sby[,j] <- log(sby[,j])
  }

  l2 <- lapply(y.names, function(a)
              c(a,paste("mean.",a,sep=''),paste("var.",a,sep=''))) 

  lapply(l2, function(a)
         {
           d <- sby[,match(a[-1], names(sby))]
           plot(d[,1],d[,2],
                xlab=paste("log(",a[2],")",sep=''),
                ylab=paste("log(",a[3],")",sep=''))
           lm.fit <- lm(d[,2]~d[,1])
           abline(lm.fit)
           s <- summary(lm.fit)
           se <- sqrt(s$cov[2,2])*s$sigma
           title(paste(a[1], "- slope=", round(lm.fit$coef[2],2),
                       "(", round(se,2), ")")) 
         })
  return(sby)
}

